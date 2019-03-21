##################################################
## Project: T-Incident
## Script purpose: features extraction
## Date: 25/10/2018
## Author: Igor and Paulo H. L. Rettore
##################################################

# STEP: 0 -----------------------------------------------------------------
## load paths, input data, and libraries

lapply(c('dplyr', 'ggplot2', 'rjson','ggmap','tm','tidytext','wordcloud','RColorBrewer','reshape2', 'maptools' ,
         "lubridate","igraph", "rgdal","scales","plyr","parallel","geosphere",
         "bigmemory","syuzhet","tokenizers","SnowballC"), 
       library, character.only = TRUE)  #load libraries

#load data
PATH2SAVE <- 'data/classification/20_NOT_INCIDENT/'
FILENAME <- 'twitter_incident(Radius0.1).csv'
data  <- read.csv(paste('/home/igor/Área de Trabalho/IC/Social Media/t-incident/data/classification/20_NOT_INCIDENT',FILENAME, sep = ""), sep = ",", stringsAsFactors = FALSE)
corpus <- Corpus(VectorSource(data$text)) #create a corpus (list of documents for) of all my tweets


# STEP 1: filtering data -------------------------------------------------- 

removeURL <- function(text) {
  x <- gsub(" ?(f|ht)(tp)s?(://)(\\S*)[./](\\S*)", "", text)
  return (x)
}

removeSpecialChars  <-function(text) {
  x <- gsub('[])(;:#%$^*\\~{}[&+=@/"`|<>_]+', "", text)
  return(x)
}

removepont <- function(text){x <-  gsub('[[:punct:] ]+',' ',text)
  return (x)
}

filtering_text  <- function(x,remove_context_words=NULL) {  #function to filter text

  corpus = tm_map(corpus, tolower) #lowercase
  
  corpus <- tm_map(corpus, removeURL)
  corpus = tm_map(corpus, removeNumbers)  #remove numbers
  corpus = tm_map(corpus, removepont)  #remove pontuations
  corpus  = tm_map(corpus, removeWords, c('the', 'and', stopwords('english')))  #remove stopwords
  corpus <- tm_map(corpus,removeSpecialChars)
  corpus = tm_map(corpus, stripWhitespace)  #remove stripWhitespace
  corpus <- tm_map(corpus, stemDocument,language = "english") #radiciação (stem)
  corpus  = tm_map(corpus, removeWords, remove_context_words)  #remove stopwords
  
  return (corpus)
}

# STEP 2: Select terms ----------------------------------------------------

corpus_tfidf <- filtering_text(corpus)

data_dtm = TermDocumentMatrix(corpus_tfidf) #crio uma matrix de documento
#inspect(data_dtm)

data_dtm_sparse <- removeSparseTerms(data_dtm, 0.98)  # level of sparcity of matrix which is inverse of matrix density 
inspect(data_dtm_sparse)

data_dtm_sparse$dimnames$Terms

context_words <- c("work","love","new","york","look","one","day" ,"just" ,"see","get","time",
                   "great","click","hire","careerarc", "can", "latest","nyc","newyork","photo","right","min","citi","job","brooklyn","manhattan","bronx","amp")   

corpus_tfidf <- filtering_text(corpus,context_words)

data_dtm = DocumentTermMatrix(corpus_tfidf) #crio uma matrix de documento
inspect(data_dtm)
data_dtm_sparse <- removeSparseTerms(data_dtm, 0.98)  # level of sparcity of matrix which is inverse of matrix density 
inspect(data_dtm_sparse)

data_dtm_sparse$dimnames$Terms


# STEP 3: Term Frequancy (TF) ---------------------------------------------

# plot(findFreqTerms(data_dtm, 1000)) #achando a frequência de termos do texto
freq_plot <- (colSums(as.matrix(data_dtm_sparse)))
freq_plot <- sort(freq_plot, decreasing = TRUE)
barplot(freq_plot) #plot the frequency of
#sum(freq_plot[1:31])/ sum(freq_plot)  #taxa de total de palavras
#creatinfg a wordcloud


freq = data.frame (sort(colSums(as.matrix(data_dtm_sparse)), decreasing =  TRUE))  #transformo a matrix de frequência em um dataframe com index igual a palavra e coluna com freq
wordcloud(rownames(freq), freq[,1], max.words =  50, colors = brewer.pal(1, 'Dark2'))
#hist(freq$sort.colSums.as.matrix.data_dtm_sparse....decreasing...TRUE.)


df <- as.matrix(data_dtm_sparse)
df <- as.data.frame(df)

# STEP 4: Term Frequancy Inverse Doc Freq (TF-IDF) ------------------------

library(dplyr)
library(janeaustenr)
library(tidytext)

corpus_tfidf <- filtering_text(corpus)  #visualizar o corpus

corpus_tfidf_df <- data.frame(text = sapply(corpus_tfidf, as.character), stringsAsFactors = FALSE)

corpus_tfidf_df$id_str <-data$id_str
# corpus$text <- removeSpecialChars(corpus$text)
# corpus$text <- removeURL(corpus$text)
# corpus$text <- removepont(corpus$text)


corpus_tfidf_df$id <-rep(1:20,length.out=nrow())


tweet_words <- corpus_tfidf_df %>%
  unnest_tokens(word, text) %>%
  count(id,word, sort = TRUE)

tweet_words[tweet_words$id_str == data$id_str[1],]

total_words <- tweet_words %>% group_by(word) %>% summarize(total = sum(n))  #counting the number of words
tweet_words <- left_join(tweet_words, total_words)




library(ggplot2)

tweet_words <- tweet_words %>%
  bind_tf_idf(word,id, n)

tweet_words <- tweet_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))

tweet_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  top_n(10) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()


tweet_words$word[1:10]


freq = data.frame(sort(colSums(as.matrix(data_dtm__tfidf_sparse)), decreasing =  TRUE))
wordcloud(rownames(data_dtm__tfidf_sparse), freq[,1], max.words=100, colors=brewer.pal(1, "Dark2")) #wordcloud do tf-idf


# STEP 5: Add emotion -----------------------------------------------------


cl <- makeCluster(4) # create clusters to process in parallel # or detect_cores() - 1

corpus_Text <- data.frame(text = sapply(corpus, as.character), stringsAsFactors = FALSE)  #convert corpus in a dataframe

text_emotions <- get_nrc_sentiment(corpus_Text[,1], cl = cl)  #add sentiment to each tweet

stopCluster(cl) #stop the clusters

df <- cbind(df,text_emotions) #bind df and sentiment

df['incident_id'] <- data$incident_id #add incident id
df['incident_type'] <- data$incident_type #add incident type
df['event'] <- data$event #add incident label
df['twitter_id'] <- data$id_str #add tweet id



# STEP 6: Feature Importance (PCA) ----------------------------------------


applyPCA<-function(df = NULL){


  library("ggplot2")
  library("reshape2")

  inputData <- df

  library("factoextra")
  # Extract active variables/individuals for PCA
  inputData.active <- inputData[, c(1:23)]
  head(inputData.active[, 1:6])


  library("FactoMineR")
  inputData.pca <- PCA(inputData.active, graph = FALSE)
  #print(inputData.pca)

  #Variances of the principal components
  #The proportion of variation retained by the principal components (PCs) can be extracted as follow :
  eigenvalues <- inputData.pca$eig
  print("Eigenvalues:")
  #head(eigenvalues[, 1:2])
  #fviz_screeplot(inputData.pca, ncp=14) + theme(text = element_text(size=16)) + xlab("Dimensões (PCs)") +  ylab("Variabilidade dos Dados (%)")#pt-br
  fviz_screeplot(inputData.pca, ncp=14) + theme(text = element_text(size=16))
  #Plot the correlations/loadings of the variables with the components
  #The correlation between a variable and a PC is called loading.
  #The variables can be plotted as points in the component space using their loadings as coordinates.
  # Coordinates of variables
  #var it is the list of matrices containing all the results for the active variables
  #print(inputData.pca$var$coord)
  fviz_pca_var(inputData.pca)

  #Cos2 : quality of the representation for variables on the factor map
  #The squared loadings for variables are called cos2 ( = cor * cor = coord * coord).
  #print(inputData.pca$var$cos2)

  fviz_pca_var(inputData.pca, col.var="cos2") +  scale_color_gradient2(low="white", mid="blue", high="red", midpoint=0.5)+ theme_minimal()
  print(inputData.pca$var$contrib)

  # Contributions of variables on PC1
  fviz_contrib(inputData.pca, choice = "var", axes = 1) + xlab("Variáveis") +  ylab("Contribuição (%)")
  # Contributions of variables on PC2
  fviz_contrib(inputData.pca, choice = "var", axes = 2)+ xlab("Variáveis") +  ylab("Contribuição (%)")
  # Contributions of variables on PC2
  fviz_contrib(inputData.pca, choice = "var", axes = 3)+ xlab("Variáveis") +  ylab("Contribuição (%)")
  # Total contribution on PC1 and PC2
  fviz_contrib(inputData.pca, choice = "var", axes = 1:2) + theme_minimal(base_size = 5, base_family = "")+
    theme(text = element_text(size=16),axis.text.x = element_blank())+
    #xlab("Variáveis") +  ylab("Contribuição (%)")#pt-br
    xlab("Variables") +
    ylim(0,17) +
    geom_text(aes(label=name), position=position_dodge(width=1),hjust=0.1, vjust=0,angle = 90,size=5)


  # fviz_contrib(inputData.pca, choice = "var", axes = 1:3, fill = "lightgray", color = "black") +
  #   theme_minimal() + theme(text = element_text(size=16),axis.text.x = element_text(angle=45))+
  #   xlab("Variáveis") +  ylab("Contribuição (%)")


  # Visualize
  # # Use habillage to specify groups for coloring
  # fviz_pca_ind(inputData.pca,
  #              label = "none", # hide individual labels
  #              habillage = df$incident, # color by groups
  #              palette = c("#00AFBB", "#E7B800", "#FC4E07"),
  #              addEllipses = TRUE # Concentration ellipses
  # )


  # Total contribution on PC1, PC2, and PC3
  # fviz_contrib(inputData.pca, choice = "var", axes = 1:3)

  # Control variable colors using their contributions
  fviz_pca_var(inputData.pca, col.var="contrib")
  print(inputData.pca$var$contrib[,1:2] > 5)
  #print(inputData.pca$var$contrib[,1] + inputData.pca$var$contrib[,2] > 9)
  # Change the gradient color
  fviz_pca_var(inputData.pca, col.var="contrib") + scale_color_gradient2(low="white", mid="blue",
                                                                         high="red", midpoint=50) + theme_minimal()

  t = fviz_contrib(inputData.pca, choice = "var", axes = 1:2)

  #transform the t data in dataframe

  #selected the features above the mean
  most_important_feature_names = data.frame(t[["data"]], stringsAsFactors = FALSE)
  most_important_feature_names$name <- as.character(most_important_feature_names$name)
  most_important_feature_names <- most_important_feature_names[most_important_feature_names$contrib > mean(most_important_feature_names$contrib),]$name
  #selecting the the variables that have a major part of contribution

  df_to_save <- df[,c(most_important_feature_names,'incident_id','incident_type','event','twitter_id')]

  path_name <-  paste(PATH2SAVE, paste(gsub(".csv","",FILENAME), "_feature_extracted_PCA.csv", sep = ''), sep = '') 
  write.csv(df_to_save,path_name,row.names = FALSE)
  #return(df_with_features_selected)


}
#Return dataframe with have the great contribuition in the description of the data (higher variance)


applyPCA(df)

# STEP 7: Saving the file -------------------------------------------------
# path_name <- paste(PATH2SAVE, paste(gsub(".csv","",FILENAME), "_feature_extracted_PCA.csv", sep = ''), sep = '') 
# write.csv(df,path_name,row.names = FALSE) #escrevo no arquivo





