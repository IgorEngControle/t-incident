lapply(c('ggplot2','plotly', 'ggmap','RCurl', 'RColorBrewer','reshape2','readr','purrr'), library, character.only = TRUE) #load libraries



files_names <- dir('Classification_Results(20_NOT_INCIDENT)/', pattern = "*.csv") # get traffic file names
# read in all the traffic files, appending the path before the filename 
# require(purrr) for map(), reduce() ; require(readr)  # for read_csv()
data <- files_names %>%
  map(function(x) read_csv(file.path("Classification_Results(20_NOT_INCIDENT)/", x))) %>%  
  reduce(rbind)

# ##Metric Analysis
# # data0 <- data[data$class == 'weighted avg',c('radius', 'f1_score')]
# # data0$radius <-as.factor(data0$radius)
# # factor(data0$radius, levels = c(1:6))
# # plot(x = data0$radius, y = data0$f1_score, xlim = c(0,5))
# # f1 Score Analysis -------------------------------------------------------
# # Basic line plot with points
# ggplot(data=data, aes(x=data$radius, y=data$f1_score, group=data$class, color = data$class)) +
#   geom_line()+
#   geom_point()+
#   labs(title="F1 Score per per Radius with diferent Classes ", x = 'Radius',y = "F1 Score")+
#   scale_color_brewer(palette="Paired")+
#   theme_minimal()
# 
# aes(x=dose, y=len, group=supp, color=supp)
# 
# # F1 Score Analisys -------------------------------------------------------
# 
# data <-data[data$class == 'weighted avg',]
# myplot <- ggplot(data=data, aes(x=data$radius, y=data$f1_score, group=data$class, color = data$class)) + 
#   geom_line(aes(linetype=data$class), size=1) +     # Set linetype by sex
#   geom_point(size=3, fill="white") +         # Use larger points, fill with white
#   expand_limits(y=0) +                       # Set y range to include 0
#   scale_colour_hue(name="Class",      # Set legend title
#                    l=30)  +                  # Use darker colors (lightness=30)
#   scale_shape_manual(name="Class",
#                      values=c(22,21)) +      # Use points with a fill color
#   scale_linetype_discrete(name="Class") +
#   xlab("Radius") + ylab("F1 Score") + # Set axis labels
#   ggtitle('F1 Score by Radius using BBC Kernel') +     # Set title
#   theme_bw() +
#   theme(legend.position=c(.8, .2))      # Position legend inside
# 
# 
# # Precision ---------------------------------------------------------------
# 
# ggplot(data=data, aes(x=data$radius, y=data$precision, group=data$class, color = data$class)) + 
#   geom_line(aes(linetype=data$class), size=1) +     # Set linetype by sex
#   geom_point(size=3, fill="white") +         # Use larger points, fill with white
#   expand_limits(y=0) +                       # Set y range to include 0
#   scale_colour_hue(name="Class",      # Set legend title
#                    l=30)  +                  # Use darker colors (lightness=30)
#   scale_shape_manual(name="Class",
#                      values=c(22,21)) +      # Use points with a fill color
#   scale_linetype_discrete(name="Class") +
#   xlab("Radius") + ylab("Precision ") + # Set axis labels
#   ggtitle('Precision by Radius using BBC Kernel') +     # Set title
#   theme_bw() +
#   theme(legend.position=c(.8, .2))           # Position legend inside
# # This must go after theme_bw
# 
# 
# 
# # Recall ------------------------------------------------------------------
# 
# data <- data[data$class == 'weighted avg',]
# 
# ggplot(data=data, aes(x=data$radius, y=data$recall, group=data$class, color = data$class)) + 
#   geom_line(aes(linetype=data$class), size=1) +     # Set linetype by sex
#   geom_point(size=3, fill="white") +         # Use larger points, fill with white
#   expand_limits(y=0) +                       # Set y range to include 0
#   scale_colour_hue(name="Class",      # Set legend title
#                    l=30)  +                  # Use darker colors (lightness=30)
#   scale_shape_manual(name="Class",
#                      values=c(22,21)) +      # Use points with a fill color
#   scale_linetype_discrete(name="Class") +
#   xlab("Radius") + ylab("FRecall") + # Set axis labels
#   ggtitle('Recall by Radius using BBC Kernel') +     # Set title
#   theme_bw() +
#   theme(legend.position=c(.8, .2))           # Position legend inside
# # This must go after theme_bw
# 
# ggplot(data, aes(x = data$radius, group=data$class, color = data$class)) + 
#   geom_line(aes(y = data$f1_score), colour="blue") + 
#   geom_line(aes(y = data$recall), colour = "grey") + 
#   geom_line(aes(y = data$precision), colour = "red") + 
#   ylab(label="Score") + 
#   xlab("Radius")
# 
# 

# Melt ------------------------------------------------------------------
data <-data[data$class == 'weighted avg',]
data_training <- data[,c('radius','kernel','cv_F1','cv_Recall','cv_Precision')] #select the most importants metrics
data_test <- data[,c('radius','kernel','f1_score','recall','precision')] #select the most importants metrics
data_training$partition <- 'Training'
data_test$partition <- 'Test'
colnames(data_training) <- c('Radius','Kernel', 'F1 Score','Recall','Precision','Partition') 
colnames(data_test) <- c('Radius','Kernel', 'F1 Score','Recall','Precision','Partition') 


md <- melt(rbind(data_test,data_training), id =c('Radius','Kernel','Partition'))  #transposed


p <- ggplot(data=md, aes(x=Radius, y=value, color=variable, group = variable)) + 
  geom_line(aes(linetype=variable), size=1) +     # Set linetype by sex
  geom_point(size=3, fill="white") +         # Use larger points, fill with white
  expand_limits(y=0) +                       # Set y range to include 0
  # scale_colour_hue(name="Metrics",      # Set legend title
  #                  l=30)  +                  # Use darker colors (lightness=30)
  # scale_shape_manual(name="Metrics",
  #                    values=c(22,21)) +      # Use points with a fill color
  # scale_linetype_discrete(name="Metrics") +
  xlab("Radius") + ylab("Value") + # Set axis labels
  theme_bw() +
  facet_grid(Kernel~Partition)         # Position legend inside
# This must go after theme_bw
  p <- p + theme(legend.position="top") 
  plot(p)




