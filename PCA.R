applyPCA<-function(df = NULL){
  
  
  library("ggplot2")
  library("reshape2")
  
  inputData <- df
  
  library("factoextra")
  # Extract active variables/individuals for PCA
  inputData.active <- inputData[, c(1:28, 30:39)]
  head(inputData.active[, 1:6])
  
  
  library("FactoMineR")
  inputData.pca <- PCA(inputData.active, graph = FALSE)
  print(inputData.pca)
  
  #Variances of the principal components
  #The proportion of variation retained by the principal components (PCs) can be extracted as follow :
  eigenvalues <- inputData.pca$eig
  print("Eigenvalues:")
  head(eigenvalues[, 1:2])
  #fviz_screeplot(inputData.pca, ncp=14) + theme(text = element_text(size=16)) + xlab("Dimensões (PCs)") +  ylab("Variabilidade dos Dados (%)")#pt-br
  fviz_screeplot(inputData.pca, ncp=14) + theme(text = element_text(size=16))
  #Plot the correlations/loadings of the variables with the components
  #The correlation between a variable and a PC is called loading. 
  #The variables can be plotted as points in the component space using their loadings as coordinates.
  # Coordinates of variables
  print(inputData.pca$var$coord)
  fviz_pca_var(inputData.pca)
  
  #Cos2 : quality of the representation for variables on the factor map
  #The squared loadings for variables are called cos2 ( = cor * cor = coord * coord).
  print(inputData.pca$var$cos2)
  
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
  
  
  fviz_contrib(inputData.pca, choice = "var", axes = 1:3, fill = "lightgray", color = "black") +
    theme_minimal() + theme(text = element_text(size=16),axis.text.x = element_text(angle=45))+
    xlab("Variáveis") +  ylab("Contribuição (%)")
  
  
  # Visualize
  # Use habillage to specify groups for coloring
  fviz_pca_ind(inputData.pca,
               label = "none", # hide individual labels
               habillage = df$incident, # color by groups
               palette = c("#00AFBB", "#E7B800", "#FC4E07"),
               addEllipses = TRUE # Concentration ellipses
  )
  
  
  # Total contribution on PC1, PC2, and PC3
  fviz_contrib(inputData.pca, choice = "var", axes = 1:3)
  
  # Control variable colors using their contributions
  fviz_pca_var(inputData.pca, col.var="contrib")
  print(inputData.pca$var$contrib[,1:2] > 10)
  print(inputData.pca$var$contrib[,1] + inputData.pca$var$contrib[,2] > 9)
  # Change the gradient color
  fviz_pca_var(inputData.pca, col.var="contrib") + scale_color_gradient2(low="white", mid="blue", 
                                                                         high="red", midpoint=50) + theme_minimal()
  
  t = fviz_contrib(inputData.pca, choice = "var", axes = 1:2)
  #removed after PCA
  # #inputData.active$OBD_Intake_Air_Temp_C <- NULL
  # inputData.active$`Intake Air Temp`<- NULL
  # #inputData.active$OBD_Engine_Coolant_Temp_C <- NULL
  # inputData.active$`Engine Temp`<- NULL
  # #inputData.active$OBD_Adapter_Voltage <- NULL
  # inputData.active$`Adapter Voltage` <- NULL
  # #inputData.active$Acceleration_kmhs <- NULL
  # inputData.active$Acceleration <- NULL
  # #inputData.active$OBD_CO2_gkm_Average <- NULL
  # inputData.active$`CO2 Av` <- NULL
  # 
  # #-----------------------------------------------------------------------------------------------------
  # #----------------------------------------------Correlation--------------------------------------------
  # #-----------------------------------------------------------------------------------------------------
  
  # correlation Matrix
  correlationMatrix = cor(inputData.active)
  # Standard Desviation of correlation Matrix
  sd(correlationMatrix)
  
  # colnames(filtredDay) <- c("KPL Av", "KPL Av Trip","Intake Air Temp","Barometer","Altitude","Speed","GPS Speed",
  #                          "Trip Dist","Engine Temp","RPM","Adapter Voltage","KPL Inst","Fuel Flow",
  #                          "CO2 Av","CO2 Inst","Acceleration","Air Drag Force")
  # colnames(filtredDay) <- c("KPL Av", "KPL Av Trip","Intake Air Temp","Barometer","Altitude","Speed","GPS Speed",
  #                           "Trip Dist","Engine Temp","RPM","Adapter Voltage","KPL Inst","Fuel Flow",
  #                           "CO2 Av","CO2 Inst","Acceleration")
  
  #graph of correlations among each variable / "Correlação entre dados aferidos do veículo"
  # corrplot(cor(filtredDay[sapply(filtredDay, is.numeric)]), diag = FALSE, title="Correlation",
  #          tl.srt = 90,mar = c(2, 1, 0, 2),method="ellipse",order ="FPC",addrect=3, type="upper")#, type="upper"
  library(corrplot)
  corrplot.mixed(cor(inputData.active), order = "FPC", upper = "ellipse",mar = c(2, 0, 1, 0),tl.pos="lt", tl.srt=60)
  
  par(mfrow=c(1,1), mar=c(0, 0, 1, 4), oma=c(0, 0, 1, 4))
  corrplot(cor(inputData.active), order = "FPC",tl.srt=60, diag = FALSE, type = "upper",method="number")
  
}

df <- read.csv('/home/igor/Área de Trabalho/IC/twitter/Main/tweets_features/2018-10-18.csv')
# df$incident <- 1
# df[df$incident_type == 'NOT_INCIDENT',]$incident <- 0
# df[df$incident_type == 'NULL',]$incident <- 'NULL'
table(df$incident)

applyPCA(df)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}