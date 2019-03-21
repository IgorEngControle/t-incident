# Function do reduce the number of features using PCA 
applyPCA<-function(df = NULL){
  
  inputData <-dataCar1Gear

  library("ggplot2")
  library("factoextra","reshape2")
  
  inputData <- df
  #inputData <- applyLimit(data1,0,-1)
  #inputData <- applyGear(inputData,1,1)
  
  # inputData$KPL_Instant = inputData$OBD_Speed_Km/(inputData$OBD_Fuel_Flow_CCmin*6/100)
  # inputData$KPL_Instant[is.nan(inputData$KPL_Instant)] = 0
  # inputData$KPL_Instant[is.infinite(inputData$KPL_Instant)] = 0
  # inputData$KPL_Instant <- round(inputData$KPL_Instant, digits = 2)
  
  #Removing 26 features=========================================  
  inputData$Context <- NULL
  
  
  #Smartphone
  inputData$Device_Barometer_M <- NULL
  inputData$GPS_Speed_Km <- NULL
  inputData$GPS_Accuracy_M <- NULL
  inputData$GPS_Altitude_M <- NULL
  inputData$GPS_Bearing <- NULL
  inputData$GPS_HDOP <- NULL
  inputData$GPS_Speed_Ms <- NULL
  inputData$GPS_Lat <- NULL
  inputData$GPS_Long <- NULL
  inputData$GPS_Time <- NULL
  inputData$Gx <- NULL
  inputData$Gy <- NULL
  inputData$Gz <- NULL
  inputData$G_Calibrated <- NULL
  inputData$Device_Cost_Km_Trip <- NULL
  inputData$Device_Cost_Km_Inst <- NULL
  inputData$Device_Time <- NULL
  inputData$Device_Fuel_Remaining <- NULL
  inputData$Device_Trip_Dist_Km <- NULL
  
  #OBD
  inputData$OBD_Ambient_Air_Temp_C <- NULL
  inputData$OBD_Fuel_Level <- NULL
  inputData$OBD_KPL_Instant <- NULL
  inputData$OBD_KPL_Average <- NULL
  inputData$OBD_Trip_KPL_Average <- NULL
  
  #virtual sensor
  inputData$Air_Drag_Force <- NULL
  inputData$Reaction_Time <- NULL
  
  
  colnames(inputData) <- c("Car","Person","Trip", "Intake Air Temp","Engine Load","Speed","Engine Temp","RPM","Adapter Voltage",
                           "Fuel Flow","CO2 Av","CO2 Inst","Pedal","Acceleration","Speed/RPM","KPL Inst","Gear")
  
  #pt-br
  #colnames(inputData) <- c("Car","Person","Trip", "Temp de Entrada do Ar","Torque","Velocidade","Temp Motor","RPM","Voltage Bateria",
  #                         "Fluxo de Comb","Média CO2","CO2","Pedal","Aceleração","Velocidade/RPM","KPL","Marcha")
  
  
  
  
  #=========================================   
  
  # #http://www.sthda.com/english/wiki/principal-component-analysis-how-to-reveal-the-most-important-variables-in-your-data-r-software-and-data-mining
  #devtools::install_github("kassambara/factoextra")
  
  library("factoextra")
  # Extract active variables/individuals for PCA
  inputData.active <- inputData[, 4:ncol(inputData)]
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
  # fviz_pca_biplot(inputData.pca, col.var="cos2") +  scale_color_gradient2(low="white", mid="blue", high="red", midpoint=0.5)
  # + theme_minimal()
  
  #Contributions of the variables to the principal components
  #The contribution of variables can be extracted as follow :
  #The larger the value of the contribution, the more the variable contributes to the component.
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
  
 
  fviz_contrib(inputData.pca, choice = "var", axes = 1:2, fill = "lightgray", color = "black") +
    theme_minimal() + theme(text = element_text(size=16),axis.text.x = element_text(angle=45))+
    xlab("Variáveis") +  ylab("Contribuição (%)")
  
    
  # Visualize
  # Use habillage to specify groups for coloring
  fviz_pca_ind(inputData.pca,
               label = "none", # hide individual labels
               habillage = iris$Species, # color by groups
               palette = c("#00AFBB", "#E7B800", "#FC4E07"),
               addEllipses = TRUE # Concentration ellipses
  )
  
 
  # Total contribution on PC1, PC2, and PC3
  fviz_contrib(inputData.pca, choice = "var", axes = 1:3)
  
  # Control variable colors using their contributions
  fviz_pca_var(inputData.pca, col.var="contrib")
  print(inputData.pca$var$contrib[,1:2] > 10)
  print(inputData.pca$var$contrib[,1] + inputData.pca$var$contrib[,2] > 10)
  # Change the gradient color
  fviz_pca_var(inputData.pca, col.var="contrib") + scale_color_gradient2(low="white", mid="blue", 
                                                                         high="red", midpoint=50) + theme_minimal()
  
  t = fviz_contrib(inputData.pca, choice = "var", axes = 1:2)
  #removed after PCA
  #inputData.active$OBD_Intake_Air_Temp_C <- NULL
  inputData.active$`Intake Air Temp`<- NULL
  #inputData.active$OBD_Engine_Coolant_Temp_C <- NULL
  inputData.active$`Engine Temp`<- NULL
  #inputData.active$OBD_Adapter_Voltage <- NULL
  inputData.active$`Adapter Voltage` <- NULL
  #inputData.active$Acceleration_kmhs <- NULL
  inputData.active$Acceleration <- NULL
  #inputData.active$OBD_CO2_gkm_Average <- NULL
  inputData.active$`CO2 Av` <- NULL
  
  
  
  
  ###relevant features
  #inputData.active$OBD_CO2_gkm_Instant
  #inputData.active$Speed_RPM_Relation
  #inputData.active$OBD_Engine_Load
  #inputData.active$OBD_Speed_Km
  #inputData.active$OBD_Engine_RPM
  #inputData.active$OBD_Fuel_Flow_CCmin
  #inputData.active$OBD_Air_Pedal
  #inputData.active$Gear
  #inputData.active$KPL_Instant
  
  
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
  
  
  
  
  # # log transform 
  # #log.m1 <- log(inputData[, 4:20])
  # #m1.person <- inputData[, 2]
  # 
  # # apply PCA - scale. = TRUE is highly 
  # # advisable, but default is FALSE. 
  # inputData.pca <- prcomp(inputData[, 4:14],center = TRUE,scale. = TRUE) 
  # 
  # # print method
  # print(inputData.pca)
  # # plot method
  # plot(inputData.pca, type = "l")
  # plot(inputData.pca$sdev, type = "l")
  # # summary method
  # summary(inputData.pca)
  # 
  # #http://www.sthda.com/english/wiki/principal-component-analysis-in-r-prcomp-vs-princomp-r-software-and-data-mining
  # # Eigenvalues
  # eig <- (inputData.pca$sdev)^2
  # # Variances in percentage
  # variance <- eig*100/sum(eig)
  # # Cumulative variances
  # cumvar <- cumsum(variance)
  # plot(cumvar)
  # abline(a=70, b=0)
  # 
  # eig.inputData.active <- data.frame(eig = eig, variance = variance,cumvariance = cumvar)
  # head(eig.inputData.active)
  # 
  # # You are right, the loadings can help you here. They can be used to compute the correlation between the variables and 
  # # the principal components. Moreover, the sum of the squared loadings of one variable over all principal components is 
  # # equal to 1. Hence, the squared loadings tell you the proportion of variance of one variable explained by one principal 
  # # component. The problem with princomp is, it only shows the "very high" loadings. But since the loadings are 
  # # just the eigenvectors of the covariance matrix, one can get all loadings using the eigen command in R:
  # loadings <- eigen(cov(inputData[, 4:15]))$vectors
  # explvar <- loadings^2
  # plot(explvar)
  # 
  
}
