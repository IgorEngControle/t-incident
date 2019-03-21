#Script about the characterization of tweetes and incidents fusion
#Entradas

# input -------------------------------------------------------------------
lapply(c('ggplot2', 'ggmap','RCurl', 'RColorBrewer'), library, character.only = TRUE) #load libraries
DIR_TWITTER = 'data/tweets_manhattan/tweets_gerados.csv' #DIR refer to path of the enter data
#Shift in the data hour because of the diferent time zone. The TIME_ZONE is used to tranform to local data
TIME_ZONE = 4 
RADIUS <- 0.1 #size of the radius of the incident
df  <- read.csv(DIR_TWITTER, sep = ",", stringsAsFactors = FALSE) #stringAsFactors
df$created_at <- strptime(df$created_at, "%Y-%m-%d %H:%M:%S", tz="GMT") - TIME_ZONE*3600
bounding_box = c(-74.051092,40.698372,-73.866384,40.867347) #bounding-box of the input region
DIR_NOT_INCDENT <- '/home/igor/Área de Trabalho/IC/Social Media/t-incident/data/incidents_manhattan/NOT_INCIDENT_LAYER.csv'
DIR_INCIDENT <- '/home/igor/Área de Trabalho/IC/Social Media/t-incident/data/incidents_manhattan/NEW_INCIDENT_LAYER(Manhattan).csv'
dir_fusion_name <- dir('data/data_grouped/20_NOT_INCIDENT/', pattern = "*.csv") # get fusion file names
map <- get_map(bounding_box, source = 'stamen', maptype = 'toner')  #Load map based on bounding box

# read in all the traffic files, appending the path before the filename 

#DIR_FUSED <- '/home/igor/Área de Trabalho/IC/Social Media/t-incident/data/data_grouped/10_NOT_INCIDENT/twitter_incident(Radius0.1).csv'
#Tweets in week -------------------------------------------------------------
weekdays_label <-c('Sun','Mon','Tues','Wed','Thurs','Fri','Sat')
df$weekday <- as.numeric(format(df$created_at, '%w')) # Agora eu defino ele numérico
#df$weekday <-factor(df$weekday, levels = c(0,1,2,3,4,5,6), labels = c('Sun','Mon','Tues','Wed','Thurs','Fri','Sat'))
hist(df$weekday, breaks = -0.5+0:7, col ="yellow",xaxt='n' , ylim= c(0, max(table(df$weekday))), main = "Histogram of Tweets in the Week", xlab = "Days of the Week", ylab = "Tweets Frequency")
axis(1, at=0:6, labels=weekdays_label)
# Breaks estabelece o espaço entre as barras de plot ( quando eu coloco -0.5 segnifica um espaço de metade da minha barra (a marcação vai para o centro), e os intervalos que eu quero é de 0 a sete)
# Tweets in Times of Day -------------------------------------------------------------
## Frequency of tweets in the hour of day
df$hours <- as.numeric(format(df$created_at, '%H'))
hist(df$hours, breaks = -0.5+0:24, col ="yellow", xaxt='n', main = "Histogram of Tweets in Times of Day", xlab = "Times of Day", ylab = "Tweets Frequency")
axis(1, at=0:23, labels=0:23)

# g <- ggplot(df, aes(hours))
# g + geom_density(alpha=0.6, fill = 'red' ) + 
#   labs(title="Hours of day", 
#        x="Hours",
#        y = 'Tweets Density')+
#   scale_x_continuous(breaks = round(seq(min(df$hours), 
#                                         max(df$hours), by = 1),1))

# Tweets in a month------------------------------------------------------------
## Frequency of tweets during the month of year
month_label = c("Jan","Feb.","Mar","Apr","May","Jun","Jul","Aug","Sept","Oct","Nov","Dec")
df$month <- as.numeric(format(df$created_at, '%m'))
hist(df$month, breaks = -0.5+1:13, col ="darkblue", xaxt='n',ylim = c(0, max(table(df$month))),  main = "Histogram of Tweets in Month", xlab = "Months", ylab = "Tweets Frequency")
axis(1, at=1:12, labels=month_label)


p<-ggplot(df, aes(x=month_label,y =table(df$month),  color=sex)) +
  geom_histogram(fill="white", position="dodge")
plot(p)

# Spacial Analysis of Tweets ------------------------------------------------------------

# Geral Tweets ------------------------------------------------------------
##Simply plot all tweets in a map based on bounding box area
# tweets_geral <- ggmap(map, extend = 'device') +
#   geom_point(aes(x = long, y = lat),size=  0.2,  data = df,  alpha = .5,  na.rm = T,  colour="red") +
#   ggtitle('Frequency of Tweets',subtitle = 'Plot of tweets in map')
# plot(tweets_geral)

tweets_geral <- ggmap(map, extend = 'device') + geom_point(
  size = 0.3,
  aes(x = long, y = lat), colour = 'gold',
  data = df,
  alpha = .5,
  na.rm = T) +
  theme(legend.position="top")+
  xlab("Longitude") +
  ylab("Latitude") +
  theme(legend.position="none")
plot(tweets_geral)  #plot incident type in a map


# Tweets on map with week frequency  -----------------------------------
##Simply plot all tweets in a map based on bounding box area with week frequency
df$weekday <-factor(df$weekday, levels = c(0,1,2,3,4,5,6), labels = c('Sun','Mon','Tues','Wed','Thurs','Fri','Sat'))
tweets_in_week <- ggmap(map, extend = 'device') +
  geom_point(aes(x = long, y = lat), size = 0.3, data = df,  alpha = .5,  na.rm = T,  colour="red") + facet_wrap(~weekday)
tweets_in_week <- tweets_in_week + ggtitle('Tweets in Week') 
plot(tweets_in_week) 
# Tweets on map with hour frequency -------------------------------------------
##Simply plot all tweets in a map based on bounding box area with hour frequency
tweets_in_hour <- ggmap(map, extend = 'device') +
  geom_point(aes(x = long, y = lat), size = 0.3, data = df,  alpha = .5,  na.rm = T,  colour="red") + facet_wrap(~hours)
tweets_in_hour <- tweets_in_hour + ggtitle('Tweet per hour') 
plot(tweets_in_hour)  #todos os tweets plotados juntos

# Tweets density on map ---------------------------------------------------
tweets_density <- ggmap(map, extend = 'device') + geom_density2d(data = df, aes(x = long, y = lat), size = 0.3) + 
  stat_density2d(data = df, 
                 aes(x = long, y = lat, fill = ..level.., alpha = ..level..), size = 2, 
                 bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0.00, 0.5), guide = FALSE)

plot(tweets_density)  #tweets density

# Incident Analisys on map -------------------------------------------------------
## Plot of incidents on map

make_circles <- function(centers, radius, nPoints = 100){
  # centers: the data frame of centers with ID    
  # radius: radius measured in kilometer
  #
  meanLat <- mean(centers$incidentLat_From)
  # length per longitude changes with lattitude, so need correction
  radiusLon <- radius /111 / cos(meanLat/57.3) 
  radiusLat <- radius / 111
  circleDF <- data.frame(ID = rep(centers$incidentID, each = nPoints))
  angle <- seq(0,2*pi,length.out = nPoints)
  
  
  circleDF$lon <- unlist(lapply(centers$incidentLong_From, function(x) x + radiusLon * cos(angle)))
  circleDF$lat <- unlist(lapply(centers$incidentLat_From, function(x) x + radiusLat * sin(angle)))
  
  return(circleDF)
  
}
not_incident <- read.csv(DIR_NOT_INCDENT, sep = ",", stringsAsFactors = FALSE)
not_incident$incidentStartTime <- strptime(not_incident$incidentStartTime , "%Y-%m-%d %H:%M:%S", tz="GMT") - TIME_ZONE*3600
incident <- read.csv(DIR_INCIDENT, sep = ",", stringsAsFactors = FALSE)
incident$incidentStartTime <- strptime(incident$incidentStartTime , "%Y-%m-%d %H:%M:%S", tz="GMT") - TIME_ZONE*3600
#incident <- incident[incident$incidentType != 'CONSTRUCTION',]
#incident <- read.csv(DIR, sep = ",", stringsAsFactors = FALSE)

##Merge the two kinds of incident
names <- colnames(not_incident) #extract the names of mainly elemenst
names <- names[-7]  #Exclude the last one 'Description'
incident <- incident[,names]  #Select terms on incident dataframe
not_incident <-not_incident[,names] #Select terms on not_incident dataframe
all_incident <- rbind(not_incident, incident) #combine dataframes by rows
all_incident$isIncident <- 'INCIDENT'
all_incident[all_incident$incidentType == 'NOT_INCIDENT', 'isIncident'] <- 'NOT_INCIDENT'
all_incident$incidentStartTime <- strptime(all_incident$incidentStartTime , "%Y-%m-%d %H:%M:%S", tz="GMT") - TIME_ZONE*3600

#Define circle
##Variables that contains the circles to be generate
myCircle <- make_circles(incident,RADIUS) # radius in km
myCircle2 <- make_circles(not_incident,RADIUS)

# Incident Geral ----------------------------------------------------------
##Plot of all incident/ not incident on the map
incident_map_geral <- ggmap(map, extend = 'device', colour='isIncident') + geom_point(
  size = 0.5,
  aes(x = all_incident$incidentLong_From, y = all_incident$incidentLat_From,colour=all_incident$isIncident ),
  data = all_incident,
  alpha = .5,
  na.rm = T) +
  theme(legend.position="top", legend.margin=margin(0,-10,0,-65),
  legend.box.margin=margin(0,0,-10,0))+
  xlab("Longitude") +
  ylab("Latitude") +
  guides(color = guide_legend( override.aes = list(size = 1)))+
  labs(color=NULL)
plot(incident_map_geral)  #plot incident type in a map



# incident and twitter density --------------------------------------------
twitter_ds <- df[,c('lat','long')]
twitter_ds$type <- 'Tweets'

incident_ds <- all_incident[all_incident$isIncident == 'INCIDENT', c('incidentLat_From','incidentLong_From')]
colnames(incident_ds)<- c("lat","long")
incident_ds$type <- 'Incidente'

not_incident_ds <- all_incident[all_incident$isIncident == 'NOT_INCIDENT', c('incidentLat_From','incidentLong_From')]
colnames(not_incident_ds)<- c("lat","long")
not_incident_ds$type <- 'Não-incidente'


incident_twitter_ds <- rbind(incident_ds,twitter_ds,not_incident_ds)

label_names <- list(
  'Incidente'="Here and Bing",
  'Não-incidente'="TripAdvisor",
  'Tweets'="Twitter"
)

change_labeller <- function(variable,value){
  return(label_names[value])
}

incident_twitter_geral <- ggmap(map, extend = 'device', colour='type') + geom_point(
  size = 0.6,
  aes(x = long, y = lat,colour=type ),
  data = incident_twitter_ds,
  alpha = 1,
  na.rm = T) +
  theme(legend.position="top", legend.margin=margin(0,-10,0,-65),
        legend.box.margin=margin(0,0,-10,0))+
  xlab("Longitude") +
  ylab("Latitude") +
  guides(color = guide_legend( override.aes = list(size = 5)))+
  labs(color=NULL) + facet_wrap(~type, labeller=change_labeller)

plot(incident_twitter_geral)  #plot incident type in a map


# Incident in Week --------------------------------------------------------
all_incident$weekday <-  as.numeric(format(all_incident$incidentStartTime, '%w'))
all_incident$weekday <- factor(all_incident$weekday, levels = c(0,1,2,3,4,5,6), labels = c('Sun','Mon','Tues','Wed','Thurs','Fri','Sat'))
incident_per_week <- ggmap(map, extend = 'device') + geom_point(
  size = 1.5,
  aes(x = all_incident$incidentLong_From, y = all_incident$incidentLat_From,colour=all_incident$isIncident),
  data = all_incident,
  alpha = .5,
  na.rm = T,
)

incident_per_week <- incident_per_week +   facet_wrap(~weekday)
plot(incident_per_week+ labs(colour = "Incident Type")) #plot of incident per a week

# Incident Densisty -------------------------------------------------------
incident_density <-  ggmap(map, extend = 'device') + geom_density2d(data = incident, aes(x = incident$incidentLong_From, y = incident$incidentLat_From), size = 0.3) + 
  stat_density2d(data = incident, 
                 aes(x = incident$incidentLong_From, y = incident$incidentLat_From, fill = ..level.., alpha = ..level..), size = 0.01, 
                 bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE)

plot(incident_density)  #densidade de TODOS os incidentes


incident_map <-  ggmap(map, extend = 'device') + geom_polygon(data = myCircle, aes(lon, lat, group = ID), color = "red", alpha = 0)


incident_map <- incident_map + labs(colour = "Incident Type")

plot(incident_map)  #plot cirlce of


incident_map_cirlce_and_tweets <-  ggmap(map, extend = 'device') + geom_polygon(data = myCircle, aes(lon, lat, group = ID), color = "red", alpha = 0)+
  geom_point(aes(x = long, y = lat), size = 0.5, data = df[df$incident_type == 'NULL',],  alpha = .5,  na.rm = T,  colour="blue")+ 
  geom_polygon(data = myCircle2, aes(lon, lat, group = ID), color = "green", alpha = 0)

incident_map_cirlce_and_tweets <- incident_map_cirlce_and_tweets + labs(colour = "Incident Type")

plot(incident_map_cirlce_and_tweets)  #plot cirlce and not incidents


# fusion_analisys ---------------------------------------------------------
# General Fusion Analisys -------------------------------------------------
# require(purrr) for map(), reduce() ; require(readr)  # for read_csv()
# fused_data <- dir_fusion_name %>%
#   map(function(x) read_csv(file.path("data/data_grouped/20_NOT_INCIDENT/", x))) %>%  
#   reduce(rbind)
# Reading files generate dataframes with colname fileName 
fused_data <- do.call("rbind", lapply(dir_fusion_name, function(x) {
  dat <- read.csv(paste("data/data_grouped/20_NOT_INCIDENT/",x,sep = '' ), stringsAsFactors = FALSE , header=TRUE)
  dat$created_at <- strptime(dat$created_at, "%Y-%m-%d %H:%M:%S", tz="GMT") - TIME_ZONE*3600
  dat$fileName <- tools::file_path_sans_ext(basename(x))
  dat
})) 


#fused_data <- read.csv(DIR_FUSION, colClasses=c(rep("character",7)))  #analisar são os incidentes que tem mais id do tweet
#fused_data <- read.csv(DIR_FUSED,  sep = ",", stringsAsFactors = FALSE)
fused_data <- fused_data [fused_data$event !='UNKNOW',]  #Só os dados em que eu tenho apenas algum evento ligado a tweets
fused_data$incident_id <- as.character(fused_data$incident_id)

fused_data$Radius <-'Radius 0.5 km'
fused_data[fused_data$fileName == 'twitter_incident(Radius0.05)','Radius'] <- 'Radius 0.05 km'
fused_data[fused_data$fileName == 'twitter_incident(Radius0.1)','Radius'] <- 'Radius 0.1 km'
fused_data[fused_data$fileName == 'twitter_incident(Radius0.15)','Radius'] <- 'Radius 0.15 km'
fused_data[fused_data$fileName == 'twitter_incident(Radius0.2)','Radius'] <- 'Radius 0.2 km'
fused_data[fused_data$fileName == 'twitter_incident(Radius0.3)','Radius'] <- 'Radius 0.3 km'
fused_data[fused_data$fileName == 'twitter_incident(Radius0.4)','Radius'] <- 'Radius 0.4 km'
fused_data[fused_data$fileName == 'twitter_incident(Radius0.5)','Radius'] <- 'Radius 0.5 km'
fused_data$Radius <- factor(fused_data$Radius, c('Radius 0.05 km', 'Radius 0.1 km','Radius 0.15 km', 'Radius 0.2 km', 'Radius 0.3 km', 'Radius 0.4 km', 'Radius 0.5 km'))

#table(incident$incidentID)  #Verificar a agragação de tweets em uma único incident

#fused_data_1 <- fused_data[fused_data$incident_id != 'NULL',]

# Temporal Analysis of Fusion Data ----------------------------------------
# Tweets Grouped per Hour -------------------------------------------------
fused_data$hours <- as.numeric(format(fused_data$created_at, '%H'))
g <- ggplot(fused_data, aes(hours))
g + geom_density(aes(fill=factor(event)), alpha=0.8 ) + 
  labs(title="Hours of day", 
       x="Hours",
       y = 'Tweets Density',
       fill="Event")+
        scale_x_continuous(breaks = round(seq(min(fused_data$hours), 
        max(fused_data$hours), by = 1),1)) +
        facet_wrap(~Radius)

# Tweets Density per Week -------------------------------------------------

fused_data$weekday <- as.numeric(format(fused_data$created_at, '%w'))
#fused_data$weekday <-factor(fused_data$weekday, levels = c(0,1,2,3,4,5,6), labels = c('Sun','Mon','Tues','Wed','Thurs','Fri','Sat'))

ggplot(fused_data, aes(x=weekday)) + 
  geom_density(aes(fill=factor(event)), alpha=0.8 ) + 
  labs(title="Tweets Density in Weekdays", 
       y=" Tweets Density",
       x = 'Weekdays',
       fill = 'Event')+
      scale_x_continuous(breaks = round(seq(min(fused_data$weekday), 
    max(fused_data$weekday), by = 1),1),
    labels = c('Sun','Mon','Tues','Wed','Thurs','Fri','Sat') ) +
    facet_wrap(~Radius)

# Tweets Spacial Density --------------------------------------------------


##Define colnames with the radius parameter

fusioned_map <- ggmap(map, extend = 'device', color = 'event') + geom_point(
  size = 0.5,
  aes(x = long, y = lat, colour = event),
  data = fused_data,alpha = .5) + 
  theme(axis.text.x= element_text(angle = 90), legend.position = c(0.7, 0.1), legend.margin=margin(0,0,0,0))+
  guides(color = guide_legend( override.aes = list(size = 4)))+
  xlab("Longitude") +
  ylab("Latitude") +
  facet_wrap(~Radius)+
  labs(color=NULL)#aes - estética do map (editar e colocar alguns features)

plot(fusioned_map)


# incident circle analysis ------------------------------------------------


incident_map_cirlce_and_tweets <-  ggmap(map, extend = 'device') + geom_polygon(data = myCircle, aes(lon, lat, group = ID), color = "red", alpha = 0)+ 
  geom_polygon(data = myCircle2, aes(lon, lat, group = ID), color = "green", alpha = 0)+
  geom_point(aes(x = long, y = lat), size = 0.5, data = df,  alpha = .5,  na.rm = T,  colour="blue")

incident_map_cirlce_and_tweets <- incident_map_cirlce_and_tweets + labs(colour = "Incident Type")

plot(incident_map_cirlce_and_tweets)  #plot cirlce and not incidents



incident_map_cirlce_and_tweets <-  ggmap(map, extend = 'device') +
geom_point(aes(x = long, y = lat),size=  0.2,  data = df,  alpha = .5,  na.rm = T,  colour="red")+
incident_map_cirlce_and_tweets <- incident_map_cirlce_and_tweets + labs(colour = "event")

plot(incident_map_cirlce_and_tweets)  #plot cirlce and not incidents


