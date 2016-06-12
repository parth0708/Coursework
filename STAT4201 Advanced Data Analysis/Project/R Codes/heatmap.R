library(rgdal)    # for readOGR and others
library(sp)       # for spatial objects
library(leaflet)  # for interactive maps (NOT leafletR here)
library(dplyr)    # for working with data frames
library(ggplot2)  # for plotting

#Prepping training and test data sets
train <- read.csv("C:/Users/bhara_000/Downloads/train.csv")
test <- read.csv("C:/Users/bhara_000/Downloads/test.csv")
trainnew <- matrix(unlist(train),ncol = ncol(train),byrow = F)
colnames(trainnew) <- colnames(train)
train <- data.frame(na.omit(trainnew))
train <- train[,-(1:3)] #removing ID, Tract, Boroughs
train <- train[,-(26:32)] #Removing individual felony data
testnew <- matrix(unlist(test),ncol = ncol(test),byrow = F)
colnames(testnew) <- colnames(test)
test <- data.frame(na.omit(testnew))
testnew <- data.frame(na.omit(testnew))
testnew <- testnew[,-(1:3)] #removing ID, Tract, Boroughs
testnew <- testnew[,-(26:32)] #Removing individual felony data

#Predicting test values
actual <- test$Total_Felony #Total felony
model.rf <- randomForest(train[,-(26:27)],train[,26])
pred.rf <- predict(model.rf,test[,-(26:27)])
mse.rf <- mean((actual-pred.rf)^2) #62.13

test$pred <- pred.rf #Test data set with predicted values


NYC <- readOGR(dsn="C:/Users/bhara_000/Downloads/tl_2013_36_tract", layer = "tl_2013_36_tract")
## OGR data source with driver: ESRI Shapefile

# convert the GEOID to a character
NYC@data$GEOID<-as.character(NYC@data$GEOID)

# convert polygons to data.frame
ggtract<-fortify(NYC, region = "GEOID")
# join tabular data
ggt<- subset(ggtract, long < -73.71 & long > -74.26 & lat < 40.92 & lat > 40.49)
test$id <- as.character(test$id)
ggt<-left_join(ggt, test, by=c("id"))


ggplot() +
  geom_polygon(data = ggt , aes(x=long, y=lat, group = group, fill = Total_Felony), color="grey50") +
  scale_fill_gradientn(colours = c("red", "yellow", "green"),
                       values = c(1,0.5, .3, .2, .1, 0))+
  coord_map(xlim = c(-74.26, -73.71), ylim = c(40.49,40.92))