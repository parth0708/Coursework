library(rgdal)    # for readOGR and others
library(sp)       # for spatial objects
library(leaflet)  # for interactive maps (NOT leafletR here)
library(dplyr)    # for working with data frames
library(ggplot2)  # for plotting
library(leaps)
library(maptools)
library(rgeos)

house <- read.csv("Housing_Dataset_v1.0.csv", na.strings= "NA")
house <- house[,-(2:3)]
housenew <- matrix(unlist(house),ncol = ncol(house),byrow = F)
colnames(housenew) <- colnames(house)
#housenew <- housenew[,-(1:3)]
house <- data.frame(na.omit(housenew))

library(mclust)
mod <- Mclust(house[,-1],G=10, control = emControl())
summary(mod)

pred <- matrix(data = NA, nrow = 1, ncol = ncol(house), byrow = F)
pred <- as.data.frame(pred)
colnames(pred) <- colnames(house)

pred$Over_18_year <- 1
pred$Over_60_year <- 1
pred$Median_Household_Income <- 50000
pred$Percentage_college_degree <- 1
pred$Unemployment_rate <- 0
pred$Race_White <- 0
pred$Race_Black <- 0
pred$Race_NativeAmerican <- 0
pred$Race_Asian <- 1
pred$Race_Other <- 0
pred$Schools <- 1
pred$Recreational <- 1
pred$Criminal_Justice <- 0
pred$Health <- 1
pred$Pharmacy <- 1
pred$Mental_Heath <- 0
pred$Disabilities <- 0
pred$Daycare <- 0
pred$Senior_homes <- 0
pred$Homeless_shelters <- 0
pred$Transport <- 1
pred$Waste_Management <- 0
pred$Crime_Rate <- 0

predval <- predict(mod,newdata = pred)$z
# summary(subset(house,mod$classification==
#                  predict(mod,newdata = pred)$classification))
for (i in 1:nrow(house)){
  prede[i] <- switch(mod$classification[i],"1"=predval[1],"2"=predval[2],"3"=predval[3],"4"=predval[4],"5"=predval[5],"6"=predval[6],"7"=predval[7],"8"=predval[8],"9"=predval[9],"10"=predval[10])  
}


house1 <- cbind(house,prede)

NYC <- readOGR(dsn="C:/Users/bhara_000/Downloads/tl_2013_36_tract", layer = "tl_2013_36_tract")
## OGR data source with driver: ESRI Shapefile

# convert the GEOID to a character
NYC@data$GEOID<-as.character(NYC@data$GEOID)

# convert polygons to data.frame
ggtract<-fortify(NYC, region = "GEOID")
# join tabular data
ggt<- subset(ggtract, long < -73.71 & long > -74.26 & lat < 40.92 & lat > 40.49)
house1$id <- as.character(house1$id)
ggt<-left_join(ggt, house1, by=c("id"))


ggplot() +
  geom_polygon(data = ggt , aes(x=long, y=lat, group = group, fill = prede), color="grey50") +
  scale_fill_gradientn(colours = c("green", "yellow", "red"),
                       values = c(1,0.5, .3, .2, .1, 0))+
  coord_map(xlim = c(-74.26, -73.71), ylim = c(40.49,40.92))


