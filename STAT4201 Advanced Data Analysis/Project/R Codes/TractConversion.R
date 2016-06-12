#Code for converting latitude longitude to tracts

library("RJSONIO") #Load Library
library("plyr")
library("RODBC")
library(maptools)

NYC <-readShapePoly("/Users/parthpareek/Documents/Acads/02 Spring '16/03 ADA/Project/Raw/Archive/tl_2013_36_tract/tl_2013_36_tract")

tractLookup <- function(x) {
  # pt <- SpatialPoints(data.frame(x = -80.1, y = 26.3))
  pt <- SpatialPoints(data.frame(x = x$Lon, y = x$Lat))
  Mapping <- over(pt, NYC) # what index number does pt fall inside?
  Mapping <- data.frame(
    "GEOID" = as.character(Mapping$GEOID),
    "State" = as.character(Mapping$STATEFP) , 
    "County" = as.character(Mapping$COUNTYFP), 
    "Tract" = as.character(Mapping$TRACTCE), 
    "Tract_Name" = as.character(Mapping$NAME), 
    "INTPTLAT" = as.character(Mapping$INTPTLAT),
    "INTPTLON" = as.character(Mapping$INTPTLON),
    stringsAsFactors = FALSE)
  Mapping[is.na(Mapping)] <- "NULL"   
  return(Mapping)
}

tractLookup(data.frame("Lon" = -73.9851, "Lat" = 40.7589))
felony <- read.csv("Felony.csv")

lat <- felony$Latitude
lon <- felony$Longitude
tracts <- tractLookup(data.frame("Lon" = lon, "Lat" = lat))$Tract_Name
geo <- tractLookup(data.frame("Lon" = lon, "Lat" = lat))$GEOID
felony$tracts <- tracts
felony$geo <- geo
write.csv(felony, "/Users/parthpareek/Desktop/felony.csv")

