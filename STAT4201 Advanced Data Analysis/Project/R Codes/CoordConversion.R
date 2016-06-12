#Code for converting x-y coordinates to tracts
frisk <- read.csv("2014.csv")

#x = 981106                      #nyc x coord
#y = 195544                      #nyc y coord

coordconvert <- function(x,y){
  a = 6378137                     #' major radius of ellipsoid, map units (NAD 83)
  e = 0.08181922146               #' eccentricity of ellipsoid (NAD 83)
  angRad = pi/180                 #' number of radians in a degree
  pi4 = pi/4                      #' Pi / 4
  
  p0 = 40.166667 * angRad        #' latitude of origin
  p1 = 40.666667 * angRad        #' latitude of first standard parallel
  p2 = 41.033333 * angRad        #' latitude of second standard parallel
  m0 = -74.000000 * angRad       #' central meridian
  x0 = 984250.000000             #' False easting of central meridian, map units
  
  m1 = cos(p1) / sqrt(1 - ((e ** 2) * sin(p1) ** 2))
  m2 = cos(p2) / sqrt(1 - ((e ** 2) * sin(p2) ** 2))
  t0 = tan(pi4 - (p0 / 2))
  t1 = tan(pi4 - (p1 / 2))
  t2 = tan(pi4 - (p2 / 2))
  t0 = t0 / (((1 - (e * (sin(p0)))) / (1 + (e * (sin(p0)))))**(e / 2))
  t1 = t1 / (((1 - (e * (sin(p1)))) / (1 + (e * (sin(p1)))))**(e / 2))
  t2 = t2 / (((1 - (e * (sin(p2)))) / (1 + (e * (sin(p2)))))**(e / 2))
  n = log(m1 / m2) / log(t1 / t2)
  f = m1 / (n * (t1 ** n))
  rho0 = a * f * (t0 ** n)
  
  x = x - x0
  pi2 = pi4 * 2
  rho = sqrt((x ** 2) + ((rho0 - y) ** 2))
  theta = atan(x / (rho0 - y))
  t = (rho / (a * f)) ** (1 / n)
  lon = (theta / n) + m0
  x = x + x0
  
  lat0 = pi2 - (2 * atan(t))
  
  part1 = (1 - (e * sin(lat0))) / (1 + (e * sin(lat0)))
  lat1 = pi2 - (2 * atan(t * (part1 ** (e / 2))))
  while (abs(lat1 - lat0) < 0.000000002) lat0 = lat1
  part1 = (1 - (e * sin(lat0))) / (1 + (e * sin(lat0)))
  lat1 = pi2 - (2 * atan(t * (part1 ^ (e / 2))))
  
  lat = lat1 / angRad
  lon = lon / angRad  
return (list(lat = lat, lon = lon))
}

frisk <- frisk[complete.cases(frisk[,"xcoord"]),]
x <- frisk[,"xcoord"]
y <- frisk[,"ycoord"]

frisk$lat <- round(coordconvert(x,y)$lat,5)
frisk$lon <- round(coordconvert(x,y)$lon,5)

frisktracts <- tractLookup(data.frame("Lon" = frisk$lon, "Lat" = frisk$lat))$Tract_Name
frisk$tracts <- frisktracts
write.csv(frisk, "/Users/parthpareek/Desktop/frisk.csv")
