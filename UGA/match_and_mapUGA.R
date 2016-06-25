# UGANDA mapping
# map source
# http://mapeastafrica.com/countries/east-africa-shapefiles/uganda-shapefiles/
library(rgdal)

dataPath <- "C:/Users/Tomas/Documents/LEI/data/UGA"

UGA <- readOGR(dsn = file.path(dataPath, "Uganda_admin_WGS84"),
               layer = "Uganda_admin_2014_WGS84") # length(unique(UGA@data$ADM1_NAME)) -> 69!!!!


library(raster)
UGA <- getData(name="GADM", country="UGA", level = 2) # i62 districts not good
