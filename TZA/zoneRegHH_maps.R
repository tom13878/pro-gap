# -------------------------------------
# region and zone map of Tanzania
# and a map showing where households are
# located
# -------------------------------------

dataPath <- "C:/Users/Tomas/Documents/LEI"

library(raster)
library(rgdal)
library(haven)
library(RColorBrewer)
library(ggplot2)
detach(package:dplyr)
library(dplyr)

# -------------------------------------
# get map from 2002 census. Available
# from 
# https://openmicrodata.wordpress.com/2010/12/16/tanzania-shapefiles-for-eas-villages-districts-and-regions/
# -------------------------------------

# need to make some small changes to match map
# regions with lsms-isa regions
TZA <- readOGR(dsn = file.path(dataPath, "data/TZA/Tanzania_Region_EA_2002_region"),
                layer = "Tanzania_Region_EA_2002_region")
TZA@data$REGNAME <- toupper(TZA@data$REGNAME)
TZA@data$REGNAME <- gsub(" ", "-", TZA@data$REGNAME)
TZA@data$REGNAME <- ifelse(TZA@data$REGNAME %in% "KASKAZINI", "KASKAZINI-UNGUJA", TZA@data$REGNAME)
TZA@data$REGNAME <- ifelse(TZA@data$REGNAME %in% "KUSINI", "KUSINI-UNGUJA", TZA@data$REGNAME)
TZA@data$REGNAME <- ifelse(TZA@data$REGNAME %in% "MJINI-MAGHARIBI", "MJINI/MAGHARIBI-UNGUJA", TZA@data$REGNAME)

# read in preprepared file linking regions and zones
ZONEREG <- read.csv(file.path(dataPath, "data/TZA/ZONEREGDIS.csv")) %>%
  select(ZONE, REGNAME) %>% unique()

# join to get a zone variable
TZA@data <- left_join(TZA@data, ZONEREG)

# -------------------------------------
# mapping - preparation
# -------------------------------------

# fortify polygon and link to the data
tf <- fortify(TZA)
TZA@data$id <- unique(tf$id)
tf <- left_join(tf, TZA@data)

# color options that look good in maps
# brewer.pal.info

# ignore zanzibar - our focus is on mainland tanzania
tfmain <- tf[!tf$ZONE %in% "ZANZIBAR",]

# get coordinates of region centroids
# these will be used to add points and names
# to regions
coords <- as.data.frame(coordinates(TZA))
names(coords) <- c("x", "y")
coords <- cbind(unique(select(TZA@data, ZONE, REGNAME)), coords)

# ignore zanzibar when looking at names
coordsmain <- coords[!coords$ZONE %in% "ZANZIBAR",]

# -------------------------------------
# mainland map of regions and zones
# -------------------------------------

# main <- ggplot(tfmain) + 
#   geom_polygon(data=tfmain, aes(x=long, y=lat, group=group, fill=ZONE), colour="black", size = .1) +
#   coord_map("mercator") +
#   ggtitle("Zones and regions of Mainland Tanzania") +
#   scale_fill_manual(values = brewer.pal(n=8, name="Accent")) +
#   geom_text(data=coordsmain, aes(label = REGNAME, x = x, y = (y + 0.2))) +
#   geom_point(data=coordsmain, aes(x, y)) +
#   guides(fill=guide_legend(title=NULL))

# -------------------------------------
# mainland map with gps locations of
# lsms-isa households
# -------------------------------------

# need to remove zanzibar so read in file containg
# regions and link to zones

location <- read_dta(file.path(dataPath, "data/TZA/2010/Data/TZNPS2HH1DTA/HH_SEC_A.dta")) %>%
  select(y2_hhid, REGCODE = region, DISCODE = district, rural = y2_rural)
location$rural <- as.integer(location$rural)

# match up with the names from the survey (prepared in a seperate file)

ZONEREGDIS <- read.csv(file.path(dataPath, "data/TZA/ZONEREGDIS.csv"))

# join with household identifications

location <- left_join(location, ZONEREGDIS)

rm(ZONEREGDIS)

# read in gps coordinates of each household
geo10 <- read_dta(file.path(dataPath, "data/TZA/2010/Data/TZNPS2GEODTA/HH.Geovariables_Y2.dta")) %>%
  select(y2_hhid, lon=lon_modified, lat=lat_modified)

# link locations and coordinates
geo10 <- left_join(location, geo10)

# remove zanzibar
geo10main <- geo10[!geo10$ZONE %in% "ZANZIBAR",]

rm("coords", "geo10", "tf", "location", "TZA","ZONEREG")
# -------------------------------------
# map of mainland tanzania, with locations
# of lsms-isa households in 2010
# -------------------------------------

# main2 <- ggplot(tfmain) +
#   geom_polygon(data=tfmain, aes(x=long, y=lat, group=group), fill="white", colour="black", size = .1) +
#   coord_map("mercator") +
#   geom_point(data=geo10main, aes(x=lon, y=lat, color=factor(rural))) +
#   ggtitle("Location of households: mainland Tanzania") +
#   guides(color=guide_legend(title=NULL)) +
#   scale_color_discrete(labels=c("Urban", "Rural"))