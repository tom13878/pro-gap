# -------------------------------------
# region and zone map of Tanzania
# and a map showing where households are
# located
# -------------------------------------

dataPath <- "C:/Users/Tomas/Documents/LEI/data/TZA/TZA2010"

library(raster)
library(dplyr)
library(ggplot2)

# get map from GADM

TZA <- getData('GADM', country = "TZA", level = 1)
TZA@data$NAME_1 <- toupper(TZA@data$NAME_1)

# get region names from pre-prepared file

ZONEREGDIS <- read.csv(file.path(paste0(dataPath,"/.."), "ZONEREGDIS0810.csv")) %>%
  select(ZONE, REGNAME)

# change a couple of SWAHILI names in the map to
# match the LSMS data

TZA@data$NAME_1 <- ifelse(TZA@data$NAME_1 %in% "ZANZIBAR SOUTH AND CENTRAL", "KUSINI-UNGUJA", TZA@data$NAME_1)
TZA@data$NAME_1 <- ifelse(TZA@data$NAME_1 %in% "ZANZIBAR WEST", "MJINI/MAGHARIBI-UNGUJA", TZA@data$NAME_1)

TZA@data <- left_join(TZA@data, ZONEREGDIS, by=c("NAME_1" = "REGNAME"))

# -------------------------------------
# mapping
# -------------------------------------

tf <- fortify(TZA)
TZA@data$OBJECTID <- as.character(TZA@data$OBJECTID)
tf <- left_join(tf, TZA@data, by=c("id"="OBJECTID"))

#colors
brewer.pal.info

# make two maps, one for mainland, one for zanzibar
EAST_COAST <- c("PWANI", "TANGA", "DAR-ES-SALAAM")
tfmain <- tf[!tf$ZONE %in% "ZANZIBAR",]
tfzan <- tf[tf$ZONE %in% "ZANZIBAR" | tf$NAME_1 %in% EAST_COAST,]

# get coordinates to add region names to map
coords <- as.data.frame(coordinates(TZA))
names(coords) <- c("x", "y")
coords <- cbind(unique(select(TZA@data, ZONE, REGNAME=NAME_1)), coords)

coordsmain <- coords[!coords$ZONE %in% "ZANZIBAR",]
coordszan <- coords[coords$ZONE %in% "ZANZIBAR" | coords$REGNAME %in% "DAR-ES-SALAAM",]

# add an offset to make the map look better
offset <- matrix(c(0.65, 0, 0.75, 0, 0.7, 0.2, 0.6, 0, 0.7, -0.2, 0.4, 0 ), nrow=6, byrow=TRUE)
coordszan <- cbind(coordszan, (coordszan[, c("x", "y")] + offset))
names(coordszan)[c(5, 6)] <- c("x2", "y2")

# mainland map
main <- ggplot(tfmain) + 
  geom_polygon(data=tfmain, aes(x=long, y=lat, group=group, fill=ZONE), colour="black", size = .1) +
  coord_map("mercator") +
  ggtitle("Zones and regions of Mainland Tanzania") +
  scale_fill_manual(values = brewer.pal(n=8, name="Accent")) +
  geom_text(data=coordsmain, aes(label = REGNAME, x = x, y = (y + 0.2))) +
  geom_point(data=coordsmain, aes(x, y))

# zanzibar map
zan <- ggplot(tfzan) + 
  geom_polygon(data=tfzan, aes(x=long, y=lat, group=group, fill=ZONE), colour="black", size = .1) +
  coord_map("mercator") +
  ggtitle("Zones and regions of Zanzibar") +
  scale_fill_manual(values = brewer.pal(n=8, name="Accent")) +
  geom_text(data=coordszan, aes(label = REGNAME, x2 , y2)) +
  geom_point(data=coordszan, aes(x, y)) +
  xlim(38.5, 41)

library(gridExtra)
library(grid)

ggsave("zones_and_regs.pdf", main,  scale=3)

# -------------------------------------
# map with gps coordinates of households
# -------------------------------------

# file containing the zone, region and district

location <- read_dta(file.path(dataPath, "TZNPS2HH1DTA/HH_SEC_A.dta")) %>%
  select(y2_hhid, REGCODE = region, DISCODE = district, rural = y2_rural)
location$rural <- as.integer(location$rural)

# match up with the names from the survey (prepared in a seperate file)

ZONEREGDIS <- read.csv(file.path(paste0(dataPath,"/.."), "ZONEREGDIS0810.csv"))

# join with household identifications

location <- left_join(location, ZONEREGDIS)

rm(ZONEREGDIS)

geo10 <- read_dta(file.path(dataPath, "TZNPS2GEODTA/HH.Geovariables_Y2.dta")) %>%
  select(y2_hhid, lon=lon_modified, lat=lat_modified)

geo10 <- left_join(location, geo10)

geo10main <- geo10[!geo10$ZONE %in% "ZANZIBAR",]
geo10zan <- geo10[geo10$ZONE %in% "ZANZIBAR" | geo10$ZONE %in% EAST_COAST,]

#mainland
main2 <- ggplot(tfmain) +
  geom_polygon(data=tfmain, aes(x=long, y=lat, group=group), fill="white", colour="black", size = .1) +
  coord_map("mercator") +
  geom_point(data=geo10main, aes(x=lon, y=lat, color=factor(rural))) +
  ggtitle("Location of households: mainland")

#zanzibar
zan2 <- ggplot(tfzan) +
  geom_polygon(data=tfzan, aes(x=long, y=lat, group=group), fill="white", colour="black", size = .1) +
  coord_map("mercator") +
  geom_point(data=geo10zan, aes(x=lon, y=lat, color=factor(rural))) +
  ggtitle("Location of households: zanzibar") +
  xlim(38.5, 41)

grid.arrange(main2, zan2, ncol=2)
