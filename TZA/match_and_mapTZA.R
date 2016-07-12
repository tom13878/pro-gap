# -------------------------------------
# mapping the 2008-2012 Tanzania data
# -------------------------------------

dataPath <- "C:/Users/Tomas/Documents/LEI"

# read in the preprocessed data from the file called
# data_combine.R. This file binds all the data together
# and also contains individual datasets for each of the
# three waves

source(file.path(dataPath, "pro-gap/TZA/data_combine.R"))
# ls() # most interested in fullData
fullDataMap <- fullData; rm("fullData", "fullData2")
fullDataMap$DISNAME <- as.character(fullDataMap$DISNAME)

# there are some small changes that need
# to be made to match the lsms data with
# the map

# Tanga
tanga <- c("TANGA", "TANGA URBAN")
fullDataMap$DISNAME <- ifelse(fullDataMap$DISNAME  %in% tanga, "TANGA", fullDataMap$DISNAME )

# Mororgoro
morogoro <- c("MOROGORO RURAL", "MOROGORO URBAN")
fullDataMap$DISNAME <- ifelse(fullDataMap$DISNAME  %in% morogoro, "MOROGORO", fullDataMap$DISNAME )

# BIHARAMULO
biharamulo <- c("CHATO", "BIHARAMULO")
fullDataMap$DISNAME <- ifelse(fullDataMap$DISNAME  %in% biharamulo, "BIHARAMULO", fullDataMap$DISNAME)

# BUKOBA RURAL
bukoba_rural <- c("BUKOBA RURAL", "MISENYE")
fullDataMap$DISNAME <- ifelse(fullDataMap$DISNAME  %in% bukoba_rural, "BUKOBA RURAL", fullDataMap$DISNAME)

# MUHEZA
muheza <- c("MUHEZA", "MKINGA")
fullDataMap$DISNAME <- ifelse(fullDataMap$DISNAME  %in% muheza, "MUHEZA", fullDataMap$DISNAME)

rm(tanga, morogoro, muheza, biharamulo, bukoba_rural)

# read in the file which correctly matches
# all the map regions and districts with
# the lsms-isa regions and districts
map2lsms <- read.csv(file.path(dataPath, "data/TZA/map2lsmsTZA.csv"))

# match the processed lsms data with the map2lsms file
# on REGNAME and DISNAME
# names(fullDataMap)
fullDataMap <- rename(fullDataMap, LSMSREGNAME = REGNAME, LSMSDISTNAME = DISNAME)
fullDataMap <- left_join(fullDataMap, map2lsms)

# there are some NA values, but these are the
# islands which are not important for our
# analysis

islands <- fullDataMap[is.na(fullDataMap$REGNAME), ]
# View(unique(select(islands, LSMSREGNAME, LSMSDISTNAME)))
fullDataMap <- fullDataMap[!is.na(fullDataMap$REGNAME), ]

# there are large outliers in the data. This
# is in part due to some very small area 
# measurements which result in unusually large
# yield and nitrogen per hectacre values

source(file.path(dataPath, "functions/winsor.R"))
fullDataMap$yld <- winsor(fullDataMap$yld)
fullDataMap$N <- winsor(fullDataMap$N)
fullDataMap$asset <- winsor(fullDataMap$asset)

# now we can group by whatever variables we choose
# here we group by year, region and district

by_district <- filter(fullDataMap, status == "HEAD", zaocode == 11) %>%
  group_by(surveyyear, REGNAME, DISTNAME) %>%
  summarise(yld = mean(yld, na.rm=TRUE),
            asset = mean(asset, na.rm=TRUE),
            N = mean(N, na.rm=TRUE),
            n = n(),
            assetph = log(mean(assetph, na.rm=TRUE)))

# This can then be matched up with the 2002 census map

TZA <- readOGR(dsn = file.path(dataPath, "data/TZA/Tanzania_District_EA_2002_region"),
               layer = "Tanzania_District_EA_2002_region")
islands <- c("Kaskazini Pemba", "Mjini Magharibi", "Kusini", "kaskazini", "Kusini Pemba")
TZA <- subset(TZA, !REGNAME %in% islands)

# fortify map for ggplot
tf <- fortify(TZA)
TZA@data$id <- unique(tf$id)

# join with the by_district lsms-isa data
TZA@data <- left_join(TZA@data, by_district)

# and finally this can all be mapped, looking at the 
# districts and years side by side

tf <- left_join(tf, TZA@data)

rm("by_district", "fullDataMap", "islands", "map2lsms", "trim", "winsor2", "TZA")

# and the plots
tf <- subset(tf, !is.na(surveyyear))

# ggplot(tf) + 
#   geom_polygon(data=tf, aes_string(x="long", y="lat", group="group", fill="yld"), colour="black", size = .1) +
#   coord_map("mercator") +
#   facet_wrap( ~ surveyyear) +
#   theme(legend.position="bottom") +
#   scale_fill_gradientn(colours=c("#ffffff", brewer.pal(n=9, name="Oranges")),
#                        na.value="#ffffff")


