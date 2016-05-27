# -------------------------------------
# mapping the 2008-2012 Tanzania data
# -------------------------------------

dataPath <- "C:/Users/Tomas/Documents/LEI"

# read in the file which correctly matches
# all the map regions and districts with
# the lsms-isa regions and districts
map2lsms <- read.csv(file.path(dataPath, "data/TZA/map2lsmsTZA.csv"))

# read in the preprocessed data from the file called
# data_combine.R. This file binds all the data together
# and also contains individual datasets for each of the
# three waves

source(file.path(dataPath, "pro-gap/TZA/data_combine.R"))
ls() # most interested in fullData
fullData$DISNAME <- as.character(fullData$DISNAME)

# there are some small changes that need
# to be made to match the lsms data with
# the map

# Tanga
tanga <- c("TANGA", "TANGA URBAN")
fullData$DISNAME <- ifelse(fullData$DISNAME  %in% tanga, "TANGA", fullData$DISNAME )

# Mororgoro
morogoro <- c("MOROGORO RURAL", "MOROGORO URBAN")
fullData$DISNAME <- ifelse(fullData$DISNAME  %in% morogoro, "MOROGORO", fullData$DISNAME )

# BIHARAMULO
biharamulo <- c("CHATO", "BIHARAMULO")
fullData$DISNAME <- ifelse(fullData$DISNAME  %in% biharamulo, "BIHARAMULO", fullData$DISNAME)

# BUKOBA RURAL
bukoba_rural <- c("BUKOBA RURAL", "MISENYE")
fullData$DISNAME <- ifelse(fullData$DISNAME  %in% bukoba_rural, "BUKOBA RURAL", fullData$DISNAME)

# MUHEZA
muheza <- c("MUHEZA", "MKINGA")
fullData$DISNAME <- ifelse(fullData$DISNAME  %in% muheza, "MUHEZA", fullData$DISNAME)

rm(tanga, morogoro, muheza, biharamulo, bukoba_rural)

# match the processed lsms data with the map2lsms file
# on REGNAME and DISNAME
names(fullData)
fullData <- rename(fullData, LSMSREGNAME = REGNAME, LSMSDISTNAME = DISNAME)
fullData <- left_join(fullData, map2lsms)

# there are some NA values, but these are the
# islands which are not important for our
# analysis

islands <- fullData[is.na(fullData$REGNAME), ]
View(unique(select(islands, LSMSREGNAME, LSMSDISTNAME)))
fullData <- fullData[!is.na(fullData$REGNAME), ]

# there are large outliers in the data. This
# is in part due to some very small area 
# measurements which result in unusually large
# yield and nitrogen per hectacre values

source(file.path(dataPath, "functions/winsor.R"))
fullData$yld <- winsor(fullData$yld)
fullData$N <- winsor(fullData$N)

# now we can group by whatever variables we choose
# here we group by year, region and district

by_district <- filter(fullData, status == "HEAD", zaocode == 11) %>%
  group_by(surveyyear, REGNAME, DISTNAME) %>%
  summarise(yld = mean(yld, na.rm=TRUE),
            asset = mean(asset, na.rm=TRUE),
            N = mean(N, na.rm=TRUE),
            n = n())

# This can then be matched up with the 2002 census map

TZA <- readOGR(dsn = file.path(dataPath, "data/TZA/Tanzania_District_EA_2002_region"),
               layer = "Tanzania_District_EA_2002_region")

# fortify map for ggplot
tf <- fortify(TZA)
TZA@data$id <- unique(tf$id)

# join with the by_district lsms-isa data
TZA@data <- left_join(TZA@data, by_district)

# and finally this can all be mapped, looking at the 
# districts and years side by side

tf <- left_join(tf, TZA@data)

# and the plots

ggplot(tf) + 
  geom_polygon(data=tf, aes_string(x="long", y="lat", group="group", fill="yld"), colour="black", size = .1) +
  coord_map("mercator") +
  facet_wrap( ~ surveyyear) +
  theme(legend.position="bottom") +
  scale_fill_gradientn(colours=c("#ffffff", brewer.pal(n=9, name="Oranges")),
                       na.value="#ffffff")


