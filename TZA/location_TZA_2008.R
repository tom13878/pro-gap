# -------------------------------------
# location_2008 variables TZA 2008
# -------------------------------------

if(Sys.info()["user"] == "Tomas"){
  dataPath <- "C:/Users/Tomas/Documents/LEI/data/TZA/2008/Data"
} else {
  dataPath <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/TZA/2008/Data"
}

# load packages
library(haven)
library(dplyr)

# remove scientific notation
options(scipen=999)

# file containing the zone, region and district

location_2008 <- read_dta(file.path(dataPath, "TZNPS1HHDTA_E/SEC_A_T.dta")) %>%
  select(hhid, REGCODE = region, DISCODE = district, rural = rural)
location_2008$rural <- ifelse(location_2008$rural %in% "Rural", 1, 0)
location_2008$REGCODE <- as.integer(location_2008$REGCODE)

# match up with the names from the survey (prepared in a seperate file)

ZONEREGDIS <- read.csv(file.path(paste0(dataPath,"/../../.."), "Other/Spatial/TZA/ZONEREGDIS.csv"))

# join with household identifications

location_2008 <- left_join(location_2008, ZONEREGDIS)

rm(ZONEREGDIS, dataPath)
