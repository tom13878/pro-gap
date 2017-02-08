# -------------------------------------
# location variables TZA 2010
# -------------------------------------

if(Sys.info()["user"] == "Tomas"){
  dataPath <- "C:/Users/Tomas/Documents/LEI/data/TZA/2012/Data"
} else {
  dataPath <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/TZA/2012/Data"
}

# load packages
library(haven)
library(dplyr)
library(reshape2)

# remove scientific notation
options(scipen=999)

# read in section A of HH questionnaire containing
# the location information of each household

location <- read_dta(file.path(dataPath, "HH_SEC_A.dta")) %>%
  select(y3_hhid, REGCODE = hh_a01_1 , DISCODE = hh_a02_1, rural = y3_rural)
location$rural <- as.integer(location$rural)
location$REGCODE <- as.integer(location$REGCODE)

# match up with the names from the survey (prepared in a seperate file)

ZONEREGDIS <- read.csv(file.path(paste0(dataPath,"/../../.."), "Other/Spatial/TZA/ZONEREGDIS.csv"))

# join with household identifications

location <- left_join(location, ZONEREGDIS)

rm(ZONEREGDIS)
