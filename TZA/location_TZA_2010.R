# -------------------------------------
# location variables TZA 2010
# -------------------------------------

if(Sys.info()["user"] == "Tomas"){
  dataPath <- "C:/Users/Tomas/Documents/LEI/data/TZA/2010/Data"
} else {
  dataPath <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/TZA/2010/Data"
}

# load packages
library(haven)
library(dplyr)

# remove scientific notation
options(scipen=999)

# read in section A of HH questionnaire containing
# the location information of each household
location <- read_dta(file.path(dataPath,
                               "TZNPS2HH1DTA/HH_SEC_A.dta")) %>%
  select(y2_hhid, REGCODE = region, DISCODE = district, ward, ea,
         rural = y2_rural)
location$rural <- as.integer(location$rural)

# match up with the names from the survey
# basic information document
# (prepared in a seperate file) as these
# are not availabe from the raw data

ZONEREGDIS <- read.csv(file.path(paste0(dataPath,"/../../.."),
                                 "Other/Spatial/TZA/ZONEREGDIS.csv"))

# join with household identifications

location <- left_join(location, ZONEREGDIS)

rm(ZONEREGDIS, dataPath)
