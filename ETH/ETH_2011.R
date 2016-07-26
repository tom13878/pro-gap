# -------------------------------------
# Ethiopia data 2011 - 2012 survey
# -------------------------------------

if(Sys.info()["user"] == "Tomas"){
  dataPath <- "C:/Users/Tomas/Documents/LEI/data/ETH/2011/Data"
} else {
  dataPath <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/ETH/2011/Data"
}

library(haven)
library(stringr)
library(dplyr)

options(scipen=999)

#######################################
############## LOCATION ###############
#######################################

# file containing the zone, region and district

location <- read_dta(file.path(dataPath, "Household/sect1_hh_w2.dta")) %>%
  select(household_id2, REGCODE = saq01, ZONECODE = saq02, EA=saq07, rural) %>%
  unique
location$type <- factor(location$rural, levels=1:3, labels=c("RURAL", "SMALL TOWN", "LARGE TOWN"))
location$rural <- ifelse(location$rural %in% 1, 1, 0)
location$REGCODE <- as.integer(location$REGCODE)

# match up with the names from the survey (prepared in a seperate file)

REGZONE <- read.csv(file.path(paste0(dataPath,"/../../.."), "Other/Spatial/ETH/REGZONEETH.csv"))

# join with household identifications
location <- left_join(location, REGZONE)

rm(REGZONE)
