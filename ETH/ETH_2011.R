# -------------------------------------
# Ethiopia data 2011 - 2012 survey
# -------------------------------------

dataPath <- "C:/Users/Tomas/Documents/LEI/data/ETH"

library(haven)
library(stringr)
library(dplyr)

options(scipen=999)

oput <- read_dta(file.path(dataPath, "ETH201011/sect9_ph_w1.dta")) %>%
  dplyr::select(holder_id, household_id, parcel_id, field_id,
                crop=crop_code, qty_kg=ph_s9q12_a, qty_gr=ph_s9q12_b)

oput$crop <- as_factor(oput$crop)

# check how many plots have maize grown on them -> 1110 maize plots with maize on them
oput_maize <- group_by(oput, holder_id, household_id, parcel_id, field_id) %>%
  summarise(maize=any(crop %in% "MAIZE"))

oput_maize2 <- filter(oput, crop %in% "MAIZE") 
sum(is.na(oput_maize2$qty_kg) & is.na(oput_maize2$qty_gr)) # 447 plots with no record
