# -------------------------------------
# Ethiopia data 2013 - 2014 survey
# -------------------------------------

# WDswitch
# dataPath <- "D:\\Data\\IPOP\\SurveyData\\"
# wdPath <- "D:\\Dropbox\\Michiel_research\\2285000066 Africa Maize Yield Gap"
# setwd(wdPath)

dataPath <- "C:/Users/Tomas/Documents/LEI/data/ETH"

library(haven)
library(stringr)
library(plyr)
library(dplyr)

options(scipen=999)


#######################################
############### OUTPUT ################
#######################################

oput <- read_dta(file.path(dataPath, "Post-Harvest/sect9_ph_w2.dta")) %>%
  dplyr::select(household_id, parcel_id, field_id, crop=crop_code, qty=ph_s9q05)

# -------------------------------------
# table(oput$household_id %in% "")
# FALSE  TRUE 
# 29318   257 
# need to remove the values that have missing households.

oput <- filter(oput, !(oput$household_id %in% ""))
oput$crop <- as_factor(oput$crop)

# -------------------------------------
# add a dummy if a legume was grown
# count number of crops per field

legumes <- c("CHICK PEAS", "HARICOT BEANS", "HORSE BEANS", "LENTILS",
             "FIELD PEAS", "VETCH", "GIBTO", "SOYA BEANS", "CASTOR BEANS")
oput <- ddply(oput, .(household_id, parcel_id, field_id), transform,
              crop_count=length(crop[!is.na(crop)]),
              legume=ifelse(any(crop %in% legumes), 1, 0))

# select on maize and remove observations with quantity NA or 0
oput_maze <- oput[oput$crop %in% "MAIZE" & ! is.na(oput$qty) & !oput$qty %in% 0,]

rm("oput", "legumes")

#######################################
############## CHEMICAL ###############
#######################################

chem0 <- read_dta(file.path(dataPath, "Post-Planting/sect3_pp_w2.dta")) %>%
  dplyr::select(household_id, parcel_id, field_id)

# -------------------------------------
# data on fertilizer use is not one 
# observation per row. As a result
# the data needs to be altered!
fert1 <- read_dta(file.path(dataPath, "Post-Planting/sect3_pp_w2.dta")) %>%
  dplyr::select(household_id, parcel_id, field_id, typ=pp_s3q15, qty=pp_s3q16_a,
                purch=pp_s3q16b, purch_kg=pp_s3q16c, valu=pp_s3q16d) %>%
  filter(typ %in% 1)

fert1$typ <- "UREA"
fert1$purch <- ifelse(fert1$purch %in% 1, 1, 0)
  
fert2 <- read_dta(file.path(dataPath, "Post-Planting/sect3_pp_w2.dta")) %>%
  dplyr::select(household_id, parcel_id, field_id, typ=pp_s3q18, qty=pp_s3q19_a,
                purch=pp_s3q19b, purch_kg=pp_s3q19c, valu=pp_s3q19d) %>%
  filter(typ %in% 1)

fert2$typ <- "DAP"
fert2$purch <- ifelse(fert2$purch %in% 1, 1, 0)

fert <- rbind(fert1, fert2)
chem <- left_join(chem, fert)

# -------------------------------------
                
fert3 <- read_dta(file.path(dataPath, "Post-Planting/sect3_pp_w2.dta")) %>%
  dplyr::select(household_id, parcel_id, field_id, typ=pp_s3q20a, valu=pp_s3q20c) %>%
  filter(typ %in% 1)

fert1$typ <- "OTHER"
            
                
                
              
