# -------------------------------------
# on and off farm income for Tanzania
# wave 1 (2008)
# output is a file containing income for
# each household from
# 1. off farm jobs and self employment
# 2. crop production (including fruit
# trees and permanent crops) in both
# the long rainy (lr) and short rainy (sr)
# seasons.
# 3. rented land during the lr and sr
# seasons
# 4. livestock sales in the last 12 months
# -------------------------------------

library(dplyr)
library(sjmisc)
library(haven)

if(Sys.info()["user"] == "Tomas"){
  dataPath <- "C:/Users/Tomas/Documents/LEI/data/TZA/2008/Data"
} else {
  dataPath <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/TZA/2008/Data"
}

#######################################
########## off farm income ############
#######################################

# not forthcoming

#######################################
########## on farm income #############
#######################################

# -------------------------------------
# on farm income from the sale of crops
# -------------------------------------

lr_crop <- read_dta(file.path(dataPath, "TZNPS1AGDTA_E/SEC_5A.dta")) %>%
  select(hhid, crop_code=zaocode, lr_crop_sold=s5aq1,
         lr_crop_qty=s5aq2, lr_crop_value=s5aq3,
         lr_customer1=s5aq4_1,
         lr_customer1_qty=s5aq5,
         lr_customer1_value=s5aq7,
         lr_customer2=s5aq4_2,
         lr_customer2_qty=s5aq7,
         lr_customer2_value=s5aq8) 

sr_crop <- read_dta(file.path(dataPath, "TZNPS1AGDTA_E/SEC_5B.dta")) %>%
  select(hhid, crop_code=zaocode, sr_crop_sold=s5bq1,
         sr_crop_qty=s5bq2, sr_crop_value=s5bq3,
         sr_customer1=s5bq4_1,
         sr_customer1_qty=s5bq5,
         sr_customer1_value=s5bq7,
         sr_customer2=s5bq4_2,
         sr_customer2_qty=s5bq7,
         sr_customer2_value=s5bq8)

# treat seasons together as income for one
# whole year
names(lr_crop) <- gsub("lr_", "", names(lr_crop))
lr_crop$season <- "lr"
names(sr_crop) <- gsub("sr_", "", names(sr_crop))
sr_crop$season <- "sr"

crop <- rbind(sr_crop, lr_crop); rm(lr_crop, sr_crop)

# calculate the full value of crops
# per household

on_farm_income_crop <- group_by(crop, hhid) %>%
  summarise(crop_value_hh=sum(crop_value, na.rm=TRUE))
rm(crop)

# -------------------------------------
# from permanent crops (fruit trees or 
# other perm crops crops)
# -------------------------------------

# fruit trees
fruit <- read_dta(file.path(dataPath, "TZNPS1AGDTA_E/SEC_7A.dta")) %>%
  select(hhid, crop_code=zaocode,
         sold_fruit=s7aq2, sold_fruit_kg=s7aq3, fruit_value=s7aq4)

# summarise to the household level
on_farm_income_fruit <- group_by(fruit, hhid) %>%
  summarise(fruit_value_hh=sum(fruit_value, na.rm=TRUE))
rm(fruit)

# permanent crops
perm <- read_dta(file.path(dataPath, "TZNPS1AGDTA_E/SEC_7B.dta")) %>%
  select(hhid, crop_code=zaocode,
         sold_perm=s7bq2, sold_perm_kg=s7bq3, perm_value=s7bq4)

# summarise to the household level
on_farm_income_perm <- group_by(perm, hhid) %>%
  summarise(perm_value_hh=sum(perm_value, na.rm=TRUE))
rm(perm)

# -------------------------------------
# income from renting out plots in 
# both seasons
# -------------------------------------

# from rented land - long rainy season
lr_rent <- read_dta(file.path(dataPath, "TZNPS1AGDTA_E/SEC_3A.dta")) %>%
  select(hhid, plotnum, lr_rent=s3aq4)

# from rented land - short rainy season
sr_rent <- read_dta(file.path(dataPath, "TZNPS1AGDTA_E/SEC_3B.dta")) %>%
  select(hhid, plotnum, sr_rent=s3bq4)

# treat seasons together as income for one
# whole year
names(lr_rent) <- gsub("lr_", "", names(lr_rent))
lr_rent$season <- "lr"
names(sr_rent) <- gsub("sr_", "", names(sr_rent))
sr_rent$season <- "sr"

rent <- rbind(sr_rent, lr_rent); rm(lr_rent, sr_rent)

# summarise to the household level
on_farm_income_rent <- group_by(rent, hhid) %>%
  summarise(rent_value_hh=sum(rent, na.rm=TRUE))
rm(rent)

# -------------------------------------
# income from livestock sales
# -------------------------------------

lvstock <- read_dta(file.path(dataPath, "TZNPS1AGDTA_E/SEC_10A.dta")) %>%
  select(hhid, animal, lvstk_sold=s10aq7,
         lvstk_number_sold=s10aq8, lvstk_value=s10aq9,
         slaughter=s10aq11, slaughter_qty=s10aq12,
         slaughter_qty_sold=s10aq13, slaughter_value=s10aq14)

on_farm_income_lvstock <- group_by(lvstock, hhid) %>%
  summarise(lvstock_value_hh=sum(lvstk_value, na.rm=TRUE),
            slaughter_value_hh=sum(slaughter_value, na.rm=TRUE))
rm(lvstock)

#######################################
# -------------------------------------
# calcualte total income
# -------------------------------------
#######################################

# use a full join as some households have one
# form of income but not another.

income_2008 <- full_join(on_farm_income_lvstock, on_farm_income_rent)
income_2008 <- full_join(income_2008, on_farm_income_perm)
income_2008 <- full_join(income_2008, on_farm_income_fruit)
income_2008 <- full_join(income_2008, on_farm_income_crop)


income_2008$income <- with(income_2008,
                      rowSums(cbind(crop_value_hh,
                                    fruit_value_hh, perm_value_hh,
                                    lvstock_value_hh, slaughter_value_hh,
                                    rent_value_hh),
                              na.rm=TRUE))

# remove all the other data
rm(on_farm_income_lvstock,
   on_farm_income_rent, on_farm_income_perm,
   on_farm_income_fruit, on_farm_income_crop,
   dataPath)
