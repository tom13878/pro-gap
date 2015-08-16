#######################################
############ NIGERIA 2010 #############
#######################################


setwd("c:/USers/tomas/Documents/work/LEI/data/NGA/NGA_2010_GHSP_v02_M_STATA")

library(haven)
library(stringr)
library(plyr)
library(dplyr)

options(scipen=999)

# Section A1 has info on the dry seaons intercropping Q 29
# Section 11b in the post planting has a question on irrigation Q24
# Section 11c PP has info on input costs 



#######################################
############### OUTPUT ################
#######################################

oput <- read_dta("Post Harvest Wave 1/Agriculture/secta3_harvestw1.dta") %>%
  select(zone, state, lga, hhid, plotid, cropid, land_area=sa3q5a, land_area_units=sa3q5b,
         qty=sa3q6a, qty_unit=sa3q6b, main_buyer=sa3q10, qty_sold_buyer=sa3q11a,
         qty_sold_buyer=sa3q11b,  qty_sold_naira=sa3q12)

# there is the option for more buyers which might be worth investigating
# for better idea of prices

#######################################
############## CHEMICAL ###############
#######################################

plot <- read_dta("Post Planting Wave 1/Agriculture/sect11c_plantingw1.dta") %>%
  select(hhid, plotid, pest=s11cq1, herb=s11cq10, leftOverFert=)

plot2 <- read_dta("Post Planting Wave 1/Agriculture/sect11d_plantingw1.dta") %>%
  select(hhid, plotid, fert= s11dq1, leftOverFert=s11dq2, leftOverFertType=s11dq3,
         leftOverFertQty=s11dq4, FreeFert=s11dq6, FreeFertType=s11dq7,
         FreeFertqty=s11dq8, firstFert=s11dq12, firstFertType=s11dq13,
         firstFertQty=s11dq15, firstFertValu=s11dq18, firstFertTransCost=s11dq16,
         secondFertUpFront=s11dq20, secondFertPayL8R=s11dq21, secondFert=s11dq23,
         secondFertType=s11dq25,
         secondFertQty=s11dq26, secondFertValu=s11dq29, secondFertTransCost=s11dq27,
         secondFertUpFront=s11dq31, secondFertPayL8R=s11dq32)

#######################################
################ SEEDS ################
#######################################

# section 11e of post planting
seed <- read_dta("Post Planting Wave 1/Agriculture/sect11e_plantingw1.dta") %>%
  select(hhid, plotid, )

#######################################
############### Assets ################
#######################################

# section A4 has info on household assets

implmt <- read_dta("") %>%
  
  
  select(y2_hhid, itemcode, qty=ag11_01, valu=ag11_02) %>%
  filter(!qty %in% 0, !is.na(qty), !valu %in% 0, !is.na(valu)) %>%
  transmute(y2_hhid, valu=qty*valu) %>%
  group_by(y2_hhid) %>%
  summarise(value=sum(valu))

# section A6 contains info on the animal holdings.
# including value of animal if sold

# in the post planting questionnaire there is also
# information on animal holdings section 11i