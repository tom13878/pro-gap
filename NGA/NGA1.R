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

# easiest to filter on maizeas early as possible

oput <- read_dta("Post Harvest Wave 1/Agriculture/secta3_harvestw1.dta") %>%
    select(hhid, plotid, cropid, crop=sa3q1, qty=sa3q6a, qty_unit=sa3q6b,
           main_buyer=sa3q10, qty_sold_buyer=sa3q11a,
           qty_sold_buyer_unit=sa3q11b, qty_sold_naira=sa3q12)

# need to watch out because some of these crops have been
# imputed wrong
# unique(oput$crop) # shows poor spellings!!!
# also what does fallow fallow and fallow fallow fallow mean
# in the crop variable????
# maize is in lower and upper case, as well as MAIZE. and MAIZE
# with spaces and MAAIZE!
# make the crop code all upper case!

# could be some other legumes but don't recognize the names
# first make a crop count variable and a legumes variable

legumes <- c("PIGEON PEA", "SOYA BEANS", "LOCUST BEAN")
oput <- ddply(



# need to sort units. Kilogram=1, gram=2, Litre=3. Other units are
# offered but survey does not mention them. Basic unit is Kilorgrams
# change everything into kilograms

oput2 <- select(oput, hhid, plotid, cropid, qty, qty_unit, qty_sold=qty_sold_buyer,
                unit_sold=qty_sold_buyer_unit, valu=qty_sold_naira)

# units are not included in the data for a lot of values but
# in the documentattion there is a supplementary table
# sadly this means adding unit codes by hand. Actually
# labels are included but they suck! Half go missing
# better to use integer values

unit_code <- c(1, 2, 3, 11, 12, 13, 14, 21, 22, 23, 24, 31,
               32, 33, 34, 41, 42, 43, 51, 52, 53, 61,
               62, 63, 71, 72, 73, 74, 81, 82, 83,
               91, 92, 93, 94, 95)
weight <- c(1, 0.001, 1, 20, 50, 100, 120, 15, 30, 50, 75, 10, 25, 40, 75,
            5, 8, 15, 3, 5, 8, 15, 25, 40, 60, 85, 110, 150,
            1500, 2000, 2500, 10, 20, 25, 50, 200)

cnvrt <- data.frame(unit_code, weight)
oput2$qty_unit <- as.integer(oput2$qty_unit)
oput2 <- left_join(oput2, cnvrt, by=c("qty_unit"="unit_code"))
good <- c(1, 2, 3)
oput2 <- oput2[oput2$qty_unit %in% good,]


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
############### LABOUR ################
#######################################




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
