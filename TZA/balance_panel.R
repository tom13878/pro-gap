#######################################
###### ANALYSIS of TZA panel data #####
#######################################

library(haven)
detach(package:dplyr)
library(plyr)
library(dplyr)

# read in the data

CS1 <- read_dta("C:/Users/Tomas/Documents/Work/LEI/TZA10_data.dta")
CS2 <- read_dta("C:/Users/Tomas/Documents/Work/LEI/TZA12_data.dta")

CS1$region <- as_factor(CS1$region)
CS2$region <- as_factor(CS2$region)

CS1$zone <- as_factor(CS1$zone)
CS2$zone <- as_factor(CS2$zone)

CS1$AEZ <- as_factor(CS1$AEZ)
CS2$AEZ <- as_factor(CS2$AEZ)

#######################################
############## CLEANING ###############
#######################################

# remove plots which have more than 7 crops on them
# add dummy variables for number of crops. In this case
# seven is actually the maximum number of crops

CS1 <- CS1[!CS1$crop_count > 7,]

CS1$crop2 <- ifelse(CS1$crop_count %in% 2, 1, 0)
CS1$crop3 <- ifelse(CS1$crop_count %in% 3, 1, 0)
CS1$crop4 <- ifelse(CS1$crop_count %in% 4, 1, 0)
CS1$crop5 <- ifelse(CS1$crop_count %in% 5, 1, 0)
CS1$crop6 <- ifelse(CS1$crop_count %in% 6, 1, 0)
CS1$crop7 <- ifelse(CS1$crop_count %in% 7, 1, 0)

# remove any Nitrogen values greater than 100
# and any yields greater than 6000
# and any assets greater than 50 million
CS1 <- CS1[!CS1$N > 100, ]
CS1 <- CS1[!CS1$yld > 6000, ]
CS1 <- CS1[!CS1$asset > 50000000, ]

# winsor the nitrogen and maize prices
source("c:/USers/tomas/Documents/work/LEI/winsor5.R")

CS1$maize_prc <- winsor2(CS1$maize_prc, 5)

CS1$WPn <- ifelse(CS1$WPn %in% 0, NA, CS1$WPn)
CS1$WPn  <- winsor2(CS1$WPn, 5)
CS1$WPn <- ifelse(is.na(CS1$WPn), 0, CS1$WPn)


#######################################
############## CLEANING2 ##############
#######################################

# remove plots which have more than 7 crops on them
# add dummy variables for number of crops
CS2 <- CS2[!CS2$crop_count > 7,]

CS2$crop2 <- ifelse(CS2$crop_count %in% 2, 1, 0)
CS2$crop3 <- ifelse(CS2$crop_count %in% 3, 1, 0)
CS2$crop4 <- ifelse(CS2$crop_count %in% 4, 1, 0)
CS2$crop5 <- ifelse(CS2$crop_count %in% 5, 1, 0)
CS2$crop6 <- ifelse(CS2$crop_count %in% 6, 1, 0)
CS2$crop7 <- ifelse(CS2$crop_count %in% 7, 1, 0)

# remove any Nitrogen values greater than 100
# and any yields greater than 6000
# and any assets greater than 50 million
# in this case no assets are greater than
# 50 million. Watch out for NAs in comparisons
CS2 <- CS2[!CS2$N > 100, ]
CS2 <- CS2[!CS2$yld > 6000, ]
CS2.1 <- CS2[is.na(CS2$asset) | !CS2$asset > 50000000, ]

# winsor the nitrogen and maize prices
source("c:/USers/tomas/Documents/work/LEI/pro-gap/winsor.R")

CS2$maize_prc <- winsor2(CS2$maize_prc, 5)

CS2$WPn <- ifelse(CS2$WPn %in% 0, NA, CS2$WPn)
CS2$WPn <- winsor2(CS2$WPn, 5)
CS2$WPn <- ifelse(is.na(CS2$WPn), 0, CS2$WPn)

#######################################
############ BALANCE PANEL ############
#######################################

CS1.2 <- CS1[CS1$hhid %in% CS2$hhid,]
CS2.2 <- CS2[CS2$hhid %in% CS1$hhid,]

panel <- rbind(CS1.2, CS2.2)

rm(list=c("CS1", "CS2", "CS1.2", "CS2.2"))

#######################################
########## ADD CRE AVERAGES ###########
#######################################

panel <- ddply(panel, .(hhid), transform,
               N_bar=mean(N, na.rm=TRUE),
               P_bar=mean(P, na.rm=TRUE),
               area_bar=mean(area, na.rm=TRUE),
               SPEI_bar=mean(SPEI, na.rm=TRUE),
               manure_bar=mean(manure, na.rm=TRUE),
               hybrd_bar=mean(hybrd, na.rm = TRUE),
               legume_bar=mean(legume, na.rm = TRUE),
               crop_count_bar=mean(crop_count, na.rm=TRUE))

write_dta(panel, "C:/Users/Tomas/Documents/work/LEI/panel.dta")
