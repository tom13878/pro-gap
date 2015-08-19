#######################################
###### ANALYSIS of TZA panel data #####
#######################################

library(haven)
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

# form the balanced panel

CS1.2 <- CS1[CS1$hhid %in% CS2$hhid,]
CS2.2 <- CS2[CS2$hhid %in% CS1$hhid,]

panel <- rbind(CS1.2, CS2.2)

rm(list=c("CS1", "CS2", "CS1.2", "CS2.2"))

# Add in some new average variables for the CRE
panel <- ddply(panel, .(hhid), transform,
               N_bar=mean(N, na.rm=TRUE),
               P_bar=mean(P, na.rm=TRUE),
               area_bar=mean(area, na.rm=TRUE),
               SPEI_bar=mean(SPEI, na.rm=TRUE),
               manure_bar=mean(manure, na.rm=TRUE),
               hybrd_bar=mean(hybrd, na.rm = TRUE),
               legume_bar=mean(legume, na.rm = TRUE),
               crop_count_bar=mean(crop_count, na.rm=TRUE))

