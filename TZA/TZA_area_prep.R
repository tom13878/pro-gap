# area imputation for 2012 data. 
# you have to choose what variables are to be included. Use exactly the same
# ones as are used in the paper on area imputation for the second wave of the
# panel.
library(foreign)
library(plyr)
library(dplyr)

setwd("C:/Users/Tomas/Documents/Work/LEI/data/TZA/TZA_2012_LSMS_v01_M_STATA_English_labels")

source("C:/Users/Tomas/Documents/Work/LEI/data/TZA/plus.R")
# -----------------------------
# data on self reported areas
# ----------------------------

# read in agricultural questionnaire section 2 A
AG2A <- read_dta("AG_SEC_2A.dta")

# both area measurements are given in acres in year 3
areas <- select(AG2A, y3_hhid, plotnum, area.est = ag2a_04, area.gps = ag2a_09)

# ---------------------------
# the plot manager characteristics
# ---------------------------

# read in the household questionnaire survey
# make the assumption that the plot manager is the head of the household.
HB <- read_dta('HH_SEC_B.dta')
HB1 <- select(HB, y3_hhid, sex = hh_b02, age = hh_b04, status = hh_b05 )
HB1 <- filter(HB1, status == "HEAD")

# ---------------------------
# Household Characteristics
# ---------------------------

# still using household data from above HB
# Household in a rural setting: 1/0
HHA <- read_dta("HH_SEC_A.dta")
rural_weight <- select(HHA, y3_hhid, y3_rural, y3_weight)

# calculate the number of household members within certain ages groups
# create a variable with the ages of each person and some with sex as well 
HB2 <- mutate(HB, age_group = cut(hh_b04, c(0, 5, 14, 39, 59, 108),
                                  include.lowest = TRUE, right = FALSE))

RS <- select(HB2, y3_hhid, age_group, id = indidy3, sex = hh_b02)

# set ages without sex
RS$less5 <- ifelse(RS$age_group == '[0,5)', 1, NA)
RS$g5l15 <- ifelse(RS$age_group == '[5,14)', 1, NA)
RS$above60 <- ifelse(RS$age_group == '[59,108]', 1, NA)

# ages with sex
RS$male15_39 <- ifelse(RS$age_group=='[14,39)' & RS$sex=='MALE', 1, NA)
RS$female15_39 <- ifelse(RS$age_group=='[14,39)' & RS$sex=='FEMALE', 1, NA)
RS$male40_59 <- ifelse(RS$age_group=='[39,59)' & RS$sex=='MALE', 1, NA)
RS$female40_59 <- ifelse(RS$age_group=='[39,59)' & RS$sex=='FEMALE', 1, NA)

# summarise to get variables per household.
by_hhid <- group_by(RS, y3_hhid) %>% summarise(
  less5 = sum(less5, na.rm = TRUE),
  above60 = sum(above60, na.rm = TRUE),
  male15_39 = sum(male15_39, na.rm = TRUE),
  female15_39 = sum(female15_39, na.rm = TRUE),
  male40_59 = sum(male40_59, na.rm = TRUE),
  female40_59 = sum(female40_59, na.rm = TRUE),
  g5l15 = sum(g5l15, na.rm = TRUE))

# -------------------------
# Household characteristics expanded
# -------------------------

# add a section here later with some more variables detailed in 
# the imputation paper


# ---------------------------
# plot characteristics
# ---------------------------

# read in agricultural questionnaire section 3 A
AG3A <- read_dta('AG_SEC_3A.dta')

# still to add household and hired labour
AG3A_vars <- select(AG3A, y3_hhid, plotnum, soil = ag3a_10, soilq = ag3a_11,
                    erosion = ag3a_13, slope = ag3a_17, irrig = ag3a_18,
                    fallow = ag3a_22, fallow.years = ag3a_23, owned = ag3a_25,
                    org = ag3a_41, orgQ1 = ag3a_42, inorg1 = ag3a_47,
                    inorg.type1 = ag3a_48, inorgQ1 = ag3a_49, voucher1 = ag3a_50,
                    inorg2 = ag3a_54, inorg.type2 = ag3a_55, inorgQ2 = ag3a_56,
                    voucher2 = ag3a_57, pest = ag3a_60, pestQ = ag3a_62_1,
                    pestU = ag3a_62_2, short.rain = ag3a_81, short.rain.crop = ag3a_82)

# calcualte the number of plots per household.
numberPlots <- ddply(select(AG3A, y3_hhid, plotnum), .(y3_hhid), summarize,
                     plots = sum(!is.na(plotnum)))

# calcualte the hired labour per plot
hir.lab.days <- apply(select(AG3A, ag3a_74_1:ag3a_74_16)[, c(1:3, 5:7, 9:11, 13:15)], 1, plus)

# revalue the ownership variable into two categories: OWNED, RENTED, FREE
# mutate to create a variable which is TRUE if plot was rented and FALSE 
# otherwise, and the same for if the plot was owned.
AG3A_vars$owned <- as_factor(AG3A_vars$owned )
AG3A_vars$owned <- revalue(AG3A_vars$owned, c("SHARED - RENT" = "RENTED", "SHARED - OWN" = "OWNED", "RENTED IN" = "RENTED"))
AG3A_vars$hir.lab.days <- hir.lab.days
# ---------------------------
# combine all data into a single dataset
# ---------------------------

comp <- left_join(AG3A_vars, areas)
comp <- left_join(comp, by_hhid)
comp <- left_join(comp,HB1)
comp <- left_join(comp, numberPlots)
comp <- left_join(comp, rural_weight)
comp <- select(comp, y3_hhid, plotnum, area.gps, area.est, sex, age, y3_rural, y3_weight,
               less5, above60, male15_39, female15_39, male40_59, female40_59,
               g5l15, fallow, owned, plots, soilq, hir.lab.days, orgQ1,
               inorgQ1, inorgQ2, irrig)
comp$inorgferttot <- apply(select(comp, inorgQ1, inorgQ2), 1, plus)
comp <- select(comp, everything(), -(inorgQ1:inorgQ2), -fallow)

row.names(comp) <- paste(comp$y3_hhid, comp$plotnum, sep="")
comp$sex <- unclass(comp$sex)
comp$y3_rural <- unclass(comp$y3_rural)
comp$owned <- unclass(comp$owned)
comp$soilq <- unclass(comp$soilq)
comp$irrig <- unclass(comp$irrig)

source("C:/Users/Tomas/Documents/Work/LEI/data/TZA/winsor.R")
comp <- winsor1(comp, 'area.est', 0.025)
comp <- winsor1(comp, 'area.gps', 0.025)

comp$area2 <- comp$area.est*comp$area.est
comp$area3 <- comp$area.est*comp$area.est*comp$area.est
comp$age2 <-  comp$age*comp$age
# -------------------------
# regression of chosen variables
# for imputed area - also would be good to do some analysis
# to find out which variables are responsible for the variance
# in the imputed variables. Also change model to include everything
# will be included in the analysis later on.
# ---------------------------

# -------------------------------------
# in the imputed data paper there is a 
# a bit about data cleaning before the
# analysis. Certainly need to remove
# any values where there are missing
# values for both area measurements
# also consider removing strange
# area.gps measurements. So drop all of
# those values (1710)
# -------------------------------------
bad <- is.na(comp$area.gps) & is.na(comp$area.est)
comp <- comp[!bad, ]

# in addition probably a good idea to
# winsor out some of the crazy values
# One way of doing this is to use the
# winsor function and knock off the 
# top and bottom 2.5 % like in the MI
# paper - start with the top 2.5% for
# area.est and area.gps




write.csv(comp, "C:/Users/Tomas/Documents/Work/LEI/data/TZA/comp.csv", row.names=TRUE)
