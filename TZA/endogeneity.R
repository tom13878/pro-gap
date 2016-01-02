# -------------------------------------
# Endogeneity analysis
# -------------------------------------

library(haven)
library(dplyr)

# candidate instruments
# 1. election data
# 2. number of years household head has
#    lived in village
# 3. distance to district headquarters
# 3. official quantity of subsidized
#    fertilizer distributed to household
#    region

# -------------------------------------
# read in election data matched with
# households for 2010 and 2012

setwd("c:/users/tomas/documents/lei/pol")

# -------------------------------------
# read in household geovariables file
# for 2010 and 2012

geoDir1 <- "C:/Users/Tomas/Documents/LEI/data/TZA/TZNPS2GEODTA/HH.Geovariables_Y2.dta"
geo1 <- read_dta(geoDir1) %>%
  select(y2_hhid, dist2town=dist02, dist2market=dist03, dist2HQ=dist05)

geoDir2 <- "C:/Users/Tomas/Documents/LEI/data/TZA/TZA_2012_LSMS_v01_M_STATA_English_labels/HouseholdGeovars_Y3.dta"
geo2 <- read_dta(geoDir2) %>%
  select(y3_hhid, dist2Rd=dist01, dist2town=dist02, dist2market=dist03, dist2HQ=dist05)

# note that distances are out somewhat between the two files
# may be better to use Michiel's files

# -------------------------------------
# read in household questionnaire to 
# get characteristics for both the 
# 2010 and 2012 surveys

# 2012

dataPath <- "C:/Users/Tomas/Documents/LEI/data/TZA/TZA_2012_LSMS_v01_M_STATA_English_labels"
HH <- read_dta(file.path(dataPath, "HH_SEC_B.dta")) %>%
  select(y3_hhid, indidy3, status=hh_b05, sex=hh_b02,
         yob=hh_b03_1, age=hh_b04, years=hh_b26)

# 99 in years variable means that the household member has
# lived in the community all his/her life
HH$years <- as.numeric(HH$years)
summary(HH$years[HH$status %in% 1])
HH$years <- ifelse(HH$years %in% 99, HH$age, HH$years)
HH <- filter(HH, status %in% 1) %>% select(-status)
HH$sex <- as_factor(HH$sex)
HH$yob <- as.integer(HH$yob)

# variables required for first stage
# 1. distance to markets
#    distance to Rd
# 2. household characteristics
#   age, sex, region
#   maize production
#   household income from maize, crops, livestock and other income
#   education in years/ education of household member 15-55
#   maize price
# 3. CRE variables
# 4. candidate instrumental variables