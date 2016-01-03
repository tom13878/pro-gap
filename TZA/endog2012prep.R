# -------------------------------------
# prepare datframe for endogeneity 2012

library(haven)
library(stringr)
library(dplyr)

# -------------------------------------
# read in election data matched with
# households for 2012

setwd("C:/Users/Tomas/Documents/LEI/pol/data")
polInt2012 <- read.csv("polInt2012.csv")


# -------------------------------------
# read in the official number of vouchers
# distributed to each region

dataPath <- "C:/Users/Tomas/Documents/LEI/pol/data"

VTotal12 <- read.csv(paste(dataPath, "voucher201213.csv", sep="/"))

VTotal12$Region <- toupper(VTotal12$Region)
names(VTotal12) <- c("reg", "households", "Vtot1", "Vtot2")
VTotal12$reg <- gsub(" ", "", VTotal12$reg)

VTotal12$vtot <- VTotal12$Vtot1 + VTotal12$Vtot2

# -------------------------------------
# read in household geovariables file
# for 2012

geo12 <- read.csv("C:/Users/Tomas/Documents/LEI/Data/TZA/TZA_geo_total_2012.csv", stringsAsFactors=F) %>% 
  select(y3_hhid, dist2Rd=dist01, dist2town=dist02, dist2market=dist03,
         dist2HQ=dist05, SPEI, reg=NAME_1, dis=NAME_2)

geo12$reg <- toupper(geo12$reg)

# -------------------------------------
# read in household questionnaire to 
# get household characteristics

dataPath <- "C:/Users/Tomas/Documents/LEI/data/TZA/TZA_2012_LSMS_v01_M_STATA_English_labels"
HH12 <- read_dta(file.path(dataPath, "HH_SEC_B.dta")) %>%
  select(y3_hhid, indidy3, status=hh_b05, sex=hh_b02,
         yob=hh_b03_1, age=hh_b04, years=hh_b26)

# 99 in years variable means that the household member has
# lived in the community all his/her life
HH12$years <- as.numeric(HH12$years)
summary(HH12$years[HH12$status %in% 1])
HH12$years <- ifelse(HH12$years %in% 99, HH12$age, HH12$years)
HH12$status <- as_factor(HH12$status)
HH12$sex <- as_factor(HH12$sex)
HH12$yob <- as.integer(HH12$yob)

HH12$cage <- cut(HH12$age, breaks = c(0, 15, 55, max(HH12$age, na.rm=TRUE)),
                 labels=1:3, include.lowest = TRUE, right = TRUE)

# -------------------------------------
# education of household head and sum of
# education of all household members
# between the ages of 15 and 55

dataPath <- "C:/Users/Tomas/Documents/LEI/data/TZA/TZA_2012_LSMS_v01_M_STATA_English_labels"
ed <- read_dta(file.path(dataPath, "HH_SEC_C.dta")) %>%
  select(y3_hhid, indidy3, start=hh_c04, end=hh_c08)

ed$end <- as.integer(as.character(ed$end))
ed$end <- ifelse(ed$end %in% 9999, NA, ed$end)

# join se and ed to find years in school
HH12 <- left_join(HH12, ed)
HH12$educ <- HH12$end - (HH12$yob + HH12$start)
HH12 <- select(HH12, -start, -end, -yob)

# if anyone received negative years of schooling, set to NA
HH12$educ <- ifelse(HH12$educ < 0, NA, HH12$educ)

HH12_x <- group_by(HH12, y3_hhid) %>% summarise(educ1555=sum(educ[cage %in% 2], na.rm=T))
HH12 <- left_join(HH12, HH12_x); rm(HH12_x)

# -------------------------------------
# total number of vouchers received by
# the household at the plot level

dataPath <- "C:/Users/Tomas/Documents/LEI/data/TZA/TZA_2012_LSMS_v01_M_STATA_English_labels"
voucher12 <- read_dta(file.path(dataPath, "AG_SEC_3A.dta")) %>%
  select(y3_hhid, plotnum, vouch1=ag3a_50, vouch2=ag3a_57)
voucher12$vouch1 <- as_factor(voucher12$vouch1)
voucher12$vouch2 <- as_factor(voucher12$vouch2)
voucher12$vouch1 <- ifelse(voucher12$vouch1 %in% "YES", 1, 0)
voucher12$vouch2 <- ifelse(voucher12$vouch2 %in% "YES", 1, 0)
voucher12$vouchTotal <- voucher12$vouch1 + voucher12$vouch2