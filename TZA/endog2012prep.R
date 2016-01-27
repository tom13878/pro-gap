# -------------------------------------
# prepare dataframe for endogeneity 2012

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

VTotal12 <- read.csv("voucher/voucher201213.csv")

VTotal12$Region <- toupper(VTotal12$Region)
names(VTotal12) <- c("reg", "households", "Vtot1", "Vtot2")
VTotal12$reg <- gsub(" ", "", VTotal12$reg) # remove white space

VTotal12$vtot <- VTotal12$Vtot1 + VTotal12$Vtot2

# for now remove the second Njombe. There are two
# in the original data, not clear why???

VTotal12 <- VTotal12[-3, ]

# -------------------------------------
# read in household geovariables file
# for 2012

geo12 <- read_dta("C:/Users/Tomas/Documents/LEI/Data/TZA/TZA_2012_LSMS_v01_M_STATA_English_labels/HouseholdGeovars_Y3.dta") %>%
  select(y3_hhid, dist2Rd=dist01, dist2town=dist02, dist2market=dist03,
         dist2HQ=dist05)

# -------------------------------------
# read in household questionnaire to 
# get household characteristics

dataPath <- "C:/Users/Tomas/Documents/LEI/data/TZA/TZA_2012_LSMS_v01_M_STATA_English_labels"
HH12 <- read_dta(file.path(dataPath, "HH_SEC_B.dta")) %>%
  select(y3_hhid, indidy3, status=hh_b05, sex=hh_b02,
         yob=hh_b03_1, age=hh_b04, years=hh_b26)

# get 2012 regions to match with voucher info
reg2012 <- readRDS("C:/Users/Tomas/Documents/LEI/data/TZA/regions2012.rds") %>%
  select(y3_hhid, reg2012)

# 99 in years variable means that the household member has
# lived in the community all his/her life
HH12$years <- as.numeric(HH12$years)
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
  select(y3_hhid, indidy3, ed_any=hh_c03, start=hh_c04, end=hh_c08)

ed$end <- as.integer(as.character(ed$end))
ed$end <- ifelse(ed$end %in% 9999, NA, ed$end)
ed$ed_any <- as_factor(ed$ed_any)

# join se and ed to find years in school
HH12 <- left_join(HH12, ed)
HH12$education <- HH12$end - (HH12$yob + HH12$start)
HH12$education <- ifelse(HH12$ed_any %in% "NO", 0, HH12$education)
HH12 <- select(HH12, -start, -end, -yob)

# if anyone received negative years of schooling, set to NA
HH12$education <- ifelse(HH12$education < 0, NA, HH12$education)

HH12_x <- group_by(HH12, y3_hhid) %>% summarise(education1555=sum(education[cage %in% 2], na.rm=T), N1555=sum(cage %in% 2))
HH12 <- left_join(HH12, HH12_x); rm(HH12_x)
HH12 <- filter(HH12, status %in% "HEAD") %>% select(-indidy3, -status, -cage, -ed_any)

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
voucher12 <- group_by(voucher12, y3_hhid) %>% summarise(vouchTotal=sum(vouchTotal, na.rm=TRUE))

# binary response if any vouchers received (1/0)
voucher12$vouchAny <- ifelse(voucher12$vouchTotal %in% 0, 0, 1)

# -------------------------------------
# join the data

endog2012 <- left_join(voucher12, HH12)
endog2012 <- left_join(endog2012, unique(geo12))
endog2012 <- left_join(endog2012, polInt2012[, c(1, 7:10)])
endog2012 <- left_join(endog2012, reg2012)
endog2012 <- left_join(endog2012, VTotal12, by=c("reg2012"="reg"))

islands <- c("MJINI MAGHARIBI", "KASKAZINI UNGUJA", "KUSINI UNGUJA",
             "KASKAZINI PEMBA", "KUSINI PEMBA")

endog2012 <- endog2012[!endog2012$reg2012 %in% islands,]

# -------------------------------------
# use the panel key to change y3_hhid to
# y2_hhid

setwd("c:/users/tomas/documents")
key <- readRDS("panel_key.rds")

endog2012 <- rename(endog2012, hhid2012=y3_hhid)

endog2012 <- left_join(endog2012, key)

# -------------------------------------
# write to a file to be used in analysis

setwd("c:/users/tomas/documents/lei")
saveRDS(endog2012, "endog2012.rds")
