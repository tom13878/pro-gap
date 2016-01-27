# -------------------------------------
# prepare dataframe for the endogeneity
# analysis of data for 2010

library(haven)
library(stringr)
library(dplyr)

# -------------------------------------
# read in election data matched with
# households for 2010 

setwd("C:/Users/Tomas/Documents/LEI/pol/data")

polInt2010 <- read.csv("polInt2010.csv")
polInt2010$y2_hhid <- ifelse(str_length(polInt2010$y2_hhid) < 16,
                             paste("0", polInt2010$y2_hhid, sep=""),
                             polInt2010$y2_hhid)

# -------------------------------------
# read in the official number of vouchers
# distributed to each region in 2010-11

VTotal10 <- read.csv("voucher/voucher201011.csv")

VTotal10$Region <- toupper(VTotal10$Region)
names(VTotal10) <- c("reg", "households", "Vtot1", "Vtot2")
VTotal10$reg <- gsub(" ", "", VTotal10$reg) # remove white space

VTotal10$vtot <- VTotal10$Vtot1 + VTotal10$Vtot2

# -------------------------------------
# read in household geovariables file

dataPath <- "C:/Users/Tomas/Documents/LEI/data/TZA"
geo10 <- read.csv(file.path(dataPath, "TZA_geo_total_2010.csv"), stringsAsFactors=F) %>%
  select(y2_hhid, dist2town=dist02, dist2market=dist03, dist2HQ=dist05,
         SPEI, reg=NAME_1, dis=NAME_2)

geo10$y2_hhid <- ifelse(str_length(geo10$y2_hhid) < 16,
                        paste("0", geo10$y2_hhid, sep=""), geo10$y2_hhid)
geo10$reg <- toupper(geo10$reg)

# -------------------------------------
# read in household questionnaire to 
# get household characteristics

dataPath <- "C:/Users/Tomas/Documents/LEI/data/TZA"
HH10 <- read_dta(file.path(dataPath, "HH_SEC_B.dta")) %>%
  select(y2_hhid, indidy2, status=hh_b05, sex=hh_b02,
         yob=hh_b03_1, age=hh_b04, years=hh_b25)

HH10$years <- as.numeric(HH10$years)
HH10$years <- ifelse(HH10$years %in% 99, HH10$age, HH10$years)
HH10$status <- as_factor(HH10$status)
HH10$sex <- as_factor(HH10$sex)
HH10$yob <- as.integer(HH10$yob)

# make a new variable cage which splits individuals
# according to their age group with breaks at 15, 55
# and the max age

HH10$cage <- cut(HH10$age, breaks = c(0, 15, 55, max(HH10$age, na.rm=TRUE)),
                 labels=1:3, include.lowest = TRUE, right = TRUE)

# -------------------------------------
# education of household head and sum of
# education of all household members
# between the ages of 15 and 55

# 2010

ed <- read_dta(file.path(dataPath, "HH_SEC_C.dta")) %>%
  select(y2_hhid, indidy2, ed_any=hh_c03, start=hh_c04, end=hh_c08)

ed$ed_any <- as_factor(ed$ed_any)
ed$end <- as.integer(as.character(ed$end))
ed$end <- ifelse(ed$end %in% 9999, NA, ed$end)

# join with HH10 dataframe

HH10 <- left_join(HH10, ed)
HH10$education <- HH10$end - (HH10$yob + HH10$start)
HH10$education <- ifelse(HH10$ed_any %in% "No", 0, HH10$education)
HH10 <- select(HH10, -start, -end, -yob)
HH10$education <- ifelse(HH10$education < 0, NA, HH10$education)

# summarise the data 
HH10_x <- group_by(HH10, y2_hhid) %>% summarise(education1555=sum(education[cage %in% 2], na.rm=T), N1555=sum(cage %in% 2))
HH10 <- left_join(HH10, HH10_x); rm(HH10_x)
HH10 <- filter(HH10, status %in% "HEAD") %>% select(-indidy2, -status, -cage, -ed_any)

# -------------------------------------
# total number of vouchers received by
# the household at the plot level

dataPath <- "C:/Users/Tomas/Documents/LEI/data/TZA"
voucher10 <- read_dta(file.path(dataPath, "TZNPS2AGRDTA/AG_SEC3A.dta")) %>%
  select(y2_hhid, plotnum, vouch1=ag3a_48, vouch2=ag3a_55)
voucher10$vouch1 <- as_factor(voucher10$vouch1)
voucher10$vouch2 <- as_factor(voucher10$vouch2)
voucher10$vouch1 <- ifelse(voucher10$vouch1 %in% "YES", 1, 0)
voucher10$vouch2 <- ifelse(voucher10$vouch2 %in% "YES", 1, 0)
voucher10$vouchTotal <- voucher10$vouch1 + voucher10$vouch2
voucher10 <- group_by(voucher10, y2_hhid) %>%
  summarise(vouchTotal=sum(vouchTotal, na.rm=TRUE))

# binary outcome for voucher (1/0)
voucher10$vouchAny <- ifelse(voucher10$vouchTotal %in% 0, 0, 1)


# -------------------------------------
# join all the data 

endog2010 <- left_join(voucher10, HH10)
endog2010 <- left_join(endog2010, unique(geo10))
endog2010 <- left_join(endog2010, polInt2010[, c(1, 7:10)])
endog2010 <- left_join(endog2010, VTotal10)

# -------------------------------------
# kill of the islands which we do not use
# in the analysis

islands <- c("KASKAZINI-UNGUJA", "ZANZIBAR SOUTH AND CENTRAL",
             "ZANZIBAR WEST", "KASKAZINI-PEMBA", "KUSINI-PEMBA")

endog2010 <- endog2010[!endog2010$reg %in% islands,]
endog2010 <- rename(endog2010, hhid2010=y2_hhid)

# -------------------------------------
# write to a file to be used in analysis

setwd("c:/users/tomas/documents/lei")
write_dta(endog2010, "endog2010.dta")
