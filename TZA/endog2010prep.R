# -------------------------------------
# prepare datframe for endogeneity 2010

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
# distributed to each region

dataPath <- "C:/Users/Tomas/Documents/LEI/pol/data"

VTotal10 <- read.csv(paste(dataPath, "voucher201011.csv", sep="/"))

VTotal10$Region <- toupper(VTotal10$Region)
names(VTotal10) <- c("reg", "households", "Vtot1", "Vtot2")
VTotal10$reg <- gsub(" ", "", VTotal10$reg)

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
  select(y2_hhid, indidy2, start=hh_c04, end=hh_c08)

ed$end <- as.integer(as.character(ed$end))
ed$end <- ifelse(ed$end %in% 9999, NA, ed$end)

# join with HH10 dataframe
HH10 <- left_join(HH10, ed)
HH10$educ <- HH10$end - (HH10$yob + HH10$start)
HH10 <- select(HH10, -start, -end, -yob)
HH10$educ <- ifelse(HH10$educ < 0, NA, HH10$educ)

# summarise the data 
HH10_x <- group_by(HH10, y2_hhid) %>% summarise(educ1555=sum(educ[cage %in% 2], na.rm=T))
HH10 <- left_join(HH10, HH10_x); rm(HH10_x)
HH10 <- filter(HH10, status %in% "HEAD") %>% select(-indidy2, -status)

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

# -------------------------------------
# join data and look at first stage using
# the 2010 data

data <- left_join(voucher10, HH10)
data <- left_join(data, geo10)
data <- left_join(data, polInt2010[, c(1, 7:10)])
data <- left_join(data, VTotal10)

islands <- c("KASKAZINI-UNGUJA", "ZANZIBAR SOUTH AND CENTRAL",
             "ZANZIBAR WEST", "KASKAZINI-PEMBA", "KUSINI-PEMBA")

data <- data[!data$reg %in% islands,]

# try a binomial model

modl <- glm(cbind(vouchTotal, 2-vouchTotal) ~ sex + dist2town +
              dist2market + dist2HQ + ccm_prez10 + split_prez10 + vtot + SPEI +
              age*years + households + I(age^2),
            family=binomial, data=data)
