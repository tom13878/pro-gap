# -------------------------------------
# creating a panel dataset and a
# balanced panel dataset with the waves
# of the ETH data (two waves)
# -------------------------------------

if(Sys.info()["user"] == "Tomas"){
  path <- "C:/Users/Tomas/Documents/LEI/pro-gap/ETH"
} else {
  path <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/Code/ETH/"
}

library(dplyr)

options(scipen=999)

# get all three waves, the output of the UGA_****.R script files
suppressMessages(source(file.path(path, "ETH_2011PP.R")))
suppressMessages(source(file.path(path, "ETH_2013PP.R")))

# -------------------------------------
# example: select only maize farmers:
# filter on household head and
# crop_code = 2 (maize)
# -------------------------------------

# 2011
# maize11 <- ETH2011
# maize11 <- filter(maize11, status %in% "HEAD", crop_code %in% 2)
# 
# # 2013
# maize13 <- ETH2013
# maize13 <- filter(maize13, status %in% "HEAD", crop_code %in% 2)

# -------------------------------------
# unlike TZA data there is no need to
# use a panel key to link households
# and individuals
# -------------------------------------

# in the second wave, the household
# identification number of the first
# wave is used. However these are recorded
# as an empty character string if the 
# household entered the survey in wave
# two. ("")
table(ETH2013$household_id %in% "")
ETH2013$household_id <- zap_empty(ETH2013$household_id)
table(is.na(ETH2013$household_id))

# the same is true of the individual id
table(ETH2013$individual_id %in% "")
ETH2013$individual_id <- zap_empty(ETH2013$individual_id )
table(is.na(ETH2013$individual_id))

# use the first wave household identification
# number. Where this is missing use the
# second wave household identification number
ETH2013$household_id <- ifelse(is.na(ETH2013$household_id), ETH2013$household_id2, ETH2013$household_id)
ETH2013$individual_id <- ifelse(is.na(ETH2013$individual_id), ETH2013$individual_id2, ETH2013$individual_id)

# -------------------------------------
# Some waves of the data have variables
# that were not available in others.
# -------------------------------------

# get all name variables that are common to both waves
good <- Reduce(intersect, list(names(ETH2011), names(ETH2013)))

# select only those names common in both waves
ETH2011_2 <- ETH2011[, good]
ETH2013_2 <- ETH2013[, good]

# new full dataset
fullData <- rbind(ETH2011_2,ETH2013_2) %>%
  select(hhid=household_id, indidy=individual_id, everything())

rm(good, path, ETH2011, ETH2011_2, ETH2013, ETH2013_2)

# maize = filter(fullData, status %in% "HEAD", crop_code==2)
