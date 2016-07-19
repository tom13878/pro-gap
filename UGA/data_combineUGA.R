# -------------------------------------
# creating a panel dataset and a
# balanced panel dataset with the waves
# of the UGA data (three waves)
# -------------------------------------

if(Sys.info()["user"] == "Tomas"){
  path <- "C:/Users/Tomas/Documents/LEI/pro-gap/UGA"
} else {
  path <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/Code/UGA/"
}

library(dplyr)

options(scipen=999)

# get all three waves, the output of the UGA_****.R script files
suppressMessages(source(file.path(path, "UGA_2009.R")))
suppressMessages(source(file.path(path, "UGA_2010.R")))
suppressMessages(source(file.path(path, "UGA_2011.R")))

# -------------------------------------
# example: select only maize farmers:
# filter on household head and
# crop_code = 130 (maize)
# -------------------------------------

# 2009
# maize09 <- UGA2009
# maize09 <- filter(maize09, status %in% "HEAD", crop_code %in% 130)
# 
# # 2010
# maize10 <- UGA2010
# maize10 <- filter(maize10, status %in% "HEAD", crop_code %in% 130)
# 
# # 2011
# maize11 <- UGA2011
# maize11 <- filter(maize11, status %in% "HEAD", crop_code %in% 130)

# -------------------------------------
# unlike TZA data there is no need to
# use a panel key to link households
# and individuals
# -------------------------------------

# hhid2009 <- unique(UGA2009$hhid2009)
# hhid2010 <- unique(UGA2010$hhid2010)
# hhid2011 <- unique(UGA2011$hhid2011)
# table(hhid2009 %in% hhid2010)
# table(hhid2009 %in% hhid2011)

# so all we do is change the names of the variables
# to a standard name across all years
UGA2009 <- rename(UGA2009, hhid=hhid2009, indidy=indidy1)
UGA2010 <- rename(UGA2010, hhid=hhid2010, indidy=indidy2)
UGA2011 <- rename(UGA2011, hhid=hhid2011, indidy=indidy3)

# -------------------------------------
# Some waves of the data have variables
# that were not available in others.
# -------------------------------------

# get all name variables that are common to the three waves
good <- Reduce(intersect, list(names(UGA2009), names(UGA2010), names(UGA2011)))

# select only those names common in all three waves
UGA2009_2 <- UGA2009[, good]
UGA2010_2 <- UGA2010[, good]
UGA2011_2 <- UGA2011[, good]

# new full dataset
fullData <- rbind(UGA2009_2, UGA2010_2, UGA2011_2) %>%
  select(hhid, indidy, everything())

rm(good, path, UGA2009, UGA2009_2, UGA2010, UGA2010_2, UGA2011, UGA2011_2)
