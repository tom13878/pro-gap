# -------------------------------------
# creating a panel dataset and a
# balanced panel dataset with the waves
# of the TZA data
# -------------------------------------

dataPath <- "C:/Users/Tomas/Documents/LEI/"
library(dplyr)

options(scipen=999)

# get all three waves, the output of the TZA_***.R script files
TZA2008 <- readRDS(file.path(dataPath, "data/TZA/TZA2008.rds"))
TZA2010 <- readRDS(file.path(dataPath, "data/TZA/TZA2010.rds"))
TZA2012 <- readRDS(file.path(dataPath, "data/TZA/TZA2012.rds"))

# -------------------------------------
# example: select only maize farmers:
# filter on household head and
# zaocode = 11 (maize)
# -------------------------------------

# 2008
# maize08 <- TZA2008
# maize08 <- filter(maize08, status %in% "HEAD", zaocode %in% 11)
# 
# # 2010
# maize10 <- TZA2010
# maize10 <- filter(maize10, status %in% "HEAD", zaocode %in% 11)
# 
# # 2012
# maize12 <- TZA2012
# maize12 <- filter(maize12, status %in% "HEAD", zaocode %in% 11)

# -------------------------------------
# Some waves of the data have variables
# that were not available in others.
# for example in 2008 fewer questions
# were asked about education
# -------------------------------------

# get all name variables that are common to the three waves
good <- Reduce(intersect, list(names(TZA2008), names(TZA2010), names(TZA2012)))

# select only those names common in all three waves
TZA2008_2 <- TZA2008[, good]
TZA2010_2 <- TZA2010[, good]
TZA2012_2 <- TZA2012[, good]

# new full dataset
fullData <- rbind(TZA2008_2, TZA2010_2, TZA2012_2) %>%
  select(hhid2008, indidy1, hhid2010, indidy2, hhid2012, indidy3, everything())

# -------------------------------------
# for analysis like random effects we 
# may want every household to have a
# unique household id through time.
# And for RE class models it does not
# matter if the data is balanced. 
# -------------------------------------

# use the final year household id
fullData2 <- fullData
fullData2$hhid2012 <- ifelse(is.na(fullData2$hhid2012), fullData2$hhid2010, fullData2$hhid2012)
fullData2$hhid2012 <- ifelse(is.na(fullData2$hhid2012), fullData2$hhid2008, fullData2$hhid2012)

# use the final year individual identification number

fullData2$indidy3 <- ifelse(is.na(fullData2$indidy3), fullData2$indidy2, fullData2$indidy3)
fullData2$indidy3 <- ifelse(is.na(fullData2$indidy3), fullData2$indidy1, fullData2$indidy3)

# remove household id and individual ids for the first two waves
fullData2$hhid2008 <- fullData2$hhid2010 <- NULL
fullData2$indidy1 <- fullData2$indidy2 <- NULL
fullData2 <- rename(fullData2, hhid = hhid2012, indidy=indidy3)

# -------------------------------------
# alternativley we may want a balanced 
# panel, with only those households that
# appear in every wave of the survey. 
# This is more difficult because of data
# attrition, and because some households
# moved or split from their original
# family. For example, a child of a farmer
# leaving to start their own farm.
# -------------------------------------

rm(list=ls()[!ls() %in% c("fullData", "fullData2", "dataPath")])
