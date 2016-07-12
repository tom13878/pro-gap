# -------------------------------------
# creating a panel dataset and a
# balanced panel dataset with the waves
# of the TZA data. Do this for all three
# waves of the data combined, and also
# just for the last two waves.
#
# There are two possibilities
# farmers can split into differnt farms
# farmers from different farms can merge
# into one farm. IN both ways there will
# be duplicates in at least one of the
# three waves. Whether that is a problem
# depends on the type of analysis you
# choose. Mainly we do not find that it
# is a problem.
# -------------------------------------


if(Sys.info()["user"] == "Tomas"){
  path <- "C:/Users/Tomas/Documents/LEI/pro-gap/TZA"
} else {
  path <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/Code/TZA/"
}

# load packages
library(dplyr)

options(scipen=999)

# get all three waves, the output of the TZA_***.R script files
source(file.path(path, "/TZA_2008PP.R"))
source(file.path(path, "/TZA_2010PP.R"))
source(file.path(path, "/TZA_2012PP.R"))

# -------------------------------------
# Some waves of the data have variables
# that were not available in others.
# for example in 2008 fewer questions
# were asked about education
# -------------------------------------

# get all name variables that are common to the three waves
good <- Reduce(intersect, list(names(TZA2008), names(TZA2010), names(TZA2012)))

# get all name variables that are common to the two waves
good2 <- Reduce(intersect, list(names(TZA2010), names(TZA2012)))

# select only those names common in all three waves
TZA2008_2 <- TZA2008[, good]
TZA2010_2 <- TZA2010[, good]
TZA2012_2 <- TZA2012[, good]

# select only those names common in the last two waves
TZA2010_2.2 <- TZA2010[, good2]
TZA2012_2.2 <- TZA2012[, good2]

# full dataset for all three waves
fullData081012 <- rbind(TZA2008_2, TZA2010_2, TZA2012_2) %>%
  select(hhid2008, indidy1, hhid2010, indidy2, hhid2012, indidy3, everything())

# full dataset for last two waves
fullData1012 <- rbind(TZA2010_2.2, TZA2012_2.2) %>%
  select(hhid2010, indidy2, hhid2012, indidy3, everything())

# -------------------------------------
# for analysis like random effects we 
# may want every household to have a
# unique household id through time.
# And for RE class models it does not
# matter if the data is balanced. 
# -------------------------------------

# use the final year household id
fullData081012$hhid2012 <- ifelse(is.na(fullData081012$hhid2012), fullData081012$hhid2010, fullData081012$hhid2012)
fullData081012$hhid2012 <- ifelse(is.na(fullData081012$hhid2012), fullData081012$hhid2008, fullData081012$hhid2012)

# use the final year individual identification number

fullData081012$indidy3 <- ifelse(is.na(fullData081012$indidy3), fullData081012$indidy2, fullData081012$indidy3)
fullData081012$indidy3 <- ifelse(is.na(fullData081012$indidy3), fullData081012$indidy1, fullData081012$indidy3)

# remove household id and individual ids for the first two waves
fullData081012$hhid2008 <- fullData081012$hhid2010 <- NULL
fullData081012$indidy1 <- fullData081012$indidy2 <- NULL
fullData081012 <- rename(fullData081012, hhid = hhid2012, indidy=indidy3)

# the same process can be used to create 
# a panel for only the last two waves

# use the final year household id
fullData1012$hhid2012 <- ifelse(is.na(fullData1012$hhid2012), fullData1012$hhid2010, fullData1012$hhid2012)

# use the final year individual identification number
fullData1012$indidy3 <- ifelse(is.na(fullData1012$indidy3), fullData1012$indidy2, fullData1012$indidy3)

# remove household id and individual ids for the first two waves
fullData1012$hhid2010 <- NULL
fullData1012$indidy2 <- NULL
fullData1012 <- rename(fullData1012, hhid = hhid2012, indidy=indidy3)

rm("path", "good", "good2", "TZA2008", "TZA2008_2", "TZA2010",
   "TZA2010_2", "TZA2010_2.2", "TZA2012", "TZA2012_2", "TZA2012_2.2")
