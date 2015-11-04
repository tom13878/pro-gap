# -------------------------------------
#' predictive mean matching to get over 
#' The problem of missing area values 
#' for some of the area measurements
#' -----------------------------------

# install.packages('mice')
# install.packages('VIM')

library(mice)
library(VIM)
library(lattice)
library(ggplot2)
library(dplyr)

# use the comp dataset prepared earlier
comp <- read.csv("C:/Users/Tomas/Documents/LEI/comp.csv")

# kill of household id and plotnum. These can't go into the 
# imputation
row.names(comp) <- paste(comp$y3_hhid, comp$plotnum, sep="")
comp$y3_hhid <- comp$plotnum <-  NULL

# this function gives an overview of the missingness in the data
# read as there are 5396 where we have complete values
# 2051 where we are missing just area.gps and so on.
md.pattern(comp)

# this function returns a list with counts of how many observations
# are available when pairs of variables are: there, not there, one
# or another of them is available.
md.pairs(comp)

# this plot helps us to identify the missingness in the data.
# blue dots are for values that we have both variables
# red dots are where we have missing values for both variables
# in the case below the red dots are represent values that are
# missing for area.gps but are available for area.est (since 
# all the missing area.est ones have already been removed)
marginplot(comp[, c("area.est", "area.gps")], col=c('blue', 'red', 'orange'))


# MICE imputation method
imp1 <- mice(comp, m=50)

# calling head reveals some of the values in the dataset
# for each of the five imputations. Clearly need to clean
# up some of the values here. 
# area_gps_imputed <- imp1$imp$area.gps
# area_gps_imputed <- rowSums(area_gps_imputed)/50

# use the complete function to combine the data with the
# original data. this returns six 'separate' dataframes
# where the missing values in the last five have been 
# replaced with the values of the corresponding imputation
imp_data <- complete(imp1)
areas <- select(comp, area.gps, area.est)
areas$gps_imputed <- imp_data$area.gps


# get household ID and plotnumber back from row.names
split_on_M <-strsplit(row.names(areas), "M")
y3_hhid <- sapply(split_on_M, function(elt) return(elt[1]))
plotnum <- sapply(split_on_M, function(elt) return(paste('M', elt[2], sep="")))

# whitespace snuck in somehow!!!
plotnum <- gsub(" ", "", plotnum)

# add y3_hhid and plotnum
# kill rownames
areas$y3_hhid <- y3_hhid
areas$plotnum <- plotnum
rownames(areas) <- NULL

# reshuffle variables 
areas <- select(areas, y3_hhid, plotnum, everything())

# save areas to a file
write.csv(areas, "C:/Users/Tomas/Documents/LEI/areas_w3.csv", row.names=FALSE)
