# -------------------------------------
#' predictive mean matching to get over 
#' The problem of missing area values in
#' R
#' -----------------------------------

# install.packages('mice')
# install.packages('VIM')

library(mice)
library(VIM)
library(lattice)
library(ggplot2)

# use the comp dataset prepared earlier
comp <- read.csv("C:/Users/Tomas/Documents/Work/LEI/data/TZA/comp.csv")
row.names(comp) <- comp$X
#kill of hhid and plotnum
comp$y3_hhid <- comp$plotnum <- comp$X<- NULL

# this function gives an overview of the missingness in the data
# read as there are 5396 where we have complete values
# 2051 where we are missing just area.gps and so on.
# md.pattern(comp)

# this function returns a list with counts of how many observations
# are available when pairs of variables are: there, not there, one
# or another of them is available.
# md.pairs(comp)

# this plot helps us to identify the missingness in the data.
# blue dots are for values that we have both variables
# red dots are where we have missing values for both variables
# in the case below the red dots are represent values that are
# missing for area.gps but are available for area.est (since 
# all the missing area.est ones have already been removed)
# marginplot(comp[, 1:2], col=c('blue', 'red', 'orange'))


# MICE imputation method
imp1 <- mice(comp, m=50)

# calling head reveals some of the values in the dataset
# for each of the five imputations. Clearly need to clean
# up some of the values here. 
area_gps_imputed <- imp1$imp$area.gps
area_gps_imputed <- rowSums(area_gps_imputed)/50

# use the complete function to combine the data with the
# original data. this returns six 'separate' dataframes
# where the missing values in the last five have been 
# replaced with the values of the corresponding imputation


# where there are missing values in the comp data replace with imputed data
#areas <- select(comp, area_gps=area.gps, area_est=area.est)
areas$area_gps_imputed <- ifelse(is.na(areas$area_gps), area_gps_imputed, areas$area_gps)
var(areas$area_gps_imputed)

# bind together areas with original comp file
comp <- read.csv("C:/Users/Tomas/Documents/Work/LEI/data/TZA/comp.csv")
comp <- cbind(comp, areas)
comp <- select(comp, y3_hhid, plotnum, area_est, area_gps, area_gps_imputed)
# 
# # split on the M in plot number and then
# # add it back in later
# split_on_M <-strsplit(row.names(areas), "M")
# y3_hhid <- sapply(split_on_M, function(elt) return(elt[1]))
# plotnum <- sapply(split_on_M, function(elt) return(paste('M', elt[2], sep="")))
# 
# # whitespace snuck in somehow!!!
# plotnum <- gsub(" ", "", plotnum)
# 
# # combine everything together
# areas$y3_hhid <- y3_hhid
# areas$plotnum <- plotnum
# 
# # reshuffle and remove row names
# row.names(areas) <- NULL
# areas <- select(areas, y3_hhid, plotnum, everything())

# save areas to a file
write.csv(comp, "C:/Users/Tomas/Documents/Work/LEI/data/TZA/areas_w3.csv", row.names=FALSE)
