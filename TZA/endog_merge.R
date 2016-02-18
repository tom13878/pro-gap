# -------------------------------------
# Endogeneity analysis
# -------------------------------------

library(dplyr)
library(haven)

# -------------------------------------
# read in the two dataframes for 2010 
# and 2012

setwd("c:/users/tomas/documents/lei")
endog2010 <- readRDS("endog2010.rds")
endog2012 <- readRDS("endog2012.rds")

# ------------------------------------
# unfortunately the dist2Rd variable does
# not seem to be available for 2010 in 
# the geo variables file, remove for now

endog2012$dist2Rd <- NULL

# -------------------------------------
# remove the y3_hhid and rearrange variables
# for the 2012 data to match up with 2010
# and remove reg2012 which is no longer
# needed

endog2012$y3_hhid <- endog2012$reg2012 <- NULL 
endog2012 <- endog2012[, c(21, 1:20, 22:23)]

# -------------------------------------
# make some year factor variables

endog2010$surveyyear <- 2010 # factor(1, levels=1:2, labels=c("2010", "2012"))
endog2012$surveyyear <- 2012 # factor(2, levels=1:2, labels=c("2010", "2012"))

# -------------------------------------
# join each endog database with the 
# corresponding final data sample

setwd("c:/users/tomas/downloads")
db0 <- readRDS("db0.rds")
endog2010 <- inner_join(db0, endog2010)

# only need 'Original' households for 2012
endog2012 <- endog2012[endog2012$hhtype %in% "Original", ]
endog2012 <- inner_join(db0, endog2012)

# kill variables no longer needed
endog2012$hhid2012 <- endog2012$hhtype <- endog2012$hhloc <- NULL

# -------------------------------------
# join the data together

endog2010 <- endog2010[, -c(45:47)]

endog <- rbind(endog2010, endog2012) 
endog$sex <- as_factor(endog$sex)
endog$cage <- as_factor(endog$cage)
endog$surveyyear <- factor(endog$surveyyear)

setwd("c:/users/tomas/documents/lei")
saveRDS(endog, "endog.rds")
