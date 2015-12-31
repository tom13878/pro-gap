# -------------------------------------
# get inflation information from the
# world bank via the WDI package
# -------------------------------------

setwd("c:/users/tomas/documents/lei/pro-gap")

# install.packages("WDI")
library(WDI)

inflation <- WDIsearch("inflation")

countries <- c("NG", "TZ", "ET", "GH", "MW")

inflation <- WDI(country=countries, indicator="FP.CPI.TOTL.ZG", start=2008, end=2014)

# change variable names

names(inflation) <- c("code", "name", "inflation", "year")
inflation <- inflation[, c("code", "name", "year", "inflation")]

# probably don't need the code variable
inflation$code <- NULL

# write to a csv file
write.csv(inflation, "inflation.csv", row.names=FALSE)
