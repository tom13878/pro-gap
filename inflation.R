# -------------------------------------
# get inflation information from the
# world bank via the WDI package
# -------------------------------------

dataPath <- "c:/users/tomas/documents/lei/pro-gap"

# install.packages("WDI")
library(WDI)

inflation <- WDIsearch("inflation")

countries <- c("NG", "TZ", "ET", "GH", "MW", "UG")

inflation <- WDI(country=countries, indicator="FP.CPI.TOTL.ZG", start=2008, end=2014)

# change variable names

names(inflation) <- c("code", "name", "inflation", "year")
inflation <- inflation[, c("code", "name", "year", "inflation")]

# write to a csv file
write.csv(inflation, file.path(dataPath, "inflation.csv"), row.names=FALSE)
