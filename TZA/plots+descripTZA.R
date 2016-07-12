# -------------------------------------
# plots and descriptive statistics
# tanzania data
# -------------------------------------

dataPath <- "C:/Users/Tomas/Documents/LEI"

library(ggplot2)

# read in the preprocessed data from the file called
# data_combine.R. This file binds all the data together
# and also contains individual datasets for each of the
# three waves

source(file.path(dataPath, "pro-gap/TZA/data_combine.R"))
# ls()
# fullData -> identifiers for each wave (hhid2008, hhid2010, hhid2012)
# fullData2 -> only one identifier hhid

# -------------------------------------
# two key variables, yield and N per Ha
# just looking at maize plots
# -------------------------------------

maize <- filter(fullData, status == "HEAD", zaocode == 11)

# univariate 
ggplot(maize, aes(x = N)) + geom_histogram() + facet_wrap( ~ surveyyear)

# obvious that vast majority do not use nitrogen
# fertilizer
maize$N <- ifelse(maize$N %in% 0, NA, maize$N)
ggplot(maize, aes(x = N)) + geom_histogram() + facet_wrap( ~ surveyyear, scales="free")

# clear evidence of outliers - try removing extreme
# values one at a time

removeOutly <- function(data, var="N", splitvars=c("surveyyear", "REGNAME"), n=5){
  for (i in 1:n){
    rows <- as.integer(row.names(data))
    x <- as.list(data)[[var]]
    splits <- data[, splitvars]
    split1 <- split(x, splits)
    split2 <- split(rows, splits)
    maxim <- sapply(split1, which.max)
    good <- sapply(maxim, function(x) !length(x) == 0)
    maxim <- maxim[good]
    split2 <- split2[good]
    loc <- sapply(1:length(maxim), function(x) split2[[x]][maxim[[x]]])
    data <- data[-loc,]
  }
  data
}

test <- removeOutly(maize, n=2)
test <- subset(test, !REGCODE > 21)
ggplot(test, aes(x = N)) + geom_histogram() + facet_wrap( ~ surveyyear + REGNAME, scales="free")


# winsor the outliers
source(file.path(dataPath, "functions/winsor.R"))
maize$N <- winsor(maize$N)
ggplot(maize, aes(x = N)) + geom_histogram() + facet_wrap( ~ surveyyear + REGNAME)


# ----------------------------------