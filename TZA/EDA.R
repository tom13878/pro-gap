# exploratory analysis of the income data
# for all three waves of the TZA data

source("c:/users/tomas/documents/lei/pro-gap/TZA/income_TZA_2008.R")
source("c:/users/tomas/documents/lei/pro-gap/TZA/income_TZA_2010.R")
source("c:/users/tomas/documents/lei/pro-gap/TZA/income_TZA_2012.R")

# for analysis better to have 0's as NAs
income_2008_2 <- income_2008[2:8]; income_2008_2[income_2008_2==0] <- NA; income_2008_2 <- cbind(hhid=income_2008$hhid, income_2008_2)
income_2010_2 <- income_2010[2:9]; income_2010_2[income_2010_2==0] <- NA; income_2010_2 <- cbind(y2_hhid=income_2010$y2_hhid, income_2010_2)
income_2012_2 <- income_2012[2:9]; income_2012_2[income_2012_2==0] <- NA; income_2012_2 <- cbind(y3_hhid=income_2012$y3_hhid, income_2012_2)

# histogram of overall income in each year
par(mfrow=c(1, 3))
hist(log(income_2008_2$income+1), main="2008", xlab="log income (Tsh)", breaks=30, freq=FALSE, ylim=c(0, 0.25), xlim=c(0, 25))
box()
hist(log(income_2010_2$income+1), main="2010", xlab="log income (Tsh)", breaks=30, freq=FALSE, ylim=c(0, 0.25), xlim=c(0, 25))
box()
hist(log(income_2012_2$income+1), main="2012", xlab="log income (Tsh)", breaks=30, freq=FALSE, ylim=c(0, 0.25), xlim=c(0, 25))
box()


# correlation amongst types of income each year
cor(income_2008_2, use="pairwise.complete.obs")
cor(log(income_2008_2), use="pairwise.complete.obs")

cor(income_2010_2, use="pairwise.complete.obs")
cor(log(income_2010_2), use="pairwise.complete.obs")

cor(income_2012_2, use="pairwise.complete.obs")
cor(log(income_2012_2), use="pairwise.complete.obs")

# pairs plots
pairs(log(income_2008_2))
pairs(log(income_2010_2))
pairs(log(income_2012_2))

# look for regional variation in incomes

source("c:/users/tomas/documents/lei/pro-gap/TZA/location_TZA_2008.R")
source("c:/users/tomas/documents/lei/pro-gap/TZA/location_TZA_2010.R")
source("c:/users/tomas/documents/lei/pro-gap/TZA/location_TZA_2012.R")

income_2008_2 <- left_join(location_2008, income_2008_2)

by_region <- group_by(income_2008_2, REGNAME) %>%
  summarise_if(is.numeric, function(x) mean(x, na.rm=T))
by_region <- select(by_region, -REGCODE, -DISCODE)
               