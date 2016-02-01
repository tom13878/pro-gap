# -------------------------------------
# Descriptive statistics and plots of
# endogeneity data

# --------------------------------------
# read in the data

setwd("c:/users/tomas/documents/lei")
endog <- readRDS("endog.rds")

# make factor variables
endog$region_name_lsms <- factor(endog$region_name_lsms)
endog$surveyyear <- factor(endog$surveyyear)

# -------------------------------------
# start by trying to find out how many
# farmers received a voucher by region
# each year

tab.households <- table(region=endog$region_name_lsms[drop=T], year=endog$surveyyear[drop=T])
tab.vouchAny <- xtabs(vouchAny ~ region_name_lsms + surveyyear, data=endog)

# this table shows the percentage of households
# who did receive a voucher by region and year

round(tab.vouchAny/tab.households*100, 2)

# -------------------------------------
# find out total number of vouchers 
# received each year and region

tab.vouchTotal <- xtabs(vouchTotal ~ region_name_lsms + surveyyear, data=endog)

# tables to show who those vouchers went to
# each year and region

tab.year.pc <- round(prop.table(tab.vouchTotal, 1)*100, 2)
tab.reg.pc <- round(prop.table(tab.vouchTotal, 2)*100, 2)

# -------------------------------------
# repeat above tables for zones


# -------------------------------------
# plots
# 1. boxplots of those who received a 
# voucher and those who did not
# 2. check for normality of linear
# predictors

library(ggplot2)

# boxplots

ggplot(endog, aes(x=factor(vouchAny), y=log(asset))) +
  geom_boxplot() +
  facet_grid(~surveyyear)
 
ggplot(endog, aes(x=factor(sex), y=vouchTotal)) +
  geom_boxplot() +
  facet_grid(~surveyyear) 

ggplot(endog, aes(x=factor(zone), y=vouchTotal)) +
  geom_boxplot() +
  facet_wrap(~surveyyear) 

# need to facet these plots
plot(dist2town ~ factor(vouchAny), data=endog)
plot(dist2market ~ factor(vouchAny), data=endog)

# normality check

library(ggplot2)

ggplot(endog) +
  geom_histogram(aes(log(asset), ..density..)) +
  facet_grid(~ surveyyear, scales = "free") +
  geom_density(aes(log(asset), ..density..))

ggplot(endog) +
  facet_grid(~ surveyyear, scales = "free") +
  geom_density(aes(log(asset), ..density..))

# try some component plus residual plots using the
# predictors for the first stage of the analysis
# to see what the distribution of the error term
# is, if it is normal -> probit, otherwise might
# be logit distributed.
