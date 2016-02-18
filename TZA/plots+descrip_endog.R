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
# tables 1: households that received any
# voucher

tab.households <- table(region=endog$region_name_lsms[drop=T], year=endog$surveyyear[drop=T])
tab.vouchAny <- xtabs(vouchAny ~ region_name_lsms + surveyyear, data=endog)

# percentage of people receiving a voucher

tab.vouch.house <- round(tab.vouchAny/tab.households*100, 2)

# all information together
cbind(tab.households, tab.vouchAny, tab.vouch.house)[, c(1, 3, 5, 2, 4, 6)]

# -------------------------------------
# tables 2: total number of vouchers
# received

tab.vouchTotal <- xtabs(vouchTotal ~ region_name_lsms + surveyyear, data=endog)

# average number of vouchers received per household
tab.vouchTotal.house <- round(tab.vouchTotal/tab.households, 2)

# proportional tables by year and region
tab.year.pc <- round(prop.table(tab.vouchTotal, 1)*100, 2)
tab.reg.pc <- round(prop.table(tab.vouchTotal, 2)*100, 2)

# -------------------------------------
# repeat above tables for zones

# -------------------------------------
# summary statistics for variables which
# will enter the final model


# -------------------------------------
# correlation between variables.


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
par(mfrow=c(1, 2))
plot(dist2town ~ factor(vouchAny), data=endog[endog$surveyyear %in% 2010,])
plot(dist2town ~ factor(vouchAny), data=endog[endog$surveyyear %in% 2012,])
plot(dist2market ~ factor(vouchAny), data=endog)

plot(factor(vouchAny) ~ dist2town, data=endog[endog$surveyyear %in% 2010,])
plot(factor(vouchAny) ~ dist2town, data=endog[endog$surveyyear %in% 2012,])

# normality check



library(ggplot2)

ggplot(endog) +
  geom_histogram(aes(log(asset), ..density..)) +
  facet_grid(~ surveyyear, scales = "free") +
  geom_density(aes(log(asset), ..density..))

ggplot(endog) +
  facet_grid(~ surveyyear, scales = "free") +
  geom_density(aes(log(asset), ..density..))

ggplot(endog) +
  facet_grid(~ surveyyear, scales = "free") +
  geom_density(aes(split_prez10, ..density..))

# try some component plus residual plots using the
# predictors for the first stage of the analysis
# to see what the distribution of the error term
# is, if it is normal -> probit, otherwise might
# be logit distributed.

# thinking in terms of a case and control group
# (voucher and no voucher)


