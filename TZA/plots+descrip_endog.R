# -------------------------------------
# Descriptive statistics and plots of
# endogeneity data

# --------------------------------------
# read in the data
setwd("c:/users/tomas/documents/lei")
endog <- readRDS("endog.rds")

# table by region
tab1 <- xtabs(vouchTotal ~ factor(region_name_lsms) + factor(surveyyear), data=endog)
tab2 <- xtabs(vouchAny ~ factor(region_name_lsms) + factor(surveyyear), data=endog)
tab3 <- table(region=factor(endog$region_name_lsms), year=factor(endog$surveyyear))

year_marg <- apply(tab3, 1, sum)
reg_marg <- apply(tab3, 2, sum)

round(tab1/year_marg, 2)
round(tab1/reg_marg, 2)

# table by zone

tab1 <- xtabs(vouchTotal ~ factor(zone) + factor(surveyyear), data=endog)
tab2 <- xtabs(vouchAny ~ factor(zone) + factor(surveyyear), data=endog)
tab3 <- table(region=factor(endog$zone), year=factor(endog$surveyyear))

year_marg <- apply(tab3, 1, sum)
zone_marg <- apply(tab3, 2, sum)

round(tab1/year_marg, 2)
round(tab1/zone_marg, 2)

# -------------------------------------

plot(asset ~ factor(vouchAny), data=endog[endog$asset < median(endog$asset), ])
plot(vouchTotal ~ factor(sex), data=endog[endog$vouchTotal > 0,])
plot(vouchTotal ~ factor(zone), data=endog)
