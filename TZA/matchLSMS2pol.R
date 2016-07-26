# join the data with the political infomration for
# each district

library(dplyr)
library(haven)

path <- "C:/users/tomas/documents/LEI/pro-gap/"
source(file.path(path, "TZA/data_combineTZA.R"))

path <- "C:/users/tomas/documents/LEI/pol/"
source(file.path(path, "code/pol20104analysis.R"))
key <- read.csv(file.path(path, "data/link_files/lsms2pol.csv")) %>%
  rename(REGNAME=REGNAME_LSMS, DISNAME=DISNAME_LSMS) %>% select(-NOTE)

maize <- filter(fullData1012, status=="HEAD", crop_code==11);rm(fullData1012, fullData081012)

maize <- left_join(maize, key)
prez2010 <- rename(prez2010, REGNAME_POL=reg, DISNAME_POL=dis)
maize <- left_join(maize, prez2010)

maize  <- maize[maize$REGCODE<=21,]

maize$REGNAME_POL <- NULL
maize$DISNAME_POL <- NULL

rm(path, key, prez2010)

# # a few plots
# with(maize, plot(split_prez10[N>0& N < 200], N[N >0 & N < 200]))
# # categorize the support for CCM
# csplit <- cut(maize$split_prez10, breaks=c(0, 25, 50, 75, 100),
#               labels=c("0-25", "25-50", "50-75", "75-100"),
#               include.lowest = TRUE, right = FALSE)
# 
# plot(csplit[maize$N>0 & maize$N<200], maize$N[maize$N>0 & maize$N<200])
# 
# par(mfrow=c(1, 2))
# with(maize, {
#   hist(area_tot, breaks="FD", freq=FALSE, ylab="Density")
#   lines(density(area_tot), lwd=2)
#   lines(density(area_tot, adjust=0.5), lwd=1)
#   rug(area_tot)
#   box()
# })
# with(GHA2010, {
#   hist(log(yld), breaks="FD", freq=FALSE, ylab="Density")
#   lines(density(log(yld)), lwd=2)
#   lines(density(log(yld), adjust=0.5), lwd=1)
#   rug(log(yld))
#   box()
# })


lm1 <- lm(N ~ split_prez10 + log(asset) + log(area_tot) + SACCO +
             factor(surveyyear) + factor(ZONE) + education + education1555 +
             rural + sex + age + years + dist_road + dist_popcenter +
             dist_market, dat=maize)

# try a tobit model
# library(AER)
modl.tobit <- tobit(N ~ split_prez10 + log(asset) + log(area_tot) + SACCO +
        factor(surveyyear) + factor(ZONE) + education + education1555 +
        rural + sex + age + years + dist_road + dist_popcenter +
        dist_market, dat=maize)


sigma <- modl.tobit$scale
X <- model.matrix(modl.tobit)
n <- nrow(X)
scale_factor <- (1/n) * sum(pnorm(((X %*% coef(modl.tobit)))/sigma, 0, 1))
APE1 <- coef(modl.tobit) * scale_factor

library(stargazer)

# initial result suggests that an increase in 
# 1 percentage point of split leads to an 
# increase of almost 1 kg of nitrogen.
stargazer(lm1, modl.tobit, type = "text",
          coef = list(coef(lm1), APE1), p.auto=FALSE,
          intercept.bottom = FALSE,
          digits = 2, digits.extra = 2)

# get the residuals
# r2 <- residuals(modl.tobit, type="deviance")

# clearly some outliers, check leverages too
# hist(r2)
