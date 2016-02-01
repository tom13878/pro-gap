# -------------------------------------
# endogeneity analysis
# -------------------------------------

options(scipen=999)

# -------------------------------------
# read in the prepared data and select
# only complete rows

setwd("c:/users/tomas/documents/lei")
endog <- readRDS("endog.rds")
endog <- endog[complete.cases(endog),]

# -------------------------------------
# first stage model - logit/probit model
# (1/0) if farmer received a voucher or not

modl.logit <- glm(cbind(vouchAny, 1-vouchAny) ~ surveyyear + ccm_prez10*split_prez10 +
              log(vtot) + log(households) + dist2HQ + dist2market + dist2town +
              years + log(asset) + age + education + education1555 + N1555,
            family=binomial, data=endog)

modl.probit <- glm(cbind(vouchAny, 1-vouchAny) ~ surveyyear + ccm_prez10*split_prez10 +
                     log(vtot) + log(households) + dist2HQ + dist2market + dist2town +
                     years + log(asset) + age + education + education1555 + N1555,
                   family=binomial(link=probit), data=endog)

# test for goodness of fit on the logit model
predicted <- as.numeric(
  predict.glm(modl.logit, type="response")>.5)
true <- endog$vouchAny
correct <- as.numeric(predicted == true)
100*table(correct)/sum(table(correct)) # model is correct 87% of the time

# test for goodness of fit on the probit model
predicted <- as.numeric(
  predict.glm(modl.probit, type="response")>.5)
true <- endog$vouchAny
correct <- as.numeric(predicted == true)
100*table(correct)/sum(table(correct))

# extract residuals from probit
r1 <- residuals(modl.probit, type="pearson")

# -------------------------------------
# second stage model - HH chars and plot chars
library(AER)

# include soil variables. And other variables leading to higher demand for fertilizer.

modl2 <- tobit(N ~ dist2HQ + dist2market + dist2town + sex + age + years + rural +
                 education + education1555 + N1555 + log(asset) + zone + surveyyear +
                 legume + hh_slope + r1
               , data=endog)

r2 <- residuals(modl2, type="pearson")

# bootstrap se

coeftest(modl2, df = Inf, vcov = vcovHC(modl2, type = "HC1"))

cov1        <- vcovHC(modl2, type = "HC1")
robust.se1   <- sqrt(diag(cov1))

stargazer(modl, type = "text",
          se = list(NULL, robust.se1), intercept.bottom = FALSE, digits = 2, digits.extra = 2)

# -------------------------------------
# third stage model - example only
# get real version from Michiel

modl3 <- lm(yld ~ N + I(N^2) + dist2HQ + dist2market + dist2town + sex + age + years + rural +
              education + education1555 + log(asset) + zone + surveyyear + legume +
              hh_slope + r2,
            data=endog)

# bootstrap se
library(stargazer)
coeftest(modl3, df = Inf, vcov = vcovHC(modl3, type = "HC1"))

cov1        <- vcovHC(modl3, type = "HC1")
robust.se1   <- sqrt(diag(cov1))

stargazer(modl3, type = "text",
          se = list(NULL, robust.se1), intercept.bottom = FALSE, digits = 2, digits.extra = 2)
