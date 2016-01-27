# -------------------------------------
# endogeneity analysis
# -------------------------------------

options(scipen=999)

# -------------------------------------
# read in the prepared data
setwd("c:/users/tomas/documents/lei")
endog <- readRDS("endog.rds")

# -------------------------------------
# first stage model

modl <- glm(cbind(vouchAny, 1-vouchAny) ~ surveyyear + ccm_prez10*split_prez10 +
              offset(log(vtot)) + log(households) + dist2HQ + dist2market + dist2town +
              years + log(asset) + age + educ1555,
            family=binomial, data=endog)
r1 <- residuals(modl)

# -------------------------------------
# second stage model - HH chars and plot chars
library(AER)

modl2 <- tobit(N ~ dist2HQ + dist2market + dist2town + sex + age + years + rural +
                 educ + educ1555 + log(asset) + zone + surveyyear + legume + hh_slope + r1
               , data=endog2)

r2 <- residuals(modl2)

# bootstrap se

coeftest(modl2, df = Inf, vcov = vcovHC(modl2, type = "HC1"))

cov1        <- vcovHC(modl2, type = "HC1")
robust.se1   <- sqrt(diag(cov1))

stargazer(modl, type = "text",
          se = list(NULL, robust.se1), intercept.bottom = FALSE, digits = 2, digits.extra = 2)

# -------------------------------------
# third stage model - example only
# get real version form Michiel

modl3 <- lm(yld ~ N + dist2HQ + dist2market + dist2town + sex + age + years + rural +
              educ + educ1555 + log(asset) + zone + surveyyear + legume + hh_slope + r1,
            data=endog2)

# bootstrap se

coeftest(modl3, df = Inf, vcov = vcovHC(modl3, type = "HC1"))

cov1        <- vcovHC(modl3, type = "HC1")
robust.se1   <- sqrt(diag(cov1))

stargazer(modl, type = "text",
          se = list(NULL, robust.se1), intercept.bottom = FALSE, digits = 2, digits.extra = 2)
