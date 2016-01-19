# -------------------------------------
# Endogeneity analysis
# -------------------------------------

library(dplyr)
library(haven)

# -------------------------------------
# read in the two dataframes for 2010 
# and 2012

setwd("c:/users/tomas/documents/lei")
endog2010 <- read_dta("endog2010.dta")
endog2012 <- read_dta("endog2012.dta")

# ------------------------------------
# for now ignore the 2005 election
# results

endog2010$ccm_prez05 <- endog2010$split_prez05 <- NULL

# ------------------------------------
# unfortunately the dist2Rd variable does
# not seem to be available for 2010 in 
# the geo variables file, remove for now

endog2012$dist2Rd <- NULL

# -------------------------------------
# remove the y3_hhid and rearrange variables
# for the 2012 data to match up with 2010

endog2012$y3_hhid <- NULL 
endog2012 <- endog2012[, c(23, 1:22)]

# -------------------------------------
# make some year factor variables

endog2010$surveyyear <- 2010 # factor(1, levels=1:2, labels=c("2010", "2012"))
endog2012$surveyyear <- 2012 # factor(2, levels=1:2, labels=c("2010", "2012"))

# -------------------------------------
# join each endog database with the 
# corresponding final data sample

setwd("c:/users/tomas/downloads")
db0 <- readRDS("db0.rds")

endog2010_2 <- inner_join(db0, endog2010) # should be 1344
endog2012_2 <- inner_join(db0, endog2012) # should be 1345

# -------------------------------------
# join the data together

endog <- rbind(endog2010_2, endog2012_2) # 500 observations too many???
endog$sex <- as_factor(endog$sex)
endog$cage <- as_factor(endog$cage)
endog$surveyyear <- factor(endog$surveyyear)

# -------------------------------------
# analysis and models - start with
# pooled model trying to explain
# characteristics leading to a voucher
# using just a linear model

# lack of education values for a large
# number of househol heads means we
# lose a lot of observations

endog2 <- endog[complete.cases(endog), ]

# binomial model n=1, farmer received any
# vouchers - can also do a tobit, but will
# not be much variation in response, maybe
# not a big problem though

# -------------------------------------
# first stage model

modl <- glm(cbind(vouchAny, 1-vouchAny) ~ surveyyear + ccm_prez10*split_prez10 +
               vtot + dist2HQ + dist2market + dist2town + sex + age + years + rural +
              educ + educ1555 + asset + SPEI + AEZ,
            family=binomial, data=endog2)

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
