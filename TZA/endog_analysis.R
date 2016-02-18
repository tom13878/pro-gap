# -------------------------------------
# endogeneity analysis
# -------------------------------------

options(scipen=999)
library(dplyr)

# -------------------------------------
# read in the prepared data and select
# only complete rows

setwd("c:/users/tomas/documents/lei")
endog <- readRDS("endog.rds")
endog <- endog[complete.cases(endog),]

# make variables for CRE
endog$lasset <- log(endog$asset)
endog_x <- group_by(endog, hhid2010) %>%
  summarise(lassets_bar = mean(lasset),
            lab_bar = mean(lab),
            SPEI_bar = mean(SPEI),
            N_bar = mean(N),
            SOC_bar = mean(SOC),
            SOC2_bar = mean(SOC2),
            irrig_bar = mean(irrig),
            manure_bar = mean(manure),
            pest_bar = mean(pest),
            ph_bar = mean(ph),
            ph2_bar = mean(ph2)
            )
endog <- left_join(endog, endog_x); rm(endog_x)

# -------------------------------------
# first stage model - logit/probit model
# (1/0) if farmer received a voucher or not

# time averages: assets, labour, anything
# affecting a farmers decision to get fertilizer
# including the soil quality

modl.probit <- glm(cbind(vouchAny, 1-vouchAny) ~ surveyyear + ccm_prez10*split_prez10 +
                     log(vtot) + dist2HQ + dist2market + dist2town +
                     years + lasset + age + lab + education + education1555 + N1555 +
                     lassets_bar + lab_bar + SPEI_bar + N_bar + SOC_bar + manure_bar +
                    pest_bar + ph_bar + ph2_bar,
                   family=binomial(link=probit), data=endog)

# goodness of fit test for the probit 
# model - percent correctly predicted

predicted <- as.numeric(
  predict.glm(modl.probit, type="response")>.5)
true <- endog$vouchAny
correct <- as.numeric(predicted == true)
100*table(correct)/sum(table(correct))

# also do percentage correctly predicted for
# each outcome (y=1 and y=0)

correct1 <- as.numeric(predicted[true == 1] == true[true == 1])
100*table(correct1)/sum(table(correct1)) # correct only ~ 48% of the time
correct0 <- as.numeric(predicted[true == 0] == true[true == 0])
100*table(correct0)/sum(table(correct0)) # correct ~ 97% of the time

# overall percent correctly predicted as above.
# Show that it is the same as weighted average
# problem here is that the model is good at 
# predicting 0s and not 1s

prop1.true <- mean(true)
prop0.pred <- mean(correct0)
prop1.pred <- mean(correct1)
(1-prop1.true)*prop0.pred + prop1.true*prop1.pred # 0.87

# extract residuals from probit
# and move on with analysis anyway
# here I use pearson residuals. Altrnatives
# are deviance residuals, working residuals
# or response residuals. Best is probably the
# pearson residuals

r1 <- residuals(modl.probit, type="pearson")

# -------------------------------------
# second stage model - tobit model for adoption
# of nitrogen using residuals from the first stage
# and HH chars and plot chars as covariates

library(AER) # for tobit function

# include soil variables. And other variables leading to higher demand for fertilizer.

modl.tobit <- tobit(N ~ dist2HQ + dist2market + dist2town + sex + age + years + rural +
                 education + education1555 + N1555 + log(asset) + zone + surveyyear +
                 legume + hh_slope + r1
               , data=endog)

# bootstrap se from the AER book ->
# recall that tobit is estimated via
# ml, so z-score not t-score

library(boot)
library(AER)
library(stargazer)

refit <- function(data, i){
  coef(tobit(N ~ dist2HQ + dist2market + dist2town + sex + age + years + rural +
                                        education + education1555 + N1555 + log(asset) + zone + surveyyear +
                                        legume + hh_slope + r1
                                      , data=data[i,]))
}


boot.out <- boot(endog, refit, R = 500) # I have seen 500 eg in liverpool-tasie 

# grab the standard errors and pass to stargazer
sds <- apply(boot.out$t, 2, sd)
names(sds) <- names(coef(modl.tobit))

# put p.auto to TRUE to calcualte the
# p-vals from supplued se
stargazer(modl.tobit, type = "text",
          se = list(sds), p.auto=TRUE,
          intercept.bottom = FALSE,
          digits = 2, digits.extra = 2)

# get residuals for the third stage
r2 <- residuals(modl.tobit, type="deviance")

