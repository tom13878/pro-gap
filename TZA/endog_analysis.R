# -------------------------------------
# endogeneity analysis
# -------------------------------------

options(scipen=999)

library(dplyr)
library(boot)
library(AER)
library(stargazer)

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
            SOC_bar = mean(SOC),
            SOC2_bar = mean(SOC2),
            irrig_bar = mean(irrig),
            manure_bar = mean(manure),
            legume_bar = mean(legume),
            pest_bar = mean(pest),
            ph_bar = mean(ph),
            ph2_bar = mean(ph2),
            area_bar = mean(area)
            )
endog <- left_join(endog, endog_x); rm(endog_x)

# -------------------------------------
# first stage model - logit/probit model
# (1/0) if farmer received a voucher or not

# time averages: assets, labour, anything
# affecting a farmers decision to get fertilizer
# including the soil quality

modl.probit <- glm(cbind(vouchAny, 1-vouchAny) ~ ccm_prez10*split_prez10 + surveyyear +
                     log(vtot) + dist2HQ + dist2market + dist2town + area + sex + rural +
                     years + lasset + age + lab + education + education1555 + N1555,
                   family=binomial(link=probit), data=endog)

modl.probit.CRE <- glm(cbind(vouchAny, 1-vouchAny) ~ surveyyear + ccm_prez10*split_prez10 +
                         log(vtot) + dist2HQ + dist2market + dist2town + area + sex + rural + 
                         years + lasset + age + lab + education + education1555 + N1555 +
                         lassets_bar + lab_bar + SPEI_bar + SOC_bar + manure_bar +
                         pest_bar + ph_bar + ph2_bar + area_bar + legume_bar,
                       family=binomial(link=probit), data=endog)

# goodness of fit test for the probit 
# model - percent correctly predicted

predicted <- as.numeric(
  predict.glm(modl.probit, type="response") > .5)
true <- endog$vouchAny
correct <- as.numeric(predicted == true)
100*table(correct)/sum(table(correct)) # ~ 90% of the time correct

# goodness of fit test for the probit 
# model with CRE - percent correctly predicted

predicted.CRE <- as.numeric(
  predict.glm(modl.probit.CRE, type="response") > .5)
correct.CRE <- as.numeric(predicted.CRE == true)
100*table(correct.CRE)/sum(table(correct.CRE)) # ~ 90% of the time correct

# also do percentage correctly predicted for
# each outcome (y=1 and y=0)

correct1 <- as.numeric(predicted[true == 1] == true[true == 1])
100*table(correct1)/sum(table(correct1)) # correct only ~ 25 % of the time
correct0 <- as.numeric(predicted[true == 0] == true[true == 0])
100*table(correct0)/sum(table(correct0)) # correct ~ 97% of the time

# also do percentage correctly predicted for
# each outcome with the probit-CRE model (y=1 and y=0)

correct1 <- as.numeric(predicted.CRE[true == 1] == true[true == 1])
100*table(correct1)/sum(table(correct1)) # correct only ~ 36 % of the time
correct0 <- as.numeric(predicted.CRE[true == 0] == true[true == 0])
100*table(correct0)/sum(table(correct0)) # correct ~ 97% of the time

# overall percent correctly predicted as above.
# this is the same as weighted average.
# model is much better at correctly predicting
# zeros than 1s. 

prop1.true <- mean(true)
prop0.pred <- mean(correct0)
prop1.pred <- mean(correct1)
(1-prop1.true)*prop0.pred + prop1.true*prop1.pred # 0.88

# extract residuals from probit
# here I use pearson residuals. Altrnatives
# are deviance residuals, working residuals
# or response residuals. Best is probably the
# pearson residuals for this stage

stargazer(modl.probit, modl.probit.CRE, type="html")

r1 <- residuals(modl.probit.CRE, type="pearson")

# -------------------------------------
# second stage model - tobit model for adoption
# of nitrogen using residuals from the first stage
# and HH chars and plot chars as covariates, along
# with CRE time averages to control for unobserved
# household fixed effects

# include soil variables. And other variables leading to higher demand for fertilizer.

modl.tobit <- tobit(N ~ dist2HQ + dist2market + dist2town + sex + age + years + rural +
                      education + education1555 + N1555 + lasset + surveyyear + zone + 
                      legume + r1
                    , data = endog)

modl.tobit.CRE <- tobit(N ~ dist2HQ + dist2market + dist2town + sex + age + years + rural +
                 education + education1555 + N1555 + lasset + surveyyear + zone + 
                 legume + hh_slope + lassets_bar + lab_bar + SPEI_bar + SOC_bar +
                   manure_bar + pest_bar + ph_bar + ph2_bar + area_bar + legume_bar + r1
               , data=endog)

# bootstrap se from the AER book ->
# recall that tobit is estimated via
# ml, so z-score not t-score

refit <- function(data, i){
  coef(tobit(N ~ dist2HQ + dist2market + dist2town + sex + age + years + rural +
               education + education1555 + N1555 + lasset + surveyyear + zone + 
               legume + hh_slope + r1
             , data=data[i, ]))
}
             

refit.CRE <- function(data, i){
  coef(tobit(N ~ dist2HQ + dist2market + dist2town + sex + age + years + rural +
               education + education1555 + N1555 + lasset + surveyyear + zone + 
               legume + hh_slope + lassets_bar + lab_bar + SPEI_bar + SOC_bar +
               manure_bar + pest_bar + ph_bar + ph2_bar + area_bar + legume_bar + r1
                                      , data=data[i,]))
}


boot.out <- boot(endog, refit, R = 500) # 500 like in liverpool-tasie 
boot.out.CRE <- boot(endog, refit.CRE, R=500)

# grab the standard errors and pass to stargazer

sds <- apply(boot.out$t, 2, sd)
sds.CRE <- apply(boot.out.CRE$t, 2, sd)
names(sds) <- names(coef(modl.tobit))
names(sds.CRE) <- names(coef(modl.tobit.CRE))

# put p.auto to TRUE to calcualte the
# p-vals from supplied se

stargazer(modl.tobit, modl.tobit.CRE, type = "html",
          se = list(sds, sds.CRE), p.auto=TRUE,
          intercept.bottom = FALSE,
          digits = 2, digits.extra = 2)

# get residuals for the third stage - pearson
# residuals not available so we use the tobit 
# residuals

r2 <- residuals(modl.tobit, type="deviance")

