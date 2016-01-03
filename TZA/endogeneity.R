# -------------------------------------
# Endogeneity analysis
# -------------------------------------



modl <- glm(cbind(vouchTotal, 2-vouchTotal) ~ sex + dist2town +
            dist2market + dist2HQ + ccm_prez10 + split_prez10 + vtot + SPEI +
              age*years + households + I(age^2),
            family=binomial, data=data)

modl <- glm(cbind(vouchTotal, 2-vouchTotal) ~ offset(vtot) + age + sex ,
            family=binomial, data=data)
