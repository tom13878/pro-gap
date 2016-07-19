# join the data with the political infomration for
# each district

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

lm1 <- lm(I(log(N+1)) ~ split_prez10 + log(asset) + log(area_tot) + SACCO +
            factor(surveyyear) + factor(ZONE) + education + education1555 +
            rural + sex + age + years + dist_road + dist_popcenter +
            dist_market, dat=maize)

# try a tobit model
library(AER)
modl.tobit <- tobit(N ~ split_prez10 + log(asset) + log(area_tot) + SACCO +
        factor(surveyyear) + factor(ZONE) + education + education1555 +
        rural + sex + age + years + dist_road + dist_popcenter +
        dist_market, dat=maize)

# get the residuals
r2 <- residuals(modl.tobit, type="deviance")

# clearly some outliers, check leverages too
hist(r2)
