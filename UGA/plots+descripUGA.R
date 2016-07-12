# -------------------------------------
# Exploratory analysis, plots and 
# descriptive stats for all three waves of
# the UGA data
# -------------------------------------

library(dplyr)

dataPath <- "C:/Users/Tomas/Documents/LEI/"

# source in the full data and summary stat
# functions
source(file.path(dataPath, "pro-gap/UGA/data_combineUGA.R"))
source(file.path(dataPath, "functions/summTabs.R"))
source(file.path(dataPath, "functions/winsor.R"))

# basic counts of households, maize farmers
# and maize plots

households <- group_by(fullData, surveyyear) %>% summarise(households=length(unique(hhid)))
parcels <- group_by(fullData, surveyyear) %>% summarise(parcels=length(unique(paste(hhid, parcel_id))))
plots <- group_by(fullData, surveyyear) %>% summarise(plots=length(unique(paste(hhid, parcel_id, plot_id))))
crops <- group_by(fullData, surveyyear) %>% summarise(crops=length(unique(paste(hhid, parcel_id, plot_id, crop_code)))) 
maize_farms <- fullData %>% filter(crop_code==130) %>%
  group_by(surveyyear) %>% summarise(maize_farms=length(unique(hhid)),
                                     maize_plots=length(unique(paste(hhid, parcel_id, plot_id))))
counts <- left_join(households, parcels) %>% left_join(plots) %>% left_join(crops) %>% left_join(maize_farms)

# summary statistics of key variables 
fullData2 <- filter(fullData, status=="HEAD")

probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)
vars <- c("area_farmer", "area_gps", "fallow_years", "irrig", "qty", "trans_cost",
          "crop_count", "legume", "maize_", "wheat", "manure", "manure_qty",
          "inorg", "pest", "lab", "yld", "assetph")
knitr::kable(round(summTab(fullData2, vars, probs=probs), 3), caption="summary stats 2008")

# all years
fullData2$surveyyear <- as.character(fullData2$surveyyear)
S <- summTabSplit(fullData2, vars, splitVar="surveyyear", unique(fullData2$surveyyear), probs)
knitr::kable(round(S[["2009"]], 3), caption="summary stats 2009")
knitr::kable(round(S[["2010"]], 3), caption="summary stats 2010")
knitr::kable(round(S[["2011"]], 3), caption="summary stats 2011")

# from the summary tables it seems that some 
# variables have outliers
source(file.path(dataPath,"functions/removeOutliers.R"))
clean <- removeOutly(fullData2, )
