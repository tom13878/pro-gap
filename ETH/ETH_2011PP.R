# -------------------------------------
# Ethiopia 2011
# -------------------------------------

# setwd
if(Sys.info()["user"] == "Tomas"){
  path2Data <- "C:/Users/Tomas/Documents/LEI/pro-gap/ETH/"
} else {
  path2Data <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/Code/ETH"
}

# source the raw data
suppressMessages(source(file.path(path2Data, "ETH_2011.R")))

# -------------------------------------
# For some questions respondents answered
# NA, it is not certain how these responses
# should be treated. Often we assume that
# an NA is equivalent to NO/0
# -------------------------------------

#ETH2011$inter_crop <- ifelse(ETH2011$inter_crop %in% 2, 1, 0)
ETH2011$irrig <- ifelse(ETH2011$irrig %in% 1, 1, 0)
ETH2011$fert_any <- ifelse(ETH2011$fert_any %in% 1, 1, 0)
ETH2011$manure <- ifelse(ETH2011$manure %in% 1, 1, 0)
ETH2011$compost <- ifelse(ETH2011$compost %in% 1, 1, 0)
ETH2011$other_org <- ifelse(ETH2011$other_org %in% 1, 1, 0)
ETH2011$herb <- ifelse(ETH2011$herb %in% 1, 1, 0)
ETH2011$fung <- ifelse(ETH2011$fung %in% 1, 1, 0)
ETH2011$seed_type <- ifelse(ETH2011$seed_type %in% 2, 1, 0)

rm(path2Data)
