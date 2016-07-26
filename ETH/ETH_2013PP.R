# -------------------------------------
# post processing file for Ethiopia
# -------------------------------------

# setwd
if(Sys.info()["user"] == "Tomas"){
  path2Data <- "C:/Users/Tomas/Documents/LEI/pro-gap/ETH/"
} else {
  path2Data <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/Code/ETH"
}

# source the raw data
suppressMessages(source(file.path(path2Data, "ETH_2013.R")))

# -------------------------------------
# For some questions respondents answered
# NA, it is not certain how these responses
# should be treated. Often we assume that
# an NA is equivalent to NO/0
# -------------------------------------

ETH2013$inter_crop <- ifelse(ETH2013$inter_crop %in% 2, 1, 0)
ETH2013$fallow10 <- ifelse(ETH2013$fallow10 %in% 1, 1, 0)
ETH2013$extension <- ifelse(ETH2013$extension %in% 1, 1, 0)
ETH2013$irrig <- ifelse(ETH2013$irrig %in% 1, 1, 0)
ETH2013$fert_any <- ifelse(ETH2013$fert_any %in% 1, 1, 0)
ETH2013$other_inorg <- ifelse(ETH2013$other_inorg %in% 1, 1, 0)
ETH2013$manure <- ifelse(ETH2013$manure %in% 1, 1, 0)
ETH2013$compost <- ifelse(ETH2013$compost %in% 1, 1, 0)
ETH2013$other_org <- ifelse(ETH2013$other_org %in% 1, 1, 0)
ETH2013$eros_prot <- ifelse(ETH2013$eros_prot %in% 1, 1, 0)
ETH2013$mulch <- ifelse(ETH2013$mulch %in% 1, 1, 0)
ETH2013$herb <- ifelse(ETH2013$herb %in% 1, 1, 0)
ETH2013$fung <- ifelse(ETH2013$fung %in% 1, 1, 0)
ETH2013$seed_type <- ifelse(ETH2013$seed_type %in% 2, 1, 0)

# make a surveyyear variable
ETH2013$surveyyear <- 2013

rm(path2Data)