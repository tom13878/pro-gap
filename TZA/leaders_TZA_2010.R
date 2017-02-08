# -------------------------------------
# Political variables from household
# questionnaire 2010
# -------------------------------------

if(Sys.info()["user"] == "Tomas"){
  dataPath <- "C:/Users/Tomas/Documents/LEI/data/TZA/2010/Data"
} else {
  dataPath <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/TZA/2010/Data"
}

# load packages
library(haven)
library(dplyr)

# remove scientific notation
options(scipen=999)

# read in local community governance structure
# information
pol <- read_dta(file.path(dataPath, "TZNPS2COMDTA/COMSEC_CI.dta")) %>%
  select(REGCODE=id_01, DISCODE=id_02, ward=id_03, ea=id_04, position=cm_i01,
         sex=cm_i02, resident=cm_i05, party=cm_i06, village=cm_i07,
         religion=cm_i10)

# make factor variables
pol$position <- as_factor(pol$position)
pol$sex <- as_factor(pol$sex)
pol$party <- as_factor(pol$party)
pol$religion <- ifelse(pol$religion %in% 1, "TDL", ifelse(pol$religion %in% 2, "CHRSTN", ifelse(pol$religion %in% 3, "MSLM", "OTHR")))

rm(dataPath)
