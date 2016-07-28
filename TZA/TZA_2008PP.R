# -------------------------------------
# Post Processing for the TZA 2008
# data. Here we make some changes to
# the data to make it possible to
# perform further analysis. This file
# is separate from the raw data processing
# file in order to make transparent any
# changes that were made to the data
# -------------------------------------

if(Sys.info()["user"] == "Tomas"){
  path2Data <- "C:/Users/Tomas/Documents/LEI/pro-gap/TZA/"
} else {
  path2Data <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/Code/TZA"
}

# source the data
suppressMessages(source(file.path(path2Data, "/TZA_2008.R")))

# -------------------------------------
# For some questions respondents answered
# NA, it is not certain how these responses
# should be treated. Often we assume that
# an NA is equivalent to NO/0
# -------------------------------------

TZA2008$SACCO <- ifelse(TZA2008$SACCO %in% 1, 1, 0) # assume NA -> no SACCO
TZA2008$death <- ifelse(TZA2008$death %in% 1, 1, 0) # assume NA -> no death
TZA2008$one_crop <- ifelse(TZA2008$one_crop %in% 1, 1, 0) # assume NA -> no crops 
TZA2008$inter_crop <- ifelse(TZA2008$inter_crop %in% 1, 1, 0) # assume NA -> no intercropping
TZA2008$hybrd <- ifelse(TZA2008$hybrd %in% 2, 1, 0) # assume NA -> no hybrid seeds
TZA2008$title <- ifelse(TZA2008$title %in% 1, 1, 0) # assume NA -> no title
TZA2008$irrig <- ifelse(TZA2008$irrig %in% 1, 1, 0) # assume NA -> no irrigation
TZA2008$manure <- ifelse(TZA2008$manure %in% 1, 1, 0) # assume NA -> no manure
TZA2008$N <- ifelse(is.na(TZA2008$N), 0, TZA2008$N) # assume NA -> no nitrogen
TZA2008$P <- ifelse(is.na(TZA2008$P), 0, TZA2008$P) # assume NA -> no Phosphorous
TZA2008$pest <- ifelse(TZA2008$pest %in% 1, 1, 0) # assume NA -> no pesticide
TZA2008$trans <- ifelse(TZA2008$trans %in% 1, 1, 0) # assume NA -> no transportation for crop

rm("path2Data")


