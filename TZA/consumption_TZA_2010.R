# consumption TZA 2010

if(Sys.info()["user"] == "Tomas"){
  dataPath <- "C:/Users/Tomas/Documents/LEI/data/TZA/2010/Data"
} else {
  dataPath <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/TZA/2010/Data"
}

# read in consumption file
consumption <- read_dta(file.path(dataPath, "TZNPS2HH2DTA/TZY2.HH.Consumption.dta"))
