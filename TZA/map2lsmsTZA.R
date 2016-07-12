# -------------------------------------
# District maps using data from the 
# lsms-isa survey
# -------------------------------------

dataPath <- "C:/Users/Tomas/Documents/LEI/data/TZA"

TZA <- readOGR(dsn = file.path(dataPath, "Tanzania_District_EA_2002_region"),
               layer = "Tanzania_District_EA_2002_region")
write.csv(unique(select(TZA@data, REGNAME, DISTNAME)), file.path(dataPath, "map2lsmsTZA"))
