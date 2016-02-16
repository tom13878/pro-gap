#######################################
########## TANZANIA 2010-11 ###########
#######################################

# WDswitch
dataPath <- "D:\\Data\\IPOP\\SurveyData\\"
extraDataPath <- "D:\\Dropbox\\Michiel_research\\2285000066 Africa Maize Yield Gap\\Analysis\\TZA\\Data"
# dataPath <- "C:/Users/Tomas/Documents/LEI/data/TZA"
wdPath <- "D:\\Dropbox\\Michiel_research\\2285000066 Africa Maize Yield Gap"
setwd(wdPath)



library(haven)
library(stringr)
library(plyr)
library(dplyr)

options(scipen=999)


#######################################
############### OUTPUT ################
#######################################

# WDswitch
oput <- read_dta(file.path(dataPath, "TZA\\2010\\Data\\TZNPS2AGRDTA/AG_SEC4A.dta")) %>%
# oput <- read_dta(file.path(dataPath, "TZNPS2AGRDTA/AG_SEC4A.dta")) %>%
  dplyr::select(y2_hhid, plotnum, zaocode, inter_crop=ag4a_04,
         harv_area=ag4a_08, qty=ag4a_15, valu=ag4a_16, hybrd=ag4a_23)

oput$inter_crop <- ifelse(oput$inter_crop %in% 1, 1, 0)
oput$hybrd <- ifelse(oput$hybrd %in% 2, 1, 0)
oput$zaocode <- as.integer(oput$zaocode)

legumes <- c(31, 32, 33, 35, 36, 37, 41, 42, 43, 47, 48)

oput_x <- group_by(oput, y2_hhid, plotnum) %>%
  summarise(crop_count=length(unique(zaocode[!is.na(zaocode)])),
            legume=ifelse(any(zaocode %in% legumes), 1, 0))

oput <- left_join(oput, oput_x); rm(oput_x)

oput_maize <- oput[ oput$zaocode %in% 11 & ! is.na(oput$qty) & !oput$qty %in% 0, ]
oput_maize$maize_prc <- oput_maize$valu/oput_maize$qty
oput_maize <- dplyr::select(oput_maize, -zaocode, -valu)

rm(list=c("oput", "legumes"))

#######################################
############# CHEMICAL ################
#######################################

# WDswitch
plot <- read_dta(file.path(dataPath, "TZA\\2010\\Data\\TZNPS2AGRDTA/AG_SEC3A.dta")) %>%
# plot <- read_dta(file.path(dataPath, "\\TZNPS2AGRDTA/AG_SEC3A.dta")) %>%
  dplyr::select(y2_hhid, plotnum, maize=zaocode, soil=ag3a_09, slope_farmer=ag3a_16, irrig=ag3a_17, title=ag3a_27,
                manure=ag3a_39, pest=ag3a_58, pest_q=ag3a_60_1, pest_q_unit=ag3a_60_2, fallow_year=ag3a_21, fallow=ag3a_22)

plot$maize <- ifelse(plot$maize %in% 11, 1, 0)
plot$soil <- factor(plot$soil, levels=c(1,2,3,4), labels=c("Sandy", "Loam", "Clay", "Other"))
plot$slope_farmer <- factor(plot$slope_farmer, levels=c(1,2,3,4), labels=c("Flat bottom", "Flat top", "Slightly sloped", "Very steep"))
plot$title <- ifelse(plot$title %in% 1, 1, 0) # assume that they don't have a title if NA
plot$irrig <- ifelse(plot$irrig %in% 1, 1, 0)
plot$manure <- ifelse(plot$manure %in% 1, 1, 0)
plot$pest <- ifelse(plot$pest %in% 1, 1, 0)
plot$pest_q_unit <- as_factor(plot$pest_q_unit)

plot$pest_q <- ifelse(plot$pest_q_unit %in% c("LITRE", "KG"), plot$pest_q,
                      ifelse(plot$pest_q_unit %in% "MILLILITRE", plot$pest_q*0.001, NA))


# two questions on fallow - make sure they match up correctly
# fallow value of 98 means subject did not know how long plot
# was left fallow
plot$fallow_year <- ifelse(plot$fallow_year %in% 98, NA, plot$fallow_year)
plot$fallow <- ifelse(plot$fallow_year %in% 0, 0, plot$fallow )
plot$fallow <- ifelse(is.na(plot$fallow_year), NA, plot$fallow)
plot <- dplyr::select(plot, -fallow_year, - pest_q_unit)

# WDswitch
fert1 <- read_dta(file.path(dataPath, "TZA\\2010\\Data\\TZNPS2AGRDTA/AG_SEC3A.dta")) %>%
#fert1 <- read_dta(file.path(dataPath, "TZNPS2AGRDTA/AG_SEC3A.dta")) %>%
  dplyr::select(y2_hhid, plotnum, typ=ag3a_46, qty=ag3a_47, vouch=ag3a_48, valu=ag3a_49)

# WDswitch
fert2 <- read_dta(file.path(dataPath, "TZA\\2010\\Data\\TZNPS2AGRDTA/AG_SEC3A.dta")) %>%
#fert2 <- read_dta(file.path(dataPath, "TZNPS2AGRDTA/AG_SEC3A.dta")) %>%
  dplyr::select(y2_hhid, plotnum, typ=ag3a_53, qty=ag3a_54, vouch=ag3a_55, valu=ag3a_56)

fert1$typ <- as_factor(fert1$typ)
fert1$vouch <- ifelse(fert1$vouch %in% 2, 0, fert1$vouch)

fert2$typ <- as_factor(fert2$typ)
fert2$vouch <- ifelse(fert2$vouch %in% 2, 0, fert2$vouch)

levels(fert1$typ) <- levels(fert2$typ) <-
  c("DAP", "UREA", "TSP", "CAN", "SA", "generic NPK (TZA)", "MRP")

# -------------------------------------
# reorganize data so that observations
# on fertilizer type occupy a single row
# fertilizer is unit of observation
# Data on NPK composition from Sheahan et al (2014), Food Policy
# -------------------------------------

conv <- read.csv(paste(extraDataPath, "Fert_comp.csv", sep="/")) %>%
  transmute(typ=Fert_type2, n=N_share/100, p=P_share/100) %>%
  filter(typ %in% levels(fert1$typ))


fert1 <- left_join(fert1, conv)
fert2 <- left_join(fert2, conv)

# -------------------------------------
# organize fertilizer data for analysis

fert <- rbind(fert1, fert2)

fert <- mutate(fert,
               Vfert=valu/qty,
               Qn=qty*n,
               Qp=qty*p)

fert$Pn <- fert$Vfert/fert$n


# Compute subsidised, non-subsidised and mix fertilizer prices per plot
# As valu or N is sometimes 0, all prices that are 0 are set to NA
fertnosub   <- filter(fert, vouch==0) %>%
  group_by(y2_hhid, plotnum) %>%
  summarise(N=sum(Qn, na.rm=TRUE),
            WPnnosub=sum((Qn/N)*Pn, na.rm=TRUE)) %>%
  dplyr::select(-N) %>%
  mutate(WPnnosub = replace(WPnnosub, WPnnosub==0, NA))


fertsub   <- filter(fert, vouch==1) %>%
  group_by(y2_hhid, plotnum) %>%
  summarise(N=sum(Qn, na.rm=TRUE),
            WPnsub=sum((Qn/N)*Pn, na.rm=TRUE)) %>%
  dplyr::select(-N) %>% 
  mutate(WPnsub = replace(WPnsub, WPnsub==0, NA))

fertmix <- filter(fert, vouch %in% c(0,1)) %>%
  group_by(y2_hhid, plotnum) %>%
  summarise(N=sum(Qn, na.rm=TRUE),
            P=sum(Qp, na.rm=TRUE),
            WPn=sum((Qn/N)*Pn, na.rm=TRUE)) %>%
  mutate(WPn = replace(WPn, WPn==0, NA))

# join back with the rest of the data and set N and P to 0 for NA values
plot <- left_join(plot, fertmix) %>%
  left_join(., fertnosub) %>%
  left_join(., fertsub) %>%
  mutate(N = ifelse(is.na(N), 0, N),
         P = ifelse(is.na(P), 0, P))

rm(list=c("fert1", "fert2", "fert", "fertsub", "fertnosub", "fertmix", "conv"))

#######################################
############### LABOUR ################
#######################################

# WDswitch
lab <- read_dta(file.path(dataPath, "TZA\\2010\\Data\\TZNPS2AGRDTA/AG_SEC3A.dta")) %>%
# lab <- read_dta(file.path(dataPath, "\\TZNPS2AGRDTA/AG_SEC3A.dta")) %>%
  dplyr::select( y2_hhid, plotnum, ag3a_70_id1:ag3a_72_9 )

# remove houshold labour IDs and question ag3a_71 which we don't need
bad <- grep( "ag3a_70_id", names( lab ) )
lab <- lab[, -bad]
lab <- dplyr::select( lab, -ag3a_71 )

# remove variables that refer to wage paid for hired labour
# this could be added later to consider input costs
bad <- names( lab )[( length( lab )-15 ):length( lab )][seq( from=4, to=16, by=4 )]
lab <- lab[, -which( names( lab ) %in% bad )]

# create a dataframe with just family and hired labour
lab <- transmute( lab, y2_hhid, plotnum,
                  fam_lab_days=rowSums( lab[, 3:26], na.rm=TRUE ),
                  hir_lab_days=rowSums( lab[, 27:ncol( lab )], na.rm=TRUE ) )

lab <- transmute(lab, y2_hhid, plotnum, lab=fam_lab_days + hir_lab_days)

# doesn't make sense to have 0 labour on a plot so set values to zero
lab$lab <- ifelse(lab$lab %in% 0, NA, lab$lab)


#######################################
############### GEO ###################
#######################################

loc <- read_dta(file.path(dataPath, "TZA\\2010\\Data\\TZNPS2HH1DTA/HH_SEC_A.dta")) %>%
  dplyr::select(ea_id2010 = ea, y2_hhid, rural=y2_rural, region_code_lsms=region, district_code_lsms=district) %>%
  mutate(rural = ifelse(rural == 1, 1, 0))

# TZA_2010 does not include labels for region and district. 
# We use information from TZA_2012 to add labels in a systematic way in all years.
# Only labels for regions are added as districts are not used in the analysis.
loc$region_name_lsms <- factor(loc$region_code_lsms, 
                               levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 51, 52, 53, 54, 55),
                               labels = c("Dodoma", "Arusha", "Kilimanjaro", "Tanga", "Morogoro", "Pwani", 
                                          "Dar es Salaam", "Lindi", "Mtwara", "Ruvuma", "Iringa", "Mbeya",
                                          "Singida", "Tabora", "Rukwa", "Kigoma", "Shinyanga", "Kagera", "Mwanza", 
                                          "Mara", "Manyara", "Kaskazini Unguja", "Kusini Unguja", "Mjini Magharibi",
                                          "Kaskazini Pemba", "Kusini Pemba"))
# Create labels of regions and zones in uppercase!

# add a zone variable
loc$zone[loc$region_name_lsms %in% c("Kagera","Mwanza", "Mara")] <- "Lake"
loc$zone[loc$region_name_lsms %in% c("Shinyanga","Kigoma", "Tabora")] <- "Western"
loc$zone[loc$region_name_lsms %in% c("Arusha","Kilimanjaro", "Manyara", "Tanga")] <- "Northern"
loc$zone[loc$region_name_lsms %in% c("Singida","Dodoma")] <- "Central"
loc$zone[loc$region_name_lsms %in% c("Rukwa", "Mbeya","Iringa")] <- "Southern Highlands"
loc$zone[loc$region_name_lsms %in% c("Pwani","Morogoro", "Dar-Es-Salaam")] <- "Eastern"
loc$zone[loc$region_name_lsms %in% c("Lindi","Ruvuma", "Mtwara")] <- "Southern"
loc$zone[loc$region_name_lsms %in% c("Dar es Salaam")] <- "Dar es Salaam" 
loc$zone[loc$region_name_lsms %in% c("Kaskazini Unguja", "Kaskazini Unguja", "Kusini Pemba", "Kusini Unguja",
                                     "Kaskazini Pemba", "Mjini Magharibi")] <- "Zanzibar"

loc$zone <- factor(loc$zone)

# NB: names of distance variables in BID are not inconsistent/not correct. There are two plot-level geo datasets. 
# One is located in the AGRDTA folder and the other in the GEODTA folder. Data is the same but in the latter the hh to plot distance
# is reffered to as dist01. We use this file. In the BID, the names are not correct. dist_hh = dist01 and dist01 =dist02. We rename.

# WDswitch
geo <- readRDS(file.path(wdPath, "Analysis\\TZA\\Data\\TZA_geo_2010.rds")) %>% 
# geo <- read.csv(file.path(dataPath, "TZA_geo_total_2010.csv"), stringsAsFactors=F) %>% 
  dplyr::select(y2_hhid, lon, lat, plotnum, SPEI, RootDepth, region_name=NAME_1, district_name=NAME_2,
                AEZ=land03, ph=ph_sd1_sd3, ph2=ph_sd1_sd5,
                SOC=SOC_sd1_sd3, SOC2=SOC_sd1_sd5, rain_CRU=gsRainfall, 
                dist_hh=dist01,  dist_road=dist02, dist_popcenter=dist03, dist_market=dist04, dist_borderpost=dist05, dist_regcap=dist06, 
                hh_elevation=soil01, hh_slope=soil02, hh_twi=soil03, 
                rain_year=crops07, rain_wq=crops08,
                YA, YW, YP) %>%
  unique()

#######################################
############### AREAs #################
#######################################

# WDswitch
areas <- read_dta(file.path(dataPath, "Plot_size/areas_tza_y2_imputed.dta")) %>%
# areas <- read_dta(file.path(dataPath, "areas_tza_y2_imputed.dta")) %>%  
  dplyr::select(y2_hhid=case_id, plotnum,
                area_farmer=area_sr, area_gps=area_gps_mi_50)

areas$area_gps <- ifelse(areas$area_gps %in% 0, NA, areas$area_gps)

areaTotal <- group_by(areas, y2_hhid) %>%
  summarise(area_tot = sum(area_gps))

areaTotal$area_tot <- ifelse(areaTotal$area_tot %in% 0, NA, areaTotal$area_tot)


#######################################
############### ASSETS ################
#######################################

# WDswitch
implmt <- read_dta(file.path(dataPath, "TZA\\2010\\Data\\TZNPS2AGRDTA/AG_SEC11.dta")) %>%
# implmt <- read_dta(file.path(dataPath, "TZNPS2AGRDTA/AG_SEC11.dta")) %>%
  dplyr::select(y2_hhid, itemcode, qty=ag11_01, valu=ag11_02) %>%
  filter(!qty %in% 0, !is.na(qty), !valu %in% 0, !is.na(valu)) %>%
  transmute(y2_hhid, valu=qty*valu) %>%
  group_by(y2_hhid) %>%
  summarise(value=sum(valu))

#######################################
########## TRANSPORT COSTS ############
#######################################

# WDswitch
tc <- read_dta(file.path(dataPath, "TZA\\2010\\Data\\TZNPS2AGRDTA/AG_SEC5a.dta")) %>%
# tc <- read_dta(file.path(dataPath, "TZNPS2AGRDTA/AG_SEC5a.dta")) %>%
  dplyr::filter(zaocode %in% 11) %>%
  dplyr::select(y2_hhid, trans=ag5a_15, trans_dist=ag5a_16, trans_cost=ag5a_19)

tc$trans <- ifelse(tc$trans %in% 1, 1, 0)

#######################################
########### SOCIO/ECONOMIC ############
#######################################

# WDswitch
se <- read_dta(file.path(dataPath, "TZA\\2010\\Data\\TZNPS2HH1DTA\\HH_SEC_B.dta")) %>%
# se <- read_dta(file.path(dataPath, "HH_SEC_B.dta")) %>%
  filter(hh_b05 %in% 1) %>% # 1 for head of household
  dplyr::select(y2_hhid, indidy2, sex=hh_b02, yob=hh_b03_1, age=hh_b04)

se$sex <- ifelse(se$sex %in% 2, 1, 0)
se$yob <- as.integer(as.character(se$yob))

# education
# WDswitch
ed <- read_dta(file.path(dataPath, "TZA\\2010\\Data\\TZNPS2HH1DTA\\HH_SEC_C.dta")) %>%
# ed <- read_dta(file.path(dataPath, "HH_SEC_C.dta")) %>%
  dplyr::select(y2_hhid, indidy2, start=hh_c04, end=hh_c08)

ed$end <- as.integer(as.character(ed$end))
ed$end <- ifelse(ed$end %in% 9999, NA, ed$end)

# join se and ed to find years in school
se <- left_join(se, ed) %>% dplyr::select(-indidy2)
rm("ed")

se$educ <- se$end - (se$yob + se$start)
se <- dplyr::select(se, -start, -end, -yob)

# if anyone received negative years of schooling, set to NA
se$educ <- ifelse(se$educ < 0, NA, se$educ)

# still some people have received a lot of schooling!!!!
# also NA values!

# plot ownership

# WDswitch
own <- read_dta(file.path(dataPath, "TZA\\2010\\Data\\TZNPS2AGRDTA/AG_SEC3A.dta")) %>%
# own <- read_dta(file.path(dataPath, "TZNPS2AGRDTA/AG_SEC3A.dta")) %>%
  dplyr::select(y2_hhid, plotnum, own=ag3a_24)

own$own <- ifelse(own$own %in% 1 | own$own %in% 5, 1, 0)

#######################################
########### CROSS SECTION #############
#######################################

# joins at the plot level
TZA2010 <- left_join(oput_maize, plot)
TZA2010 <- left_join(TZA2010, lab)
TZA2010 <- left_join(TZA2010, areas)
TZA2010 <- left_join(TZA2010, own)
TZA2010 <- left_join(TZA2010, geo)

# joins at the household level
TZA2010 <- left_join(TZA2010, implmt)
TZA2010 <- left_join(TZA2010, loc)
TZA2010 <- left_join(TZA2010, se)
TZA2010 <- left_join(TZA2010, tc)
TZA2010 <- left_join(TZA2010, areaTotal)


# -------------------------------------
# Make some new variables
# -------------------------------------

# per hectacre
TZA2010 <- mutate(TZA2010,
                  yld=qty/area_gps,
                  N=N/area_gps,
                  P=P/area_gps,
                  lab=lab/area_gps,
                  pest_q=pest_q/area_gps,
                  asset=value/area_tot
)



# -------------------------------------
# Inflate 2011 prices to 2013 prices: assets, fertilizer and maize
# using inflation rate for 2011 and 2013. These years were selected as the main part of the survey takes place in these years.
# from world bank:
# http://data.worldbank.org/indicator/FP.CPI.TOTL.ZG/countries/TZ?display=graph
# -------------------------------------

inflation <- read.csv(file.path(extraDataPath, "inflation.csv"))
rate2011 <- inflation$inflation[inflation$code=="TZ" & inflation$year==2011]
rate2013 <- inflation$inflation[inflation$code=="TZ" & inflation$year==2013]

TZA2010 <- mutate(TZA2010,
                  asset = asset*(1 + rate2011)*(1 + rate2013),
                  maize_prc = maize_prc*(1 + rate2011)*(1 + rate2013),
                  WPn = WPn*(1 + rate2011)*(1 + rate2013),
                  WPnnosub = WPnnosub*(1 + rate2011)*(1 + rate2013),
                  WPnsub = WPnsub*(1 + rate2011)*(1 + rate2013))

TZA2010 <- dplyr::select(TZA2010, -qty, -value)

# add final variables
TZA2010<- mutate(TZA2010, surveyyear=2010) %>% rename(hhid2010=y2_hhid)

# save to file
saveRDS(TZA2010, file=".\\Analysis\\TZA\\Data\\TZA_data_2010.rds")
#write.csv(TZA_2010, "C:/Users/Tomas/Documents/LEI/TZA10_data.csv", row.names=FALSE)



