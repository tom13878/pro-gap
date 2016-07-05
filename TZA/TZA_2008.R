#######################################
########## TANZANIA 2008-09 ###########
#######################################

# Tom
dataPath <- "C:/Users/Tomas/Documents/LEI/data/TZA/2008/Data"

# LEI Path
# dataPath <- "W:/LEI/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/TZA/2008/Data"

library(haven)
library(stringr)
library(reshape2)
library(dplyr)

options(scipen=999)

#######################################
############## LOCATION ###############
#######################################

# file containing the zone, region and district

location <- read_dta(file.path(dataPath, "TZNPS1HHDTA_E/SEC_A_T.dta")) %>%
  select(hhid, REGCODE = region, DISCODE = district, rural = rural)
location$rural <- ifelse(location$rural %in% "Rural", 1, 0)
location$REGCODE <- as.integer(location$REGCODE)

# match up with the names from the survey (prepared in a seperate file)

ZONEREGDIS <- read.csv(file.path(paste0(dataPath,"/../../.."), "Other/Spatial/TZA/ZONEREGDIS.csv"))

# join with household identifications

location <- left_join(location, ZONEREGDIS)

rm(ZONEREGDIS)

#######################################
########### SOCIO/ECONOMIC ############
#######################################

HH08 <- read_dta(file.path(dataPath, "/TZNPS1HHDTA_E/SEC_B_C_D_E1_F_G1_U.dta")) %>%
  select(hhid, indidy1=sbmemno, status=sbq5, sex=sbq2,
         yob=sbq3yr, age=sbq4, years=sbq24)

HH08$years <- as.numeric(HH08$years)
HH08$years <- ifelse(HH08$years %in% 99, HH08$age, HH08$years)
HH08$status <- as_factor(HH08$status)
HH08$sex <- toupper(as_factor(HH08$sex))
HH08$yob <- as.integer(HH08$yob)

# make a new variable cage (cut age = cage) which splits
# individuals according to their age group with
# breaks at 15, 55 and the max age

HH08$cage <- cut(HH08$age, breaks = c(0, 15, 55, max(HH08$age, na.rm=TRUE)),
                 labels=1:3, include.lowest = TRUE, right = TRUE)

# education of household members and sum
# of education of all household members
# between the ages of 15 and 55

ed <- read_dta(file.path(dataPath, "/TZNPS1HHDTA_E/SEC_B_C_D_E1_F_G1_U.dta")) %>%
  select(hhid, indidy1=sbmemno, ed_any=sbq2, start=sbq3yr)

ed$ed_any <- as_factor(ed$ed_any) # ever went to school

# join with HH10 dataframe
HH08 <- left_join(HH08, ed)
HH08 <- select(HH08, -start, -yob)

# summarise the data: number of
# household members 15:55
HH08_x <- group_by(HH08, hhid) %>%
  summarise(N1555=sum(cage %in% 2))
HH08 <- left_join(HH08, HH08_x); rm(HH08_x)

# -------------------------------------
# death in the family
# -------------------------------------

death <- read_dta(file.path(dataPath, "TZNPS1HHDTA_E/SEC_S2.dta")) %>%
  select(hhid) %>% mutate(death=1) %>% unique

# -------------------------------------
# membership to a credit group
# -------------------------------------

credit <- read_dta(file.path(dataPath, "TZNPS1HHDTA_E/SEC_O3.dta")) %>%
  select(hhid) %>% unique() %>% mutate(SACCO = 1)

HH08 <- left_join(HH08, death) 
HH08 <- left_join(HH08, credit) 

rm(ed, credit, death)

#######################################
############### OUTPUT ################
#######################################

oput <- read_dta(file.path(dataPath, "TZNPS1AGDTA_E/SEC_4A.dta")) %>%
  dplyr::select(hhid, plotnum, zaocode, 
                one_crop=s4aq3, crop_share=s4aq4,
                inter_crop=s4aq6, harv_area=s4aq8,
                harv_area2=s4aq9, harv_area3=s4aq10,
                qty=s4aq15, valu=s4aq16, hybrd=s4aq22)

oput$crop_share <- as_factor(oput$crop_share)
oput$zaocode <- as.integer(oput$zaocode)
oput$harv_area2 <- as_factor(oput$harv_area2)
oput$harv_area3 <- toupper(as_factor(oput$harv_area3))
levels(oput$crop_share) <- c("1/4", "1/2", "3/4")

# harv area is in acres -> change to hectares
oput$harv_area <- oput$harv_area*0.404686

# -------------------------------------
# create dummy variables for crop groups
# (fruit, cash crops (permanent),
# Cereals/Tubers/Roots, cash crops (not permanent),
# vegetables, legumes)
# -------------------------------------

fruit <- c(70:74, 76:85, 97:99, 67, 38, 39)
cashCropsPerm <- c(53:61, 63:66, 18, 34, 21, 75, 44:46) # permanent cash crops
CTR <- c(11:17, 22:27) # Cereals, Tubers, Roots
cashCropNPerm <- c(50, 51, 53, 62, 19) # non permanent cash crops
vegetables <- c(86:96, 100, 101)
legumes <- c(31, 32, 33, 35, 36, 37, 41, 42, 43, 47, 48)

oput_x <- group_by(oput, hhid, plotnum) %>%
  summarise(crop_count=length(unique(zaocode[!is.na(zaocode)])),
            fruit=ifelse(any(zaocode %in% fruit), 1, 0),
            cashCropsPerm=ifelse(any(zaocode %in% cashCropsPerm), 1, 0),
            CTR=ifelse(any(zaocode %in% CTR), 1, 0),
            cashCropNPerm=ifelse(any(zaocode %in% cashCropNPerm), 1, 0),
            vegetables=ifelse(any(zaocode %in% vegetables), 1, 0),
            legume=ifelse(any(zaocode %in% legumes), 1, 0),
            maize=ifelse(any(zaocode %in% 11), 1, 0), # maize has crop code 11
            wheat=ifelse(any(zaocode %in% 16), 1, 0)) # wheat has crop code 16

oput <- left_join(oput, oput_x); rm(oput_x)

# for productivity of maize farmers we are only interested
# in the maize farmers, exclude everyone else, and farmers
# who responded they produced zero maize, or did not respond (NA)

oput <- oput[! is.na(oput$qty) & !oput$qty %in% 0, ]
oput$crop_price <- oput$valu/oput$qty
oput$valu <- NULL

rm(list=c("legumes", "cashCropNPerm", "cashCropsPerm",
          "CTR", "fruit", "vegetables"))

#######################################
############# CHEMICAL ################
#######################################

plot <- read_dta(file.path(dataPath, "/TZNPS1AGDTA_E/SEC_3A.dta")) %>%
  select(hhid, plotnum, zaocode=s3aq5code, soil=s3aq7, slope_farmer=s3aq14, irrig=s3aq15, title=s3aq25,
                manure=s3aq37, pest=s3aq49, pest_q=s3aq51_amount, pest_q_unit=s3aq51_measure, fallow_year=s3aq19, fallow=s3aq20)

plot$zaocode <- as.integer(plot$zaocode)
plot$soil <- factor(plot$soil, levels=c(1,2,3,4), labels=c("Sandy", "Loam", "Clay", "Other"))
plot$slope_farmer <- factor(plot$slope_farmer, levels=c(1,2,3,4), labels=c("Flat bottom", "Flat top", "Slightly sloped", "Very steep"))
plot$pest_q_unit <- as_factor(plot$pest_q_unit)
plot$fallow <- ifelse(plot$fallow_year %in% 0, 0, plot$fallow )
plot$fallow <- ifelse(is.na(plot$fallow_year), NA, plot$fallow) 


plot$pest_q <- ifelse(plot$pest_q_unit %in% c("LITRE", "KG"), plot$pest_q,
                      ifelse(plot$pest_q_unit %in% "MILLILITRE", plot$pest_q*0.001, NA))

# two questions on fallow - make sure they match up correctly
# fallow value of 98 means subject did not know how long plot
# was left fallow

plot$fallow_year <- ifelse(plot$fallow_year %in% 98, NA, plot$fallow_year)
plot <- dplyr::select(plot, -fallow_year, - pest_q_unit)

# inorganic fertilizer - note that there is no inorganic fertilizer
# voucher question as in 2010 and 2012 surveys. Farmers are only asked
# about one inorganic fertilizer

fert <- read_dta(file.path(dataPath, "TZNPS1AGDTA_E/SEC_3A.dta")) %>%
  dplyr::select(hhid, plotnum, typ=s3aq44, qty=s3aq45, valu=s3aq46)

fert$typ <- as_factor(fert$typ)

levels(fert$typ) <- 
  c("DAP", "UREA", "TSP", "CAN", "SA", "generic NPK (TZA)", "MRP")

# -------------------------------------
# reorganize data so that observations
# on fertilizer type occupy a single row
# fertilizer is unit of observation
# Data on NPK composition from Sheahan et al (2014), Food Policy
# -------------------------------------

conv <- read.csv(file.path(paste0(dataPath,"/../../.."), "Other/Fertilizer/Fert_comp.csv")) %>%
  transmute(typ=Fert_type2, n=N_share/100, p=P_share/100) %>%
  filter(typ %in% levels(fert$typ))

fert <- left_join(fert, conv)

# -------------------------------------
# organize fertilizer data for analysis

fert <- mutate(fert,
               Vfert=valu/qty,
               N=qty*n,
               P=qty*p)

fert$WPn <- fert$Vfert/fert$n
fert <- select(fert, -qty, -Vfert, -n, -p, -valu, -typ)

# join back with the rest of the data and set N and P to 0 for NA values
plot <- left_join(plot, fert) 

rm(list=c("conv", "fert"))

#######################################
############### LABOUR ################
#######################################

lab <- read_dta(file.path(dataPath, "/TZNPS1AGDTA_E/SEC_3A.dta")) %>%
  dplyr::select( hhid, plotnum, s3aq61_id1:s3aq63_9)

# remove houshold labour IDs and question ag3a_71 which we don't need
bad <- grep( "s3aq61_id", names( lab ) )
lab <- lab[, -bad]
lab <- dplyr::select( lab, -s3aq62 )

# remove variables that refer to wage paid for hired labour
# this could be added later to consider input costs
bad <- names( lab )[( length( lab )-8 ):length( lab )][seq( from=3, to=9, by=3 )]
lab <- lab[, -which( names( lab ) %in% bad )]

# create a dataframe with just family and hired labour
lab <- transmute( lab, hhid, plotnum,
                  fam_lab_days=rowSums( lab[, 3:38], na.rm=TRUE ),
                  hir_lab_days=rowSums( lab[, 39:ncol( lab )], na.rm=TRUE ) )

lab <- transmute(lab, hhid, plotnum, lab=fam_lab_days + hir_lab_days)

# doesn't make sense to have 0 labour on a plot so set values to NA
lab$lab <- ifelse(lab$lab %in% 0, NA, lab$lab)

rm(bad)

#######################################
############### GEO ###################
#######################################

# read in the household geovariables

geo08 <- read_dta(file.path(dataPath, "TZNPS1_consdta/HH.Geovariables_Y1.dta")) %>%
  select(hhid, lon=lon_modified, lat=lat_modified, dist2town=dist02, dist2market=dist03,
         dist2HQ=dist05, avgTemp=clim01, avgpPrecip=clim03)

#######################################
############### ASSETS ################
#######################################

implmt <- read_dta(file.path(dataPath, "/TZNPS1AGDTA_E/SEC_11_ALL.dta")) %>%
  dplyr::select(hhid, impcode, qty=s11q1, valu=s11q2) %>%
  filter(!qty %in% 0, !is.na(qty), !valu %in% 0, !is.na(valu)) %>%
  transmute(hhid, valu=qty*valu) %>%
  group_by(hhid) %>%
  summarise(value=sum(valu))

# -------------------------------------
# Livestock assets
# -------------------------------------

# classifications from wave 3 classification table
LR <- c("BULLS", "COWS", "STEERS", "HEIFERS", "MALE-CALVES", "FEMALE-CALVES")
SR <- c("GOATS", "SHEEP")
PIGS <- c("PIGS")
POULTRY <- c("CHICKENS", "TURKEYS")
OTHER <- c("RABBITS", "DONKEYS", "HORSES", "DOGS", "OTHER")

# read in the data
lvstock <- read_dta(file.path(dataPath, "/TZNPS1AGDTA_E/SEC_10A.dta")) %>%
  select(hhid, animal, owned = s10aq2, indigQty = s10aq4_1,
         improvBeefQty = s10aq4_2, improvDairyQty = s10aq4_3 )
lvstock$owned <- ifelse(lvstock$owned %in% 1, 1, 0)
lvstock$animal <- as_factor(lvstock$animal)

# sometimes 9999 has been used instead of NA
lvstock$indigQty <- ifelse(lvstock$indigQty == 9999, NA, lvstock$indigQty)
lvstock$improvBeefQty <- ifelse(lvstock$improvBeefQty == 9999, NA, lvstock$improvBeefQty)
lvstock$improvDairyQty <- ifelse(lvstock$improvDairyQty == 9999, NA, lvstock$improvDairyQty)
lvstock$animal <- gsub(" ", "-", lvstock$animal)
lvstock <- filter(lvstock, !is.na(animal) )

# count the number of animals of each class a household owns
lvstock_x <- select(lvstock, hhid, animal, indigQty, improvBeefQty, improvDairyQty) %>%
  melt(id = c("hhid", "animal")) %>%
  group_by(hhid, animal) %>%
  mutate(class = ifelse(animal %in% LR, "LR",
                        ifelse(animal %in% SR, "SR", 
                               ifelse(animal %in% PIGS, "PIGS_",
                                      ifelse(animal %in% POULTRY, "POULTRY",
                                             ifelse(animal %in% OTHER, "OTHER_")))))) %>%
  group_by(hhid, class) %>%
  summarise(n=sum(value, na.rm=TRUE)) %>%
  dcast(hhid ~ class)

# count the number of each animal a household owns
lvstock_y <- select(lvstock, hhid, animal, indigQty, improvBeefQty, improvDairyQty) %>%
  melt(id = c("hhid", "animal")) %>%
  group_by(hhid, animal) %>%
  summarise(n=sum(value, na.rm=TRUE)) %>%
  dcast(hhid ~ animal)

# join together
lvstock <- left_join(lvstock_x, lvstock_y)

rm("LR", "SR", "lvstock_x", "lvstock_y", "OTHER", "PIGS", "POULTRY")

# -------------------------------------
# land holdings - in wave 1
# -------------------------------------

# only 25% of the areas are GPS measured.
land <- read_dta(file.path(dataPath, "/TZNPS1AGDTA_E/SEC_2A.dta")) %>%
  rename(area_sr = s2aq4, area_gps = area)

# measurements are in acres -> change to hectacres
land$areas_gps <- land$area_gps*0.404686
land$area_sr <- land$area_sr*0.404686

# where a gps measurement is missing, replace
# with farmer estimate

land$area <- ifelse(is.na(land$area_gps), land$area_sr, land$area_gps)
land$area <- ifelse(land$area %in% 0, NA, land$area)

# create variable for ownership of the land
# and join with area information

own <- read_dta(file.path(dataPath, "/TZNPS1AGDTA_E/SEC_3A.dta")) %>%
  dplyr::select(hhid, plotnum, own=s3aq22)
own$own <- ifelse(own$own %in% 1 | own$own %in% 5, 1, 0)

land <- left_join(land, own)

# and add a variable for total land holdings, as
# a form of asset wealth

areaTotal <- group_by(land, hhid) %>%
  summarise(area_tot = sum(area))

rm(own)

#######################################
####### Extension Services ############
#######################################

ext <- read_dta(file.path(dataPath, "TZNPS1AGDTA_E/SEC_QNFLOW.dta")) %>%
  select(hhid, region, district, s13q10:s13q15)


#######################################
########## TRANSPORT COSTS ############
#######################################

tc <- read_dta(file.path(dataPath, "/TZNPS1AGDTA_E/SEC_5a.dta")) %>%
  dplyr::select(hhid, zaocode, trans=s5aq9, trans_dist=s5aq10, trans_cost=s5aq13)

#######################################
############ PANEL KEY ################
#######################################

# key for joing individuals and households across years
key <- read_dta(file.path(dataPath, "../../2012/Data/NPSY3.PANEL.KEY.dta")) %>%
  select(-UPI3) %>% rename(hhid = y1_hhid, hhid2010 = y2_hhid, hhid2012=y3_hhid)
key$hhid <- zap_empty(key$hhid)
key$hhid2010 <- zap_empty(key$hhid2010)
key$hhid2012 <- zap_empty(key$hhid2012)

#######################################
########### CROSS SECTION #############
#######################################

# joins at the household level (hhid)

TZA2008 <- left_join(location, HH08); rm(location); rm(HH08)
TZA2008 <- left_join(TZA2008, key); rm(key)
TZA2008 <- left_join(TZA2008, geo08); rm(geo08)
TZA2008 <- left_join(TZA2008, implmt); rm(implmt)
TZA2008 <- left_join(TZA2008, areaTotal); rm(areaTotal)
TZA2008 <- left_join(TZA2008, lvstock); rm(lvstock)

# joins at the plot level (hhid, plotnum)

TZA2008 <- left_join(TZA2008, oput); rm(oput)
TZA2008 <- left_join(TZA2008, plot); rm(plot)
TZA2008 <- left_join(TZA2008, tc); rm(tc)
TZA2008 <- left_join(TZA2008, lab); rm(lab)
TZA2008 <- left_join(TZA2008, land); rm(land)

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
TZA2008$pest <- ifelse(TZA2008$pest %in% 1, 1, 0) # assume NA -> no pesticide
TZA2008$trans <- ifelse(TZA2008$trans %in% 1, 1, 0) # assume NA -> no transportation for crop

# -------------------------------------
# Make some new variables
# -------------------------------------

# per hectacre
TZA2008 <- mutate(TZA2008,
                  yld=qty/area,
                  N=N/area,
                  P=P/area,
                  lab=lab/area,
                  pest_q=pest_q/area,
                  assetph=value/area_tot
)

# -------------------------------------
# Inflate 2011 prices to 2013 prices: assets, fertilizer and maize prices
# using inflation rate for 2011 and 2013. These years were selected as the main part of the survey takes place in these years.
# from world bank:
# http://data.worldbank.org/indicator/FP.CPI.TOTL.ZG/countries/TZ?display=graph
# -------------------------------------

inflation <- read.csv(file.path(paste0(dataPath,"/../../.."), "Other/Inflation/inflation.csv"))
rate2009 <- inflation$inflation[inflation$code=="TZ" & inflation$year==2009]/100
rate2010 <- inflation$inflation[inflation$code=="TZ" & inflation$year==2010]/100
rate2011 <- inflation$inflation[inflation$code=="TZ" & inflation$year==2011]/100
rate2012 <- inflation$inflation[inflation$code=="TZ" & inflation$year==2012]/100

inflate <- (1 + rate2009)*(1 + rate2010)*(1 + rate2011)*(1 + rate2012)

TZA2008 <- mutate(TZA2008,
                  asset = value*inflate,
                  assetph = assetph*inflate,
                  crop_price = crop_price*inflate,
                  WPn = WPn*inflate)

TZA2008 <- select(TZA2008, -value) %>% rename(crop_qty_harv = qty)


# add final variables

TZA2008<- mutate(TZA2008, surveyyear=2008) %>% rename(hhid2008=hhid)

rm(list=ls()[!ls() %in% c("TZA2008", "dataPath")])

# saveRDS(TZA2008, file.path(dataPath, "/../../TZA2008.rds"))


