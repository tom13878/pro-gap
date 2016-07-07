#######################################
########### UGANDA 2009-10 ############
#######################################

# Tom
dataPath <- "C:/Users/Tomas/Documents/LEI/data/UGA/2009_10/Data"

# LEI Path
# dataPath <- "W:/LEI/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/UGA/2009_10/Data/"

library(haven)
library(reshape2)
library(dplyr)

options(scipen=999)

#######################################
############## LOCATION ###############
#######################################

location <- read_dta(file.path(dataPath, "GSEC1.dta")) %>%
  select(HHID, REGCODE=region, rural=urban, DISTCODE=h1aq1)
location$rural <- ifelse(location$rural %in% 0, 1, 0)
location$REGNAME <- toupper(as_factor(location$REGCODE))
location$REGNAME <- gsub(" WITHOUT KAMPALA", "", location$REGNAME)
location$REGCODE <- as.numeric(location$REGCODE)

# in the first wave the district names
# are not available in the data. These are
# contained in an external file called
# reg_dis_lsmsUGA.csv and were copied from
# the codebook
REGDIS <- read.csv(file.path(paste0(dataPath,"/../../.."), "Other/Spatial/UGA/reg_dis_lsmsUGA.csv"))
REGDIS <- select(REGDIS, REGCODE=LSMSREGCODE, DISTNAME=LSMSDISTNAME, DISTCODE=LSMSDISTCODE)

location <- left_join(location, REGDIS)

#######################################
########### SOCIO/ECONOMIC ############
#######################################

HH09 <- read_dta(file.path(dataPath, "GSEC2.dta")) %>%
  select(HHID, indidy1=PID, status=h2q4, sex=h2q3,
         yob=h2q9c, age=h2q8)

HH09$status <- toupper(as_factor(HH09$status))
HH09$sex <- as.integer(HH09$sex) # female = 1
HH09$yob <- as.integer(HH09$yob)

# make a new variable cage (cut age = cage) which splits
# individuals according to their age group with
# breaks at 15, 55 and the max age

HH09$cage <- cut(HH09$age, breaks = c(0, 15, 55, max(HH09$age, na.rm=TRUE)),
                 labels=c("0-15", "16-55", "56+"), include.lowest = TRUE, right = TRUE)

# education of household members and sum
# of education of all household members
# between the ages of 15 and 55

ed <- read_dta(file.path(dataPath, "GSEC4.dta")) %>%
  select(HHID, indidy1=PID, ed_any=h4q5, grade=h4q7)

ed$ed_any <- ifelse(ed$ed_any %in% c(2, 3), 1, 0) # ever went to school
ed$grade <- as_factor(ed$grade)

# join with HH10 dataframe
HH09 <- left_join(HH09, ed)

# summarise the data: get sum of education
# of household members 15-55 and number of
# household members 15:55

HH09_x <- group_by(HH09, HHID) %>%
  summarise(N1555=sum(cage %in% "16-55"))
HH09 <- left_join(HH09, HH09_x); rm(HH09_x)

# -------------------------------------
# death in the family
# -------------------------------------

death <- read_dta(file.path(dataPath, "GSEC16.dta")) %>%
  filter(h16q00 %in% c("112", "113")) %>%
  select(HHID, death = h16q01) %>% 
  group_by(HHID) %>%
  summarise(death=any(death %in% 1)) 
death$death <- ifelse(death$death, 1, 0)

# -------------------------------------
# membership to a credit group
# -------------------------------------

credit <- read_dta(file.path(dataPath, "GSEC13.dta")) %>%
  select(HHID, h13q01:h13q03) %>% 
  melt(id = "HHID") %>%
  group_by(HHID) %>%
  summarise(SACCO=any(value %in% 1))

credit$SACCO <- ifelse(credit$SACCO, 1, 0)

HH09 <- left_join(HH09, death) 
HH09 <- left_join(HH09, credit) 

rm(ed, credit, death)

#######################################
############### OUTPUT ################
#######################################

oput <- read_dta(file.path(dataPath, "AGSEC5B.dta")) %>%
  select(HHID, parcel_id = a5bq1, plot_id = a5bq3, crop_name = a5bq4, crop_code = a5bq5,
         qty=a5bq6a, qty_unit=a5bq6c, qty_unit2kg = a5bq6d,
         qty_sold=a5bq7a, qty_sold_unit=a5bq7c, value = a5bq8,
         trans_cost = a5bq10)

oput$qty <- oput$qty * oput$qty_unit2kg
oput$qty_sold <- oput$qty_sold * oput$qty_unit2kg
oput <- select(oput, HHID, parcel_id, plot_id, crop_name, crop_code, qty, qty_sold, value, trans_cost)

# change ids to integer
oput$parcel_id <- as.integer(oput$parcel_id)
oput$plot_id <- as.integer(oput$plot_id)

# -------------------------------------
# create dummy variables for crop groups
# (fruit, cash crops (permanent),
# Cereals/Tubers/Roots, cash crops (not permanent),
# vegetables, legumes)
# -------------------------------------

fruit <- c(700, 710, 741, 742, 744, 750, 760, 770, 780)
cashCropsPerm <- c(810, 820, 830, 880, 720, 510, 850, 870) # permanent cash crops
CTR <- c(111, 112, 120, 130, 141, 150, 440, 610, 620, 630, 640, 650, 840) # Cereals, Tubers, Roots
cashCropNPerm <- c(520, 530) # non permanent cash crops
vegetables <- c(410, 420, 430, 450, 460, 470)
legumes <- c(210, 221, 222, 223, 224)

oput_x <- group_by(oput, HHID, parcel_id, plot_id) %>%
  summarise(crop_count=length(unique(crop_code[!is.na(crop_code)])),
            fruit=ifelse(any(crop_code %in% fruit), 1, 0),
            cashCropsPerm=ifelse(any(crop_code %in% cashCropsPerm), 1, 0),
            CTR=ifelse(any(crop_code %in% CTR), 1, 0),
            cashCropNPerm=ifelse(any(crop_code %in% cashCropNPerm), 1, 0),
            vegetables=ifelse(any(crop_code %in% vegetables), 1, 0),
            legume=ifelse(any(crop_code %in% legumes), 1, 0),
            maize_=ifelse(any(crop_code %in% 130), 1, 0), # maize has crop code 11
            wheat=ifelse(any(crop_code %in% 111), 1, 0)) # wheat has crop code 16

oput <- left_join(oput, oput_x); rm(oput_x)

# exclude farmers who responded they produced zero crop, or did not respond (NA)

oput <- oput[! is.na(oput$qty) & !oput$qty %in% 0, ]
oput$qty_sold[oput$qty_sold %in% 0] <- NA
oput$crop_price <- oput$value/oput$qty_sold
oput$value <- NULL 
oput$parcel_id <- as.character(oput$parcel_id)
oput <- unique(oput) # remove duplicates
rm(list=c("legumes", "cashCropNPerm", "cashCropsPerm",
          "CTR", "fruit", "vegetables"))

#######################################
############### INPUTS ################
#######################################

plot <- read_dta(file.path(dataPath, "AGSEC3A.dta")) %>%
  select(HHID, parcel_id=a3aq1, plot_id=a3aq3, manure=a3aq4, manure_qty = a3aq5, 
                inorg=a3aq14, typ=a3aq15, inorg_qty=a3aq16, inorg_purch=a3aq17,
                inorg_purch_qty=a3aq18, inorg_value=a3aq19, pest=a3aq26, pest_unit=a3aq28a,
                pest_qty=a3aq28b, pest_purch=a3aq29, pest_purch_qty=a3aq30, pest_purch_value=a3aq31,
                fam_lab_people=a3aq38, fam_lab_days=a3aq39, hir_lab=a3aq41, hir_lab_men_days=a3aq42a,
                hir_lab_woman_days=a3aq42b, hir_lab_child_days=a3aq42c)

plot$manure <- ifelse(plot$manure %in% 1, 1, 0)
plot$inorg <- ifelse(plot$inorg %in% 1, 1, 0)
plot$inorg_purch <- ifelse(plot$inorg_purch %in% 1, 1, 0)
plot$pest <- ifelse(plot$pest %in% 1, 1, 0)
plot$pest_purch <- ifelse(plot$pest_purch %in% 1, 1, 0)
plot$typ <- as_factor(plot$typ)
plot$pest_unit <- as_factor(plot$pest_unit)
plot$hir_lab <- as_factor(plot$hir_lab)
plot$HHID <- as.character(plot$HHID)
plot$parcel_id <- as.character(plot$parcel_id)

# make a single plot labour variable
lab <- c("fam_lab_people", "fam_lab_days", "hir_lab_men_days", "hir_lab_woman_days", "hir_lab_child_days")
plot[, lab][is.na(plot[, lab])] <- 0;rm(lab)
plot <- mutate(plot,
               fam_lab=fam_lab_people * fam_lab_days,
               hir_lab=hir_lab_men_days+hir_lab_woman_days+hir_lab_child_days)
plot$lab <- plot$fam_lab + plot$hir_lab

# crop level variables
crop <- read_dta(file.path(dataPath, "AGSEC4A.dta")) %>% 
  select(HHID, parcel_id=a4aq2, plot_id=a4aq4, crop_name=a4aq5, crop_code=a4aq6,
         inter_crop=a4aq7, share=a4aq8, area_share=a4aq9, hybrd=a4aq13)
crop$crop_code <- as.integer(crop$crop_code)
crop$HHID <- as.character(crop$HHID)

crop$crop_name <- zap_empty(crop$crop_name)
crop$inter_crop <- ifelse(crop$inter_crop %in% 1, 1, 0)
crop$hybrd <- ifelse(crop$hybrd %in% 2, 1, 0)
crop$HHID <- as.character(crop$HHID)

parcel <- read_dta(file.path(dataPath, "AGSEC2A.dta")) %>% 
  select(HHID, parcel_id=a2aq2, soil=a2aq18, irrig=a2aq20,
         slope_farmer=a2aq21)

parcel$irrig <- ifelse(parcel$irrig %in% 1, 1, 0)
parcel$soil <- as_factor(parcel$soil)
parcel$slope_farmer <- as_factor(parcel$slope_farmer)
parcel$HHID <- as.character(parcel$HHID)
parcel$parcel_id <- as.character(parcel$parcel_id)

#######################################
############### GEO ###################
#######################################

geo09 <- read_dta(file.path(dataPath, "UNPS_Geovars_0910.dta")) %>%
  select(HHID, lon=lon_mod, lat=lat_mod, dist2Rd=dist_road,
         dist2town=dist_popcenter, dist2market=dist_market,
         dist2HQ=dist_admctr, avgTemp=af_bio_1, avgpPrecip=af_bio_12)

#######################################
############### AREAs #################
#######################################

areas <- read_dta(file.path(dataPath, "AGSEC2A.dta")) %>%  
  select(HHID, parcel_id=a2aq2, area_farmer=a2aq5, own=a2aq25,
         area_gps=a2aq4, fallow_year=a2aq14, fallow_years=a2aq15)

areas$own <- as_factor(areas$own)
areas$area_gps <- ifelse(areas$area_gps %in% 0, NA, areas$area_gps)
areas$area <- ifelse(is.na(areas$area_gps), areas$area_farmer, areas$area_gps)
areas$parcel_id <- as.character(parcel$parcel_id)

# calcualte the households total land holdings
areaTotal <- group_by(areas, HHID) %>%
  summarise(area_tot = sum(area))

areaTotal$area_tot <- ifelse(areaTotal$area_tot %in% 0, NA, areaTotal$area_tot)

#######################################
############### ASSETS ################
#######################################

asset <- read_dta(file.path(dataPath, "GSEC14.dta")) %>%
  select(HHID, code=h14q2, own=h14q3, qty=h14q4, value=h14q5) %>%
  filter(!qty %in% 0, !is.na(qty), !value %in% 0, !is.na(value)) %>%
  transmute(HHID, value=qty*value) %>%
  group_by(HHID) %>%
  summarise(asset=sum(value))

# -------------------------------------
# Livestock assets
# -------------------------------------

# classifications from wave 3 classification table
LR <- c("CALVES","BULLS-AND-OXEN", "HEIFER-AND-COWS", "STEERS", "HEIFERS", "MALE-CALVES", "FEMALE-CALVES")
SR <- c("MALE-GOATS", "FEMALE-GOATS", "MALE-SHEEP", "FEMALE-SHEEP")
PIGS <- c("PIGS")
OTHER <- c("DONKEYS", "MULES-/-HORSES")

# read in the data for large animals (cows etc)
lvstock_large <- read_dta(file.path(dataPath, "AGSEC6A.dta")) %>%
  select(HHID, animal=a6aq2, owned=a6aq4, qty=a6aq5)
lvstock_large$owned <- ifelse(lvstock_large$owned %in% 1, 1, 0)

# read in the data for small animals (sheep etc)
lvstock_small <- read_dta(file.path(dataPath, "AGSEC6B.dta")) %>%
  select(HHID, animal=a6bq2, owned=a6bq4, qty=a6bq5)
lvstock_small$owned <- ifelse(lvstock_small$owned %in% 1, 1, 0)

# combine and remove white space
lvstock <- rbind(lvstock_large, lvstock_small)
lvstock$animal <- toupper(lvstock$animal)
lvstock$animal <- gsub(" ", "-", lvstock$animal)
lvstock <- lvstock[!lvstock$animal %in% "",]

# count the number of animals of each class a household owns
lvstock_x <- select(lvstock, HHID, animal, qty) %>%
  melt(id = c("HHID", "animal")) %>%
  group_by(HHID, animal) %>%
  mutate(class = ifelse(animal %in% LR, "LR",
                        ifelse(animal %in% SR, "SR", 
                               ifelse(animal %in% PIGS, "PIGS_",
                                      ifelse(animal %in% OTHER, "OTHER_"))))) %>%
  group_by(HHID, class) %>%
  summarise(n=sum(value, na.rm=TRUE)) %>%
  dcast(HHID ~ class)

# count the number of each animal a household owns
lvstock_y <- select(lvstock, HHID, animal, qty) %>%
  melt(id = c("HHID", "animal")) %>%
  group_by(HHID, animal) %>%
  summarise(n=sum(value, na.rm=TRUE)) %>%
  dcast(HHID ~ animal)

# join together
lvstock <- left_join(lvstock_x, lvstock_y)
lvstock[is.na(lvstock)] <- 0

rm("LR", "SR", "lvstock_small", "lvstock_large",
   "lvstock_x", "lvstock_y", "OTHER", "PIGS")

#######################################
########### CROSS SECTION #############
#######################################

# joins at the household level (HHID)

UGA2009 <- left_join(HH09, location); rm(location); rm(HH09)
UGA2009 <- left_join(UGA2009, geo09); rm(geo09)
UGA2009 <- left_join(UGA2009, asset); rm(asset)
UGA2009 <- left_join(UGA2009, lvstock); rm(lvstock)
UGA2009 <- left_join(UGA2009, areaTotal); rm(areaTotal)

# joins at the parcel level
UGA2009 <- left_join(UGA2009, areas); rm(areas)
UGA2009 <- left_join(UGA2009, parcel); rm(parcel)

# joins at the plot level
UGA2009 <- left_join(UGA2009, oput); rm(oput)
UGA2009 <- left_join(UGA2009, plot); rm(plot)

# -------------------------------------
# Make some new variables and changes
# -------------------------------------

# per hectacre
UGA2009 <- mutate(UGA2009,
                  yld=qty/area_gps,
                  lab=lab/area_gps,
                  assetph=asset/area_tot
)

# many households report not having owned
# any livestock. This is likely because
# they are urban dwellers, or just not
# livestock farmers. In this case set livestock
# values to 0

lvstock <- c("LR", "OTHER_", "PIGS_", "SR", "BULLS-AND-OXEN",
             "CALVES", "DONKEYS", "FEMALE-GOATS", "FEMALE-SHEEP",
             "HEIFER-AND-COWS", "MALE-GOATS", "MALE-SHEEP", "MULES-/-HORSES",
             "PIGS")
UGA2009[, lvstock][is.na(UGA2009[, lvstock])] <- 0;rm(lvstock)


# -------------------------------------
# remove some variables which may be of
# use later on but which are not 
# required now
# -------------------------------------

UGA2009 <- select(UGA2009, -yob,
                  -typ, -inorg_qty, -inorg_purch,
                  -inorg_purch_qty, -inorg_value,
                  -pest_unit, -pest_qty, -pest_purch,
                  -pest_purch_qty, -pest_purch_value,
                  -fam_lab_people, -fam_lab_days,
                  - hir_lab, -hir_lab_men_days, 
                  -hir_lab_woman_days, -hir_lab_child_days,
                  -fam_lab)

# -------------------------------------
# Inflate 2009 prices to 2011 prices: assets, fertilizer and maize prices
# using inflation rate for 2011 and 2013. These years were selected as the main part of the survey takes place in these years.
# from world bank:
# http://data.worldbank.org/indicator/FP.CPI.TOTL.ZG/countries/TZ?display=graph
# -------------------------------------

inflation <- read.csv(file.path(paste0(dataPath,"/../../.."), "Other/Inflation/inflation.csv"))
rate2010 <- inflation$inflation[inflation$code=="UG" & inflation$year==2010]/100
rate2011 <- inflation$inflation[inflation$code=="UG" & inflation$year==2011]/100
inflate <- (1 + rate2011)*(1 + rate2011)

UGA2009 <- mutate(UGA2009,
                  asset = asset*inflate,
                  assetph = assetph*inflate,
                  crop_price = crop_price*inflate)

# add final variables

UGA2009 <- mutate(UGA2009, surveyyear=2009) %>% rename(hhid2009=HHID)

rm(list=ls()[!ls() %in% c("UGA2009", "dataPath")])

# saveRDS(UGA2009, file.path(dataPath, "/../../UGA2009.rds"))
