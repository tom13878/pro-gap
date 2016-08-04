#######################################
########### UGANDA 2010-11 ############
#######################################

if(Sys.info()["user"] == "Tomas"){
  dataPath <- "C:/Users/Tomas/Documents/LEI/data/UGA/2010_11/Data"
} else {
  dataPath <- "W:/LEI/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/UGA/2010_11/Data/"
}

library(haven)
library(reshape2)
library(dplyr)

options(scipen=999)

#######################################
############## LOCATION ###############
#######################################

location <- read_dta(file.path(dataPath, "GSEC1.dta")) %>%
  select(HHID, REGCODE=region, rural=urban, DISNAME=h1aq1)
location$rural <- ifelse(location$rural %in% 0, 1, 0)
location$REGNAME <- toupper(as_factor(location$REGCODE))
location$REGCODE <- as.numeric(location$REGCODE)

#######################################
########### SOCIO/ECONOMIC ############
#######################################

HH10 <- read_dta(file.path(dataPath, "GSEC2.dta")) %>%
  select(HHID, indidy2=PID, status=h2q4, sex=h2q3,
         yob=h2q9c, age=h2q8)

HH10$status <- toupper(as_factor(HH10$status))
HH10$yob <- as.integer(HH10$yob)
HH10$sex <- as.integer(HH10$sex) # female = 1

# make a new variable cage (cut age = cage) which splits
# individuals according to their age group with
# breaks at 15, 55 and the max age

HH10$cage <- cut(HH10$age, breaks = c(0, 15, 55, max(HH10$age, na.rm=TRUE)),
                 labels=c("0-15", "16-55", "56+"), include.lowest = TRUE, right = TRUE)

# education of household members and sum
# of education of all household members
# between the ages of 15 and 55

ed <- read_dta(file.path(dataPath, "GSEC4.dta")) %>%
  select(HHID, indidy2=PID, ed_any=h4q5, grade=h4q7)

ed$ed_any <- ifelse(ed$ed_any %in% c(2, 3), 1, 0) # ever went to school

# join with HH10 dataframe
HH10 <- left_join(HH10, ed)

# summarise the data: get sum of education
# of household members 15-55 and number of
# household members 15:55

HH10_x <- group_by(HH10, HHID) %>%
  summarise(N1555=sum(cage %in% "16-55"))
HH10 <- left_join(HH10, HH10_x); rm(HH10_x)

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

# credit <- read_dta(file.path(dataPath, "GSEC13A.dta")) %>%
#   select(HHID, h13q01:h13q03) %>% 
#   melt(id = "HHID") %>%
#   group_by(HHID) %>%
#   summarise(credit=any(value %in% 1))

#credit$credit <- ifelse(credit$credit, 1, 0)

HH10 <- left_join(HH10, death) 

rm(ed, death)

#######################################
############### OUTPUT ################
#######################################

oput <- read_dta(file.path(dataPath, "AGSEC5A.dta")) %>%
  select(HHID, parcel_id=prcid, plot_id=pltid, crop_code=cropID,
         qty=a5aq6a, qty_unit=a5aq6c, qty_unit2kg = a5aq6d,
         qty_sold=a5aq7a, qty_sold_unit=a5aq7c, value = a5aq8,
         trans_cost = a5aq10)

oput$crop_code <- as.integer(oput$crop_code)
oput$qty <- oput$qty * oput$qty_unit2kg
oput$qty_sold <- oput$qty_sold * oput$qty_unit2kg
oput <- select(oput, HHID, parcel_id, plot_id, crop_code, qty, qty_sold, value, trans_cost)

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
oput <- unique(oput) # remove duplicates
rm(list=c("legumes", "cashCropNPerm", "cashCropsPerm",
          "CTR", "fruit", "vegetables"))

#######################################
############### INPUTS ################
#######################################

plot <- read_dta(file.path(dataPath, "AGSEC3A.dta")) %>%
  select(HHID, parcel_id=prcid, plot_id=pltid, manure=a3aq4, manure_qty = a3aq5, 
         inorg=a3aq14, pest=a3aq26, pest_unit=a3aq28a,
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

# make a single plot labour variable
lab <- c("fam_lab_people", "fam_lab_days", "hir_lab_men_days", "hir_lab_woman_days", "hir_lab_child_days")
plot[, lab][is.na(plot[, lab])] <- 0;rm(lab)
plot <- mutate(plot,
               fam_lab=fam_lab_people * fam_lab_days,
               hir_lab=hir_lab_men_days+hir_lab_woman_days+hir_lab_child_days)
plot$lab <- plot$fam_lab + plot$hir_lab

# there are many variables in the plot section.
# select those that are close enough to the
# other countries and make most sense
plot <- transmute(plot, HHID, parcel_id, plot_id, manure, manure_qty,
               inorg, pest, lab)
plot$HHID <- as.character(plot$HHID)

# crop level variables
crop <- read_dta(file.path(dataPath, "AGSEC4A.dta")) %>% 
  select(HHID, parcel_id=prcid, plot_id=pltid, crop_code=cropID,
         inter_crop=a4aq7, share=a4aq8, area_share=a4aq9, hybrd=a4aq13)

crop$HHID <- as.character(crop$HHID)
crop$crop_code <- as.integer(crop$crop_code)
crop$inter_crop <- ifelse(crop$inter_crop %in% 1, 1, 0)
crop$hybrd <- ifelse(crop$hybrd %in% 2, 1, 0)

parcel <- read_dta(file.path(dataPath, "AGSEC2A.dta")) %>% 
  select(HHID, parcel_id=prcid, soil=a2aq18, irrig=a2aq20,
         slope_farmer=a2aq21)

parcel$soil <- as_factor(parcel$soil)
parcel$irrig <- ifelse(parcel$irrig %in% 1, 1, 0)
parcel$slope_farmer <- as_factor(parcel$slope_farmer)

# -------------------------------------
# fertilizer

fert <- read_dta(file.path(dataPath, "AGSEC3A.dta")) %>%
  select(HHID, parcel_id=prcid, plot_id=pltid, typ=a3aq15,
         qty=a3aq16, purch_kg=a3aq18, valu=a3aq19)
fert$typ <- as_factor(fert$typ)

# -------------------------------------
# read in nitrogen conversion file

conv <- read.csv(file.path(paste0(dataPath,"/../../.."), "Other/Fertilizer/Fert_comp.csv")) %>%
  transmute(typ=Fert_type2, n=N_share/100, p=P_share/100) 

fert <- left_join(fert, conv)

# -------------------------------------
# If purchased amount of nitrogen is zero 
# set to NA to avoid Inf values

fert$purch_kg <- ifelse(fert$purch_kg == 0, NA, fert$purch_kg)

fert <- mutate(fert,
               Vfert=valu/purch_kg,
               Qn=qty*n,
               Qp=qty*p)

# if Qn is zero change to NA
fert$Qn <- ifelse(fert$Qn == 0, NA, fert$Qn)

# if vfert is 0 change to NA
fert$Vfert <- ifelse(fert$Vfert == 0, NA, fert$Vfert)

fert$Pn <- fert$Vfert/fert$n

fert <- group_by(fert, HHID, parcel_id, plot_id) %>%
  summarise(N=sum(Qn, na.rm=TRUE), P=sum(Qp, na.rm=TRUE),
            WPn=sum((Qn/N)*Pn, na.rm=TRUE)) %>%
  mutate(WPn = replace(WPn, WPn==0, NA))
fert$HHID <- as.character(fert$HHID)
rm(conv)

#######################################
############### GEO ###################
#######################################

geo10 <- read_dta(file.path(dataPath, "UNPS_Geovars_1011.dta")) %>%
  select(HHID, lon=lon_mod, lat=lat_mod, dist2Rd=dist_road,
         dist2town=dist_popcenter, dist2market=dist_market,
         dist2HQ=dist_admctr, avgTemp=af_bio_1, avgPrecip=af_bio_12)

#######################################
############### AREAs #################
#######################################

areas <- read_dta(file.path(dataPath, "AGSEC2A.dta")) %>%  
  select(HHID, parcel_id=prcid, area_farmer=a2aq5, own=a2aq25,
         area_gps=a2aq4, fallow_year=a2aq14, fallow_years=a2aq15a)

areas$area_gps <- ifelse(areas$area_gps %in% 0, NA, areas$area_gps)
areas$area <- ifelse(is.na(areas$area_gps), areas$area_farmer, areas$area_gps)
areas$own <- as_factor(areas$own)

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
LR <- c("CALVES","COWS", "BULLS", "HEIFER", "OXEN", "INDIGENOUS", "EXOTIC/CROSS")
# SR <- c("MALE-GOATS", "FEMALE-GOATS", "MALE-SHEEP", "FEMALE-SHEEP")
# PIGS <- c("PIGS")
OTHER <- c("DONKEYS", "MULES-/-HORSES")

# read in the data for large animals (cows etc)
lvstock_large <- read_dta(file.path(dataPath, "AGSEC6A.dta")) %>%
  select(HHID, animal=a6aq2, owned=a6aq4, qty=a6aq5a)
lvstock_large$owned <- ifelse(lvstock_large$owned %in% 1, 1, 0)

# read in the data for small animals (sheep etc)
# lvstock_small <- read_dta(file.path(dataPath, "AGSEC6B.dta")) %>%
#   select(HHID, animal=a6bq3, owned=a6bq4, qty=a6bq5a)
# lvstock_small$owned <- ifelse(lvstock_small$owned %in% 1, 1, 0)

# combine and remove white space
# lvstock <- rbind(lvstock_large, lvstock_small)
lvstock <- lvstock_large
lvstock$animal <- toupper(lvstock$animal)
lvstock$animal <- gsub(" ", "-", lvstock$animal)
lvstock <- lvstock[!lvstock$animal %in% "",]

# count the number of animals of each class a household owns
lvstock_x <- select(lvstock, HHID, animal, qty) %>%
  melt(id = c("HHID", "animal")) %>%
  group_by(HHID, animal) %>%
  mutate(class = ifelse(animal %in% LR, "LR",
                        ifelse(animal %in% OTHER, "OTHER_"))) %>%
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

rm("LR", "lvstock_large",
   "lvstock_x", "lvstock_y", "OTHER")

#######################################
########### CROSS SECTION #############
#######################################

# joins at the household level (HHID)

UGA2010 <- left_join(HH10, location); rm(location); rm(HH10)
UGA2010 <- left_join(UGA2010, geo10); rm(geo10)
UGA2010 <- left_join(UGA2010, asset); rm(asset)
UGA2010 <- left_join(UGA2010, lvstock); rm(lvstock)
UGA2010 <- left_join(UGA2010, areaTotal); rm(areaTotal)

# joins at the parcel level
UGA2010 <- left_join(UGA2010, areas); rm(areas)
UGA2010 <- left_join(UGA2010, parcel); rm(parcel)

# joins at the plot level
UGA2010 <- left_join(UGA2010, oput); rm(oput)
UGA2010 <- left_join(UGA2010, plot); rm(plot)
UGA2010 <- left_join(UGA2010, fert); rm(fert)
UGA2010 <- left_join(UGA2010, crop); rm(crop)

# -------------------------------------
# Make some new variables
# -------------------------------------

# per hectacre
UGA2010 <- mutate(UGA2010,
                  yld=qty/area_gps,
                  lab=lab/area_gps,
                  assetph=asset/area_tot)

# many households report not having owned
# any livestock. This is likely because
# they are urban dwellers, or just not
# livestock farmers. In this case set livestock
# values to 0

lvstock <- c("LR", "OTHER_", "BULLS", "CALVES", "COWS", "DONKEYS",
             "EXOTIC/CROSS", "HEIFER", "INDIGENOUS", "MULES-/-HORSES","OXEN")
UGA2010[, lvstock][is.na(UGA2010[, lvstock])] <- 0;rm(lvstock)

# -------------------------------------
# remove some variables which may be of
# use later on but which are not 
# required now
# -------------------------------------


# -------------------------------------
# Inflate 2010 prices to 2011 prices:
# assets, fertilizer and maize prices
# using inflation rate for 2011 and 2013.
# from world bank:
# http://data.worldbank.org/indicator/FP.CPI.TOTL.ZG/countries/TZ?display=graph
# -------------------------------------

inflation <- read.csv(file.path(paste0(dataPath,"/../../.."), "Other/Inflation/inflation.csv"))
rate2011 <- inflation$inflation[inflation$code=="UG" & inflation$year==2011]/100
inflate <- rate2011

UGA2010 <- mutate(UGA2010,
                  asset = asset*inflate,
                  assetph = assetph*inflate,
                  crop_price = crop_price*inflate)

# add final variables

UGA2010 <- mutate(UGA2010, surveyyear=2010) %>% rename(hhid2010=HHID)

rm(dataPath, inflate, inflation, rate2011)

