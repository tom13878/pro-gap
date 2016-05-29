#######################################
########### UGANDA 2011-12 ############
#######################################

# Tom
dataPath <- "C:/Users/Tomas/Documents/LEI/data/UGA/2011_12/Data"

# LEI server dataPath
# dataPath <- ""

library(haven)
library(reshape2)
library(dplyr)

options(scipen=999)

#######################################
############## LOCATION ###############
#######################################

location <- read_dta(file.path(dataPath, "GSEC1.dta")) %>%
  select(HHID, region, district=h1aq1, rural=urban)
location$rural <- ifelse(location$rural %in% 0, 1, 0)
location$region <- as_factor(location$region)

#######################################
########### SOCIO/ECONOMIC ############
#######################################

HH11 <- read_dta(file.path(dataPath, "GSEC2.dta")) %>%
  select(HHID, PID, status=h2q4, sex=h2q3,
         yob=h2q9c, age=h2q8)

HH11$status <- toupper(as_factor(HH11$status))
HH10$sex <- as.integer(HH10$sex) # female = 1
HH11$yob <- as.integer(HH11$yob)

# make a new variable cage (cut age = cage) which splits
# individuals according to their age group with
# breaks at 15, 55 and the max age

HH11$cage <- cut(HH11$age, breaks = c(0, 15, 55, max(HH11$age, na.rm=TRUE)),
                 labels=c("0-15", "16-55", "56+"), include.lowest = TRUE, right = TRUE)

# education of household members and sum
# of education of all household members
# between the ages of 15 and 55

ed <- read_dta(file.path(dataPath, "GSEC4.dta")) %>%
  select(HHID, PID, ed_any=h4q5, grade=h4q7)

ed$ed_any <- ifelse(ed$ed_any %in% c(2, 3), 1, 0) # ever went to school

# join with HH10 dataframe
HH11 <- left_join(HH11, ed)

# summarise the data: get sum of education
# of household members 15-55 and number of
# household members 15:55

HH11_x <- group_by(HH11, HHID) %>%
  summarise(N1555=sum(cage %in% "16-55"))
HH11 <- left_join(HH11, HH11_x); rm(HH11_x)

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
# 
# credit$credit <- ifelse(credit$credit, 1, 0)

HH11 <- left_join(HH11, death) 
HH11 <- left_join(HH11, credit) 

rm(ed, credit, death)

#######################################
############### OUTPUT ################
#######################################

oput <- read_dta(file.path(dataPath, "AGSEC5A.dta")) %>%
  select(HHID, parcel_id=parcelID, plot_id=plotID, crop_code=cropID,
         qty=a5aq6a, qty_unit=a5aq6c, qty_unit2kg = a5aq6d,
         qty_sold=a5aq7a, qty_sold_unit=a5aq7c, value = a5aq8,
         tc = a5aq10)

oput$crop_code <- as.integer(oput$crop_code)
oput$qty <- oput$qty * oput$qty_unit2kg
oput$qty_sold <- oput$qty_sold * oput$qty_unit2kg
oput <- select(oput, HHID, parcel_id, plot_id, crop_code, qty, qty_sold, value, tc)
oput$HHID <- as.character(oput$HHID)

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

rm(list=c("legumes", "cashCropNPerm", "cashCropsPerm",
          "CTR", "fruit", "vegetables"))

#######################################
############### INPUTS ################
#######################################

plot <- read_dta(file.path(dataPath, "AGSEC3A.dta")) %>%
  select(HHID, parcel_id=parcelID, plot_id=plotID, manure=a3aq4, manure_qty = a3aq5, 
         inorg=a3aq13, typ=a3aq14, inorg_qty=a3aq15, inorg_purch=a3aq16,
         inorg_purch_qty=a3aq17, inorg_value=a3aq18, pest=a3aq22, pest_unit=a3aq24a,
         pest_qty=a3aq24b, pest_purch=a3aq25, pest_purch_qty=a3aq26, pest_purch_value=a3aq27,
         fam_lab_days=a3aq32, hir_lab_men_days=a3aq35a,
         hir_lab_woman_days=a3aq35b, hir_lab_child_days=a3aq35c)

plot$manure <- ifelse(plot$manure %in% 1, 1, 0)
plot$inorg <- ifelse(plot$inorg %in% 1, 1, 0)
plot$inorg_purch <- ifelse(plot$inorg_purch %in% 1, 1, 0)
plot$pest <- ifelse(plot$pest %in% 1, 1, 0)
plot$pest_purch <- ifelse(plot$pest_purch %in% 1, 1, 0)
plot$typ <- as_factor(plot$typ)
plot$pest_unit <- as_factor(plot$pest_unit)
plot$hir_lab_men_days <- ifelse(is.na(plot$hir_lab_men_days), 0, plot$hir_lab_men_days)
plot$hir_lab_woman_days <- ifelse(is.na(plot$hir_lab_woman_days), 0, plot$hir_lab_woman_days)
plot$hir_lab_child_days <- ifelse(is.na(plot$hir_lab_child_days), 0, plot$hir_lab_child_days)

# there are many variables in the plot section.
# select those that are close enough to the
# other countries and make most sense
plot <- transmute(plot, HHID, parcel_id, plot_id, manure, manure_qty,
                  inorg, pest, famLab = fam_lab_days,
                  hirLab = hir_lab_men_days + hir_lab_woman_days + hir_lab_child_days)
plot$HHID <- as.character(plot$HHID)

# crop level variables
crop <- read_dta(file.path(dataPath, "AGSEC4A.dta")) %>% 
  select(HHID, parcel_id=parcelID, plot_id=plotID, crop_code=cropID,
         inter_crop=a4aq8, share=a4aq7, area_share=a4aq9, hybrd=a4aq13)
crop$HHID <- as.character(crop$HHID)

crop$crop_code <- as.integer(crop$crop_code)
crop$inter_crop <- ifelse(crop$inter_crop %in% 1, 1, 0)
crop$hybrd <- ifelse(crop$hybrd %in% 2, 1, 0)

parcel <- read_dta(file.path(dataPath, "AGSEC2A.dta")) %>% 
  select(HHID, parcel_id=parcelID, soil=a2aq17, irrig=a2aq20,
         slope_farmer=a2aq19)

parcel$soil <- as_factor(parcel$soil)
parcel$irrig <- as_factor(parcel$irrig)
parcel$slope_farmer <- as_factor(parcel$slope_farmer)
parcel$HHID <- as.character(parcel$HHID)

#######################################
############### GEO ###################
#######################################

geo11 <- read_dta(file.path(dataPath, "UNPS_Geovars_1112.dta")) %>%
  select(HHID, lon=lon_mod, lat=lat_mod, dist2Rd=dist_road,
         dist2town=dist_popcenter, dist2market=dist_market,
         dist2HQ=dist_admctr, avgTemp=af_bio_1, avgPrecip=af_bio_12)

#######################################
############### AREAs #################
#######################################

areas <- read_dta(file.path(dataPath, "AGSEC2A.dta")) %>%  
  select(HHID, parcel_id=parcelID, area_farmer=a2aq5, own=a2aq23,
         area_gps=a2aq4, fallow_year=a2aq12, fallow_years=a2aq13a)

areas$HHID <- as.character(areas$HHID)
areas$own <- as_factor(areas$own)
areas$area_gps <- ifelse(areas$area_gps %in% 0, NA, areas$area_gps)
areas$area <- ifelse(is.na(areas$area_gps), areas$area_farmer, areas$area_gps)

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
  summarise(value=sum(value))

# -------------------------------------
# Livestock assets
# -------------------------------------

# classifications from wave 3 classification table
LR <- c("EXOTIC-CALVES","EXOTIC-COWS", "EXOTIC-BULLS", "EXOTIC-HEIFER",
        "EXOTIC-OXEN", "INDIGENOUS-CALVES", "INDIGENOUS-BULLS",
        "INDIGENOUS-OXEN", "INDIGENOUS-HEIFER", "INDIGENOUS-COWS")
# SR <- c("MALE-GOATS", "FEMALE-GOATS", "MALE-SHEEP", "FEMALE-SHEEP")
# PIGS <- c("PIGS")
OTHER <- c("INDIGENOUS-DONKEYS", "INDIGENOUS-MULES/HORSES")

# read in the data for large animals (cows etc)
lvstock_large <- read_dta(file.path(dataPath, "AGSEC6A.dta")) %>%
  select(HHID, animal=lvstid, owned=a6aq2, qty=a6aq3a)
lvstock_large$owned <- ifelse(lvstock_large$owned %in% 1, 1, 0)
lvstock_large$animal <- as_factor(lvstock_large$animal)

# read in the data for small animals (sheep etc) -> not codes!
# lvstock_small <- read_dta(file.path(dataPath, "AGSEC6B.dta")) %>%
#    select(HHID, animal=lvstid, owned=a6bq2, qty=a6bq3a)
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
lvstock$HHID <- as.character(lvstock$HHID)

rm("LR", "lvstock_large",
   "lvstock_x", "lvstock_y", "OTHER")

#######################################
########### CROSS SECTION #############
#######################################

# joins at the household level (HHID)

UGA2011 <- left_join(location, HH11); rm(location); rm(HH11)
UGA2011 <- left_join(UGA2011, geo11); rm(geo11)
UGA2011 <- left_join(UGA2011, asset); rm(asset)
UGA2011 <- left_join(UGA2011, lvstock); rm(lvstock)

# joins at the parcel level
UGA2011 <- left_join(UGA2011, areaTotal); rm(areaTotal)
UGA2011 <- left_join(UGA2011, areas); rm(areas)
UGA2011 <- left_join(UGA2011, parcel); rm(parcel)

# joins at the plot level
UGA2011 <- left_join(UGA2011, oput); rm(oput)
UGA2011 <- left_join(UGA2011, plot); rm(plot)
UGA2011 <- left_join(UGA2011, crop); rm(crop)

rm(list=ls()[!ls() %in% "UGA2011"])