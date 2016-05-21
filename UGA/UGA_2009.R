#######################################
########### UGANDA 2009-10 ############
#######################################

# Tom
dataPath <- "C:/Users/Tomas/Documents/LEI/data/UGA/2009_10/Data"

# Michiel
# dataPath <- ""

# Anne
# dataPath <- ""

# Vincent
# dataPath <- ""

library(haven)
library(stringr)
library(reshape2)
library(dplyr)

options(scipen=999)

#######################################
############## LOCATION ###############
#######################################

test <- read_dta(file.path(dataPath, "GSEC1.dta")) %>%
  select(HHID, region, urban, regurb, h1aq1, h1aq1_05)

#######################################
########### SOCIO/ECONOMIC ############
#######################################

HH09 <- read_dta(file.path(dataPath, "GSEC2.dta")) %>%
  select(HHID, PID, status=h2q4, sex=h2q3,
         yob=h2q9c, age=h2q8)

HH09$status <- toupper(as_factor(HH09$status))
HH09$sex <- toupper(as_factor(HH09$sex))
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
  select(HHID, PID, ed_any=h4q5, grade=h4q7)

ed$ed_any <- ifelse(ed$ed_any %in% c(2, 3), 1, 0) # ever went to school

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
  summarise(credit=any(value %in% 1))

credit$credit <- ifelse(credit$credit, 1, 0)

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
         tc = a5bq10)

oput$qty <- oput$qty * oput$qty_unit2kg
oput$qty_sold <- oput$qty_sold * oput$qty_unit2kg
oput <- select(oput, HHID, parcel_id, plot_id, crop_name, crop_code, qty, qty_sold, value, tc)

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
############# CHEMICAL ################
#######################################

plot <- read_dta(file.path(dataPath, "TZNPS2AGRDTA/AG_SEC3A.dta")) %>%
  dplyr::select(y2_hhid, plotnum, zaocode, soil=ag3a_09, slope_farmer=ag3a_16, irrig=ag3a_17, title=ag3a_27,
                manure=ag3a_39, pest=ag3a_58, pest_q=ag3a_60_1, pest_q_unit=ag3a_60_2, fallow_year=ag3a_21, fallow=ag3a_22)

plot$maize <- ifelse(plot$zaocode %in% 11, 1, 0)
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



