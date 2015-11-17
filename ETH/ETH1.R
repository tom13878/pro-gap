# -------------------------------------
# Ethiopia data 2013 - 2014 survey
# -------------------------------------

# WDswitch
# dataPath <- "D:\\Data\\IPOP\\SurveyData\\"
# wdPath <- "D:\\Dropbox\\Michiel_research\\2285000066 Africa Maize Yield Gap"
# setwd(wdPath)

dataPath <- "C:/Users/Tomas/Documents/LEI/data/ETH"

library(haven)
library(stringr)
library(plyr)
library(dplyr)

options(scipen=999)


#######################################
############### OUTPUT ################
#######################################

oput <- read_dta(file.path(dataPath, "Post-Harvest/sect9_ph_w2.dta")) %>%
  dplyr::select(household_id, parcel_id, field_id, crop=crop_code, qty=ph_s9q05)

# -------------------------------------
# table(oput$household_id %in% "")
# FALSE  TRUE 
# 29318   257 
# need to remove the values that have missing households.

oput <- filter(oput, !(oput$household_id %in% ""))
oput$crop <- as_factor(oput$crop)

# -------------------------------------
# add a dummy if a legume was grown
# count number of crops per field

legumes <- c("CHICK PEAS", "HARICOT BEANS", "HORSE BEANS", "LENTILS",
             "FIELD PEAS", "VETCH", "GIBTO", "SOYA BEANS", "CASTOR BEANS")
oput <- ddply(oput, .(household_id, parcel_id, field_id), transform,
              crop_count=length(crop[!is.na(crop)]),
              legume=ifelse(any(crop %in% legumes), 1, 0))

# select on maize and remove observations with quantity NA or 0
oput_maze <- oput[oput$crop %in% "MAIZE" & ! is.na(oput$qty) & !oput$qty %in% 0,]

rm("oput", "legumes")

# -------------------------------------
# get prices for the output, in a 
# different section from quantities
# very few people sell their maize.
prc <- read_dta(file.path(dataPath, "Post-Harvest/sect11_ph_w2.dta")) %>%
  select(household_id, crop_name, sold=ph_s11q01, qty_sold_kg=ph_s11q03_a,
         qty_sold_g=ph_s11q03_b, prc=ph_s11q04) %>%
  filter(crop_name %in% "MAIZE")


#######################################
############## CHEMICAL ###############
#######################################

# -------------------------------------
# Some plot specific variables are 
# recorded at the parcel level, some
# at the field level and othrs at the
# crop level

parcel <- read_dta(file.path(dataPath, "Post-Planting/sect2_pp_w2.dta")) %>%
  select(household_id, parcel_id, soil_type=pp_s2q14, soil_qlty=pp_s2q15)

field <- read_dta(file.path(dataPath, "Post-Planting/sect3_pp_w2.dta")) %>%
  select(household_id, parcel_id, field_id, cropping=pp_s3q03b, fallow10=pp_s3q03c,
         fallow_year=pp_s3q03d, irrig=pp_s3q12, fert_any=pp_s3q14, manure=pp_s3q21,
         compost=pp_s3q23, other_organic=pp_s3q25, erosprot=pp_s3q32)

crop <- read_dta(file.path(dataPath, "Post-Planting/sect4_pp_w2.dta")) %>%
  select(household_id, parcel_id, field_id, crop_code, cropping=pp_s4q02,
         crop_area=pp_s4q03, herb=pp_s4q06, fung=pp_s4q07, seed_type=pp_s4q11,
         seed_qty=pp_s4q11b)
  

# -------------------------------------
# unit of observation is not fertilizer

fert1 <- read_dta(file.path(dataPath, "Post-Planting/sect3_pp_w2.dta")) %>%
  dplyr::select(household_id, parcel_id, field_id, typ=pp_s3q15, qty=pp_s3q16_a,
                purch=pp_s3q16b, purch_kg=pp_s3q16c, valu=pp_s3q16d)
fert1$typ <- ifelse(fert1$typ %in% 1, "UREA", NA)
fert1$purch <- ifelse(fert1$purch %in% 1, 1, 0)
  
fert2 <- read_dta(file.path(dataPath, "Post-Planting/sect3_pp_w2.dta")) %>%
  dplyr::select(household_id, parcel_id, field_id, typ=pp_s3q18, qty=pp_s3q19_a,
                purch=pp_s3q19b, purch_kg=pp_s3q19c, valu=pp_s3q19d) 
fert2$typ <- ifelse(fert2$typ %in% 1, "DAP", NA)
fert2$purch <- ifelse(fert2$purch %in% 1, 1, 0)

# -------------------------------------
# nitrogen conversions from Michiel's 
# csv file

typ <- c("DAP", "UREA")
n <- c(0.18, 0.46)
p <- c(0.2, NA)
comp <- data.frame(typ, n, p)

fert1 <- left_join(fert1, comp)
fert2 <- left_join(fert2, comp)

fert1 <- mutate(fert1,
               Vfert=valu/purch_kg,
               Qn=qty*n,
               Qp=qty*p)
fert2 <- mutate(fert2,
                Vfert=valu/purch_kg,
                Qn=qty*n,
                Qp=qty*p)

# if Qn is zero change to NA
fert1$Qn <- ifelse(fert1$Qn == 0, NA, fert1$Qn)
fert2$Qn <- ifelse(fert2$Qn == 0, NA, fert2$Qn)

# if vfert is 0 chenge to NA
fert1$Vfert <- ifelse(fert1$Vfert == 0, NA, fert1$Vfert)
fert2$Vfert <- ifelse(fert2$Vfert == 0, NA, fert2$Vfert)

fert <- rbind(fert1, fert2) %>% unique

fert$Pn <- fert$Vfert/fert$n

fert <- group_by(fert, household_id, parcel_id, field_id) %>%
  summarise(N=sum(Qn, na.rm=TRUE),
            WPn=sum((Qn/N)*Pn, na.rm=TRUE)) %>%
  mutate(WPn = replace(WPn, WPn==0, NA))


#######################################
############### LABOUR ################
#######################################

# POST PLANTING

lab1 <- read_dta(file.path(dataPath, "Post-Planting/sect3_pp_w2.dta")) %>%
  select(household_id, parcel_id, field_id, pp_s3q27_a:pp_s3q29_f) %>%
  transmute(household_id, parcel_id, field_id,
            id1=pp_s3q27_a, lab1=pp_s3q27_b*pp_s3q27_c,
            id2=pp_s3q27_e, lab2=pp_s3q27_f*pp_s3q27_g,
            id3=pp_s3q27_i, lab3=pp_s3q27_j*pp_s3q27_k,
            id4=pp_s3q27_m, lab4=pp_s3q27_n*pp_s3q27_o,
            hirM=pp_s3q28_a*pp_s3q28_b,
            hirF=pp_s3q28_d*pp_s3q28_e,
            hirC=pp_s3q28_g*pp_s3q28_h,
            OHHlabM=,
            OHHlabF=,
            OHHlabC=
            )

# make all NA values zero
lab1[is.na(lab1)] <- 0

# sum all labour across a single plot - all measured in days
lab1 <- transmute(lab1, household_id, parcel_id, field_id,
                  plant_lab=lab1 + lab2 + lab3 + lab4 +
                    hirM + hirF + hirC + OHHlabM + OHHlabF + OHHlabC)



# POST HARVEST

lab2 <- read_dta(file.path(dataPath, "Post-Harvest/sect10_ph_w2.dta")) %>%
  select(household_id, parcel_id, field_id, ph_s10q01_a:ph_s10q03_f) %>%
  transmute(household_id, parcel_id, field_id,
            id1=ph_s10q02_a, lab1=ph_s10q02_b*ph_s10q02_c,
            id2=ph_s10q02_e, lab2=ph_s10q02_f*ph_s10q02_g,
            id3=ph_s10q02_i, lab3=ph_s10q02_j*ph_s10q02_k,
            id4=ph_s10q02_m, lab4=ph_s10q02_n*ph_s10q02_o,
            hirM=ph_s10q01_b,
            hirF=ph_s10q01_e,
            hirC=ph_s10q01_h,
            OHHlabM=ph_s10q03_b,
            OHHlabF=ph_s10q03_d,
            OHHlabC=ph_s10q03_f
  )

# make all NA values zero
lab2[is.na(lab2)] <- 0

# sum all labour across a single plot - all measured in days
lab2 <- transmute(lab2, household_id, parcel_id, field_id,
                  harv_lab=lab1 + lab2 + lab3 + lab4 +
                    hirM + hirF + hirC + OHHlabM + OHHlabF + OHHlabC)




#######################################
############### ASSETS ################
#######################################



#######################################
############### GEO ###################
#######################################

geo <- read_dta(file.path(dataPath, "Geodata/Pub_ETH_HouseholdGeovars_Y2.dta")) %>%
  dplyr::select(household_id, lon=lon_dd_mod, lat=lat_dd_mod) %>%
  unique()


#######################################
############### AREAs #################
#######################################

# -------------------------------------
# some NA values in the gps measurements

areas <- read_dta(file.path(dataPath, "Post-Planting/sect3_pp_w2.dta")) %>%
  select(area_sr=pp_s3q02_a, area_sr_unit=pp_s3q02_c, area_gps=pp_s3q05_a)

#######################################
########### SOCIO/ECONOMIC ############
#######################################

own <- read_dta(file.path(dataPath, "Post-Planting/sect2_pp_w2.dta")) %>%
  select(household_id, parcel_id, cert=pp_s2q04)
own$cert <- ifelse(own$cert %in% 1, 1, 0)  
  
              
#######################################
########### CROSS SECTION #############
#######################################