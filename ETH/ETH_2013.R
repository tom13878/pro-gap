# -------------------------------------
# Ethiopia data 2013 - 2014 survey
# -------------------------------------

# WDswitch
dataPath <- "C:/Users/Tomas/Documents/LEI/data/ETH"
# dataPath <- "D:\\Data\\IPOP\\SurveyData\\ETH\\2013\\Data"
# wdPath <- "D:\\Dropbox\\Michiel_research\\2285000066 Africa Maize Yield Gap"
# setwd(wdPath)

library(haven)
library(stringr)
library(dplyr)

options(scipen=999)


#######################################
############### OUTPUT ################
#######################################

# WDswitch
oput <- read_dta(file.path(dataPath, "ETH201314/Post-Harvest/sect9_ph_w2.dta")) %>%
  dplyr::select(holder_id, household_id2, parcel_id, field_id,
                crop=crop_code, qty=ph_s9q05)

oput$crop <- as_factor(oput$crop)

# -------------------------------------
# add a dummy if a legume was grown
# count number of crops per field

legumes <- c("CHICK PEAS", "HARICOT BEANS", "HORSE BEANS", "LENTILS",
             "FIELD PEAS", "VETCH", "GIBTO", "SOYA BEANS", "CASTOR BEANS")

oput_x <- group_by(oput, holder_id, household_id2, parcel_id, field_id) %>%
  summarise(crop_count=sum(!is.na(crop)),
            legume = ifelse(any(crop %in% legumes), 1, 0))

oput <- left_join(oput, oput_x); rm(oput_x)

# select on maize and remove observations with quantity NA or 0
oput_maize <- oput[oput$crop %in% "MAIZE" & ! is.na(oput$qty) & !oput$qty %in% 0,]
oput_maize <- dplyr::select(oput_maize, -crop)

rm("oput", "legumes")

#######################################
############## CHEMICAL ###############
#######################################

# -------------------------------------
# variables recorded at parcel, field,
# and crop level

# parcel level
# WDswitch
parcel <- read_dta(file.path(dataPath, "ETH201314/Post-Planting/sect2_pp_w2.dta")) %>%
  dplyr::select(holder_id, household_id2, parcel_id, soil=pp_s2q14, soil_qlty=pp_s2q15)

parcel$soil_type <- as_factor(parcel$soil_type)
parcel$soil_qlty <- as_factor(parcel$soil_qlty)

# field level variables
# WDswitch

field <- read_dta(file.path(dataPath, "ETH201314/Post-Planting/sect3_pp_w2.dta")) %>%
  dplyr::select(holder_id, household_id2, parcel_id, field_id,
                inter_crop=pp_s3q03b, fallow10=pp_s3q03c, fallow_year=pp_s3q03d,
                extension=pp_s3q11, irrig=pp_s3q12, fert_any=pp_s3q14,
                other_inorg=pp_s3q20a, manure=pp_s3q21, compost=pp_s3q23,
                other_org=pp_s3q25, eros_prot=pp_s3q32, mulch=pp_s3q37) 

field$inter_crop <- ifelse(field$inter_crop %in% 2, 1, 0)
field$fallow10 <- ifelse(field$fallow10 %in% 1, 1, 0)
field$extension <- ifelse(field$extension %in% 1, 1, 0)
field$irrig <- ifelse(field$irrig %in% 1, 1, 0)
field$fert_any <- ifelse(field$fert_any %in% 1, 1, 0)
field$other_inorg <- ifelse(field$other_inorg %in% 1, 1, 0)
field$manure <- ifelse(field$manure %in% 1, 1, 0)
field$compost <- ifelse(field$compost %in% 1, 1, 0)
field$other_org <- ifelse(field$other_org %in% 1, 1, 0)
field$eros_prot <- ifelse(field$eros_prot %in% 1, 1, 0)
field$mulch <- ifelse(field$mulch %in% 1, 1, 0)

# crop level variables
# WDswitch

crop <- read_dta(file.path(dataPath, "ETH201314/Post-Planting/sect4_pp_w2.dta")) %>%
  dplyr::select(holder_id, household_id2, parcel_id, field_id, crop=crop_code,
                cropping=pp_s4q02, month=pp_s4q12_a, crop_area=pp_s4q03,
                herb=pp_s4q06, fung=pp_s4q07, seed_type=pp_s4q11, 
                seed_qty=pp_s4q11b)

crop$crop <- as_factor(crop$crop)
crop$cropping <- as_factor(crop$cropping)
crop$month <- as_factor(crop$month)
crop$herb <- ifelse(crop$herb %in% 1, 1, 0)
crop$fung <- ifelse(crop$fung %in% 1, 1, 0)
crop$seed_type <- ifelse(crop$seed_type %in% 2, 1, 0)

# select only maize crops
crop <- filter(crop, crop %in% "MAIZE") %>% select(-crop)

# -------------------------------------
# unit of observation is not fertilizer
# WDswitch

fert1 <- read_dta(file.path(dataPath, "ETH201314/Post-Planting/sect3_pp_w2.dta")) %>%
  dplyr::select(holder_id, household_id2, parcel_id, field_id, typ=pp_s3q15, qty=pp_s3q16_a,
                purch=pp_s3q16b, purch_kg=pp_s3q16c, valu=pp_s3q16d)
  
fert1$typ <- ifelse(fert1$typ %in% 1, "UREA", NA)
fert1$purch <- ifelse(fert1$purch %in% 1, 1, 0)

# WDswitch  
fert2 <- read_dta(file.path(dataPath, "ETH201314/Post-Planting/sect3_pp_w2.dta")) %>%
  dplyr::select(holder_id, household_id2, parcel_id, field_id, typ=pp_s3q18, qty=pp_s3q19_a,
                purch=pp_s3q19b, purch_kg=pp_s3q19c, valu=pp_s3q19d) 

fert2$typ <- ifelse(fert2$typ %in% 1, "DAP", NA)
fert2$purch <- ifelse(fert2$purch %in% 1, 1, 0)

# -------------------------------------
# read in nitrogen conversion file

conv <- read.csv(paste(dataPath, "Fert_comp.csv", sep="/")) %>%
  transmute(typ=Fert_type2, n=N_share/100, p=P_share/100) %>%
  filter(typ %in% c("UREA", "DAP"))


fert1 <- left_join(fert1, conv)
fert2 <- left_join(fert2, conv)

# -------------------------------------
# If purchased amount of nitrogen is zero 
# set to NA to avoid Inf values

fert1$purch_kg <- ifelse(fert1$purch_kg == 0, NA, fert1$purch_kg)
fert2$purch_kg <- ifelse(fert2$purch_kg == 0, NA, fert2$purch_kg)

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

# if vfert is 0 change to NA
fert1$Vfert <- ifelse(fert1$Vfert == 0, NA, fert1$Vfert)
fert2$Vfert <- ifelse(fert2$Vfert == 0, NA, fert2$Vfert)

fert <- rbind(fert1, fert2) %>% unique

fert$Pn <- fert$Vfert/fert$n

fert <- group_by(fert, holder_id, household_id2, parcel_id, field_id) %>%
  summarise(N=sum(Qn, na.rm=TRUE), P=sum(Qp, na.rm=TRUE),
            WPn=sum((Qn/N)*Pn, na.rm=TRUE)) %>%
  mutate(WPn = replace(WPn, WPn==0, NA))

rm(fert1, fert2, conv)

#######################################
############### LABOUR ################
#######################################

# POST PLANTING labour
# WDswitch
pp_lab <- read_dta(file.path(dataPath, "ETH201314/Post-Planting/sect3_pp_w2.dta")) %>%
  dplyr::select(holder_id, household_id2, parcel_id, field_id, pp_s3q27_a:pp_s3q29_f) %>%
  transmute(holder_id, household_id2, parcel_id, field_id,
            id1=pp_s3q27_a, lab1=pp_s3q27_b*pp_s3q27_c,
            id2=pp_s3q27_e, lab2=pp_s3q27_f*pp_s3q27_g,
            id3=pp_s3q27_i, lab3=pp_s3q27_j*pp_s3q27_k,
            id4=pp_s3q27_m, lab4=pp_s3q27_n*pp_s3q27_o,
            id5=pp_s3q27_q, lab5=pp_s3q27_r*pp_s3q27_s,
            id6=pp_s3q27_u, lab6=pp_s3q27_v*pp_s3q27_w,
            id7=pp_s3q27_y, lab7=pp_s3q27_z*pp_s3q27_ca,
            hirM=pp_s3q28_a*pp_s3q28_b,
            hirF=pp_s3q28_d*pp_s3q28_e,
            hirC=pp_s3q28_g*pp_s3q28_h,
            OHHlabM=pp_s3q29_a*pp_s3q29_b,
            OHHlabF=pp_s3q29_c*pp_s3q29_d,
            OHHlabC=pp_s3q29_e*pp_s3q29_f
            )

# make all NA values zero
pp_lab[is.na(pp_lab)] <- 0

# sum all labour across a single plot - all measured in days
pp_lab <- transmute(pp_lab, holder_id, household_id2, parcel_id, field_id,
                  plant_lab=lab1 + lab2 + lab3 + lab4 + lab5 + lab6 + lab7 +
                    hirM + hirF + hirC + OHHlabM + OHHlabF + OHHlabC)

# presumably if crop was planted then some labour
# was used. Therefore set all 0's for plant_lab
# to NA

pp_lab$plant_lab[pp_lab$plant_lab %in% 0] <- NA

# POST HARVEST
# WDswitch
ph_lab <- read_dta(file.path(dataPath, "ETH201314/Post-Harvest/sect10_ph_w2.dta")) %>%
  dplyr::select(holder_id, household_id2, parcel_id, field_id,
                crop=crop_code, ph_s10q01_a:ph_s10q03_f) %>%
  transmute(holder_id, household_id2, parcel_id, field_id, crop,
            id1=ph_s10q02_a, lab1=ph_s10q02_b*ph_s10q02_c,
            id2=ph_s10q02_e, lab2=ph_s10q02_f*ph_s10q02_g,
            id3=ph_s10q02_i, lab3=ph_s10q02_j*ph_s10q02_k,
            id4=ph_s10q02_m, lab4=ph_s10q02_n*ph_s10q02_o,
            hirM=ph_s10q01_a*ph_s10q01_b,
            hirF=ph_s10q01_d*ph_s10q01_e,
            hirC=ph_s10q01_g*ph_s10q01_h,
            OHHlabM=ph_s10q03_a*ph_s10q03_b,
            OHHlabF=ph_s10q03_c*ph_s10q03_d,
            OHHlabC=ph_s10q03_e*ph_s10q03_f
  )

# -------------------------------------
# filter for maize crops only because
# the post harvest labour is recorded
# at the crop level

ph_lab$crop <- as_factor(ph_lab$crop)
ph_lab <- filter(ph_lab, crop %in% "MAIZE") %>% select(-crop)

# make all NA values zero
ph_lab[is.na(ph_lab)] <- 0

# sum all labour across a single plot - all measured in days
ph_lab <- transmute(ph_lab, holder_id, household_id2, parcel_id, field_id,
                  harv_lab=lab1 + lab2 + lab3 + lab4 +
                    hirM + hirF + hirC + OHHlabM + OHHlabF + OHHlabC) 

ph_lab$harv_lab[ph_lab$harv_lab %in% 0] <- NA

#######################################
############### GEO ###################
#######################################

# WDswitch
load(file.path(dataPath, "ETH201314/ETH_geo_total_2013.RData"))
geo <- geo.total.plot %>% 
  dplyr::select(holder_id, household_id2, ea_id2, parcel_id, field_id, lon, lat, SPEI, RootDepth, region=NAME_1,
                AEZ=ssa_aez09, ph=ph_sd1_sd3, ph2=ph_sd1_sd5,
                SOC=SOC_sd1_sd3, SOC2=SOC_sd1_sd5, rain=gsRainfall, 
                YA, YW, YP, everything()) %>%
  unique()

#######################################
############### AREAs #################
#######################################

# -------------------------------------
# imputed and original gps measurements
# included
# WDswitch
areas <- read_dta(paste(dataPath, "ETH201314/areas_ETH2013.dta", sep="/"))
areas <- select(areas, holder_id, household_id2,
                parcel_id, field_id, area_gps, area_gps_mi50,
                area_farmer=area_sr)

areas$area_gps <- ifelse(areas$area_gps %in% 0, NA, areas$area_gps)
areas$area_gps_mi50 <- ifelse(areas$area_gps_mi50 %in% 0, NA, areas$area_gps_mi50)

areaTotal <- group_by(areas, household_id2) %>%
  summarise(area_tot = sum(area_gps_mi50, na.rm=TRUE))
  
#######################################
########### SOCIO/ECONOMIC ############
#######################################

# WDswitch
se <- read_dta(file.path(dataPath, "ETH201314/Household/sect1_hh_w2.dta")) %>%
  filter(hh_s1q02 %in% 1) %>% # 1 for head of household
  dplyr::select(household_id2, sex=hh_s1q03, age=hh_s1q04_a,
                rural)

se$sex <- ifelse(se$sex %in% 2, 1, 0) # 1/0 female/male
se$rural <- ifelse(se$rural %in% 1, 1, 0)

# ownership of a parcel
# WDswitch
own <- read_dta(file.path(dataPath, "ETH201314/Post-Planting/sect2_pp_w2.dta")) %>%
  dplyr::select(holder_id, household_id2, parcel_id, title=pp_s2q04)
own$title <- ifelse(own$title %in% 1, 1, 0)  
  
#######################################
########### CROSS SECTION #############
#######################################

# -------------------------------------
# crop level joins

ETH2013 <- left_join(oput_maize, crop) 
ETH2013 <- left_join(ETH2013, ph_lab)

# -------------------------------------
# field level joins

ETH2013 <- left_join(ETH2013, fert) 
ETH2013 <- left_join(ETH2013, areas)
ETH2013 <- left_join(ETH2013, pp_lab) 
ETH2013 <- left_join(ETH2013, field)

# -------------------------------------
# parcel level joins

ETH2013 <- left_join(ETH2013, parcel)
ETH2013 <- left_join(ETH2013, own) 

# -------------------------------------
# household level joins

ETH2013 <- left_join(ETH2013, se)
ETH2013 <- left_join(ETH2013, areaTotal)
ETH2013 <- left_join(ETH2013, geo)

# -------------------------------------
# Make some new variables
# -------------------------------------

# per hectacre
ETH2013 <- mutate(ETH2013,
              yld=qty/area_gps_mi50,
              N=N/area_gps_mi50,
              P=P/area_gps_mi50
)

# squared values

rm(list=ls()[!ls() %in% "ETH2013"])

# save to file
# save(ETH2013, file=".\\Analysis\\ETH\\Data\\ETH2013_data.RData")
