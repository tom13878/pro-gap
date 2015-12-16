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
library(plyr)
library(dplyr)

options(scipen=999)


#######################################
############### OUTPUT ################
#######################################

# WDswitch
oput <- read_dta(file.path(dataPath, "Post-Harvest/sect9_ph_w2.dta")) %>%
  dplyr::select(holder_id, household_id2, parcel_id, field_id,
                crop=crop_code, qty=ph_s9q05)

oput$crop <- as_factor(oput$crop)

# -------------------------------------
# add a dummy if a legume was grown
# count number of crops per field

legumes <- c("CHICK PEAS", "HARICOT BEANS", "HORSE BEANS", "LENTILS",
             "FIELD PEAS", "VETCH", "GIBTO", "SOYA BEANS", "CASTOR BEANS")
oput <- ddply(oput, .(holder_id, household_id2, parcel_id, field_id), transform,
              crop_count=length(crop[!is.na(crop)]),
              legume=ifelse(any(crop %in% legumes), 1, 0))

# select on maize and remove observations with quantity NA or 0
oput_maize <- oput[oput$crop %in% "MAIZE" & ! is.na(oput$qty) & !oput$qty %in% 0,]
oput_maize <- dplyr::select(oput_maize, -crop)

rm("oput", "legumes")

# -------------------------------------
# The value received for maize is in a
# different section of the questionnaire.
# very few farmers record the price they
# received for maize sold.

prc <- read_dta(file.path(dataPath, "Post-Harvest/sect11_ph_w2.dta")) %>%
   select(holder_id, household_id2, crop_name, sold=ph_s11q01, qty_sold_kg=ph_s11q03_a,
          qty_sold_g=ph_s11q03_b, prc=ph_s11q04) %>%
   filter(crop_name %in% "MAIZE")

# check how many prices available for maize
sum(!is.na(prc$prc)) # 303

#######################################
############## CHEMICAL ###############
#######################################

# -------------------------------------
# variables recorded at parcel, field,
# and crop level

# parcel level
# WDswitch
parcel <- read_dta(file.path(dataPath, "Post-Planting/sect2_pp_w2.dta")) %>%
  dplyr::select(holder_id, household_id2, parcel_id, soil_type=pp_s2q14, soil_qlty=pp_s2q15)

parcel$soil_type <- as_factor(parcel$soil_type)
parcel$soil_qlty <- as_factor(parcel$soil_qlty)

# field level variables
# WDswitch

field <- read_dta(file.path(dataPath, "Post-Planting/sect3_pp_w2.dta")) %>%
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

crop <- read_dta(file.path(dataPath, "Post-Planting/sect4_pp_w2.dta")) %>%
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

fert1 <- read_dta(file.path(dataPath, "Post-Planting/sect3_pp_w2.dta")) %>%
  dplyr::select(holder_id, household_id2, parcel_id, field_id, typ=pp_s3q15, qty=pp_s3q16_a,
                purch=pp_s3q16b, purch_kg=pp_s3q16c, valu=pp_s3q16d)
  
fert1$typ <- ifelse(fert1$typ %in% 1, "UREA", NA)
fert1$purch <- ifelse(fert1$purch %in% 1, 1, 0)

# WDswitch  
fert2 <- read_dta(file.path(dataPath, "Post-Planting/sect3_pp_w2.dta")) %>%
  dplyr::select(holder_id, household_id2, parcel_id, field_id, typ=pp_s3q18, qty=pp_s3q19_a,
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

rm(fert1, fert2, comp, n, p, typ)

#######################################
############### LABOUR ################
#######################################

# POST PLANTING
# WDswitch
lab1 <- read_dta(file.path(dataPath, "Post-Planting/sect3_pp_w2.dta")) %>%
  dplyr::select(holder_id, household_id2, parcel_id, field_id, pp_s3q27_a:pp_s3q29_f) %>%
  transmute(holder_id, household_id2, parcel_id, field_id,
            id1=pp_s3q27_a, lab1=pp_s3q27_b*pp_s3q27_c,
            id2=pp_s3q27_e, lab2=pp_s3q27_f*pp_s3q27_g,
            id3=pp_s3q27_i, lab3=pp_s3q27_j*pp_s3q27_k,
            id4=pp_s3q27_m, lab4=pp_s3q27_n*pp_s3q27_o,
            hirM=pp_s3q28_a*pp_s3q28_b,
            hirF=pp_s3q28_d*pp_s3q28_e,
            hirC=pp_s3q28_g*pp_s3q28_h,
            OHHlabM=pp_s3q29_b,
            OHHlabF=pp_s3q29_d,
            OHHlabC=pp_s3q29_f
            )

# make all NA values zero
lab1[is.na(lab1)] <- 0

# sum all labour across a single plot - all measured in days
lab1 <- transmute(lab1, holder_id, household_id2, parcel_id, field_id,
                  plant_lab=lab1 + lab2 + lab3 + lab4 +
                    hirM + hirF + hirC + OHHlabM + OHHlabF + OHHlabC)

# POST HARVEST
# WDswitch
lab2 <- read_dta(file.path(dataPath, "Post-Harvest/sect10_ph_w2.dta")) %>%
  dplyr::select(holder_id, household_id2, parcel_id, field_id, crop=crop_code, ph_s10q01_a:ph_s10q03_f) %>%
  transmute(holder_id, household_id2, parcel_id, field_id, crop,
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

# -------------------------------------
# filter for maize crops only because
# the post harvest labour is recorded
# at the crop level

lab2$crop <- as_factor(lab2$crop)
lab2 <- filter(lab2, crop %in% "MAIZE") %>% select(-crop)


# make all NA values zero
lab2[is.na(lab2)] <- 0

# sum all labour across a single plot - all measured in days
lab2 <- transmute(lab2, holder_id, household_id2, parcel_id, field_id,
                  harv_lab=lab1 + lab2 + lab3 + lab4 +
                    hirM + hirF + hirC + OHHlabM + OHHlabF + OHHlabC) 

#######################################
############### ASSETS ################
#######################################

# -------------------------------------
# capital assets - but no prices!

# assets <- read_dta(file.path(dataPath, "Household/sect10_hh_w2.dta")) %>%
#   select(household_id2, item_code=hh_s10q00, item_name=hh_s10q0a, qty=hh_s10q01
# )

# -------------------------------------
# livestock assets - but no prices!
# and livestock are recorded in more than
# one place in livestock questionnaire
# some observations record age of livestock
# some record quantity of a certain animal


#######################################
############### GEO ###################
#######################################

# WDswitch
geo <- read_dta(file.path(dataPath, "Geodata/Pub_ETH_HouseholdGeovars_Y2.dta")) %>%
  dplyr::select(household_id2, lon=lon_dd_mod, lat=lat_dd_mod)

#######################################
############### AREAs #################
#######################################

# -------------------------------------
# imputed and original gps measurements
# included
# WDswitch
areas <- read.csv(paste(dataPath, "areas_ETH2013.csv", sep="/"))
areas <- select(areas, holder_id, household_id2,
                parcel_id, field_id, area_gps, area_gps_mi50)

add0 <- str_length(areas$holder_id) == 15
areas$holder_id <- ifelse(add0, paste0("0", areas$holder_id), areas$holder_id)

add0 <- str_length(areas$household_id2) == 17
areas$household_id2 <- ifelse(add0, paste0("0", areas$household_id2), areas$household_id2)

#######################################
########### SOCIO/ECONOMIC ############
#######################################

# WDswitch
se <- read_dta(file.path(dataPath, "Household/sect1_hh_w2.dta")) %>%
  filter(hh_s1q02 %in% 1) %>% # 1 for head of household
  dplyr::select(household_id2, sex=hh_s1q03, age=hh_s1q04_a,
                rural)

se$sex <- ifelse(se$sex %in% 2, 1, 0) # 1/0 female/male
se$rural <- ifelse(se$rural %in% 1, 1, 0)

# ownership of a parcel
# WDswitch
own <- read_dta(file.path(dataPath, "Post-Planting/sect2_pp_w2.dta")) %>%
  dplyr::select(holder_id, household_id2, parcel_id, cert=pp_s2q04)
own$cert <- ifelse(own$cert %in% 1, 1, 0)  
  
#######################################
########### CROSS SECTION #############
#######################################

# -------------------------------------
# crop level joins

data2013 <- left_join(oput_maize, crop) 
data2013 <- left_join(data2013, lab2)

# -------------------------------------
# field level joins

data2013 <- left_join(data2013, fert) 
data2013 <- left_join(data2013, areas)
data2013 <- left_join(data2013, lab1) 
data2013 <- left_join(data2013, field)

# -------------------------------------
# parcel level joins

data2013 <- left_join(data2013, parcel)
data2013 <- left_join(data2013, own) 

# -------------------------------------
# household level joins

data2013 <- left_join(data2013, geo)
data2013 <- left_join(data2013, se)

# -------------------------------------
# Make some new variables
# -------------------------------------

data2013 <- ddply(data2013, .(holder_id, household_id2), transform, area_tot=sum(area))

# per hectacre
data2013 <- mutate(data2013,
              yld=qty/area,
              N=N/area,
              P=P/area
)

rm(list=ls()[!ls() %in% "data2013"])

# save to file
save(data2013, file=".\\Analysis\\ETH\\Data\\ETH2013_data.RData")
