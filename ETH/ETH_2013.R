# -------------------------------------
# Ethiopia data 2013 - 2014 survey
# -------------------------------------

if(Sys.info()["user"] == "Tomas"){
  dataPath <- "C:/Users/Tomas/Documents/LEI/data/ETH/2013/Data"
} else {
  dataPath <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/ETH/2013/Data"
}

library(haven)
library(dplyr)

options(scipen=999)

#######################################
############## LOCATION ###############
#######################################

# file containing the zone, region and district

location <- read_dta(file.path(dataPath, "Household/sect1_hh_w2.dta")) %>%
  select(household_id2, REGCODE = saq01, ZONECODE = saq02, ea_id2, rural) %>%
  unique
location$type <- factor(location$rural, levels=1:3, labels=c("RURAL", "SMALL TOWN", "LARGE TOWN"))
location$rural <- ifelse(location$rural %in% 1, 1, 0)
location$REGCODE <- as.integer(location$REGCODE)

# match up with the names from the survey (prepared in a seperate file)

REGZONE <- read.csv(file.path(paste0(dataPath,"/../../.."), "Other/Spatial/ETH/REGZONEETH.csv"))

# join with household identifications
location <- left_join(location, REGZONE)

rm(REGZONE)

#######################################
########### SOCIO/ECONOMIC ############
#######################################

HH13 <- read_dta(file.path(dataPath, "Household/sect1_hh_w2.dta")) %>%
  select(household_id, household_id2, individual_id, individual_id2,
         ea_id, ea_id2, status=hh_s1q02, sex=hh_s1q03, age=hh_s1q04_a,
         religion=hh_s1q07, marital=hh_s1q08)

HH13$status <- toupper(as_factor(HH13$status))
HH13$religion <- toupper(as_factor(HH13$religion))
HH13$sex <- toupper(as_factor(HH13$sex)) 
HH13$marital <- toupper(as_factor(HH13$marital)) 

# make a new variable cage (cut age = cage) which splits
# individuals according to their age group with
# breaks at 15, 55 and the max age

HH13$cage <- cut(HH13$age, breaks = c(0, 15, 55, max(HH13$age, na.rm=TRUE)),
                 labels=c("0-15", "16-55", "56+"), include.lowest = TRUE, right = TRUE)

# education of household members and sum
# of education of all household members
# between the ages of 15 and 55

education <- read_dta(file.path(dataPath, "Household/sect2_hh_w2.dta")) %>%
  select(household_id2, individual_id, individual_id2, ea_id, ea_id2,
         literate=hh_s2q02, ed_any=hh_s2q03)

education$literate <- toupper(as_factor(education$literate))
education$ed_any <- toupper(as_factor(education$ed_any))

# summarise the data: get sum of education
# of household members 15-55 and number of
# household members 15:55

HH13_x <- group_by(HH13, household_id2) %>%
  summarise(N1555=sum(cage %in% "16-55"),
            family_size=n())

# -------------------------------------
# death in the family
# -------------------------------------

death <- read_dta(file.path(dataPath, "Household/sect8_hh_w2.dta")) %>%
  select(household_id2, code=hh_s8q00, death=hh_s8q01) %>% 
  filter(code %in% c("101", "101b")) %>% select(-code) %>%
  group_by(household_id2) %>%
  summarise(death=ifelse(any(death %in% 1), 1, 0))

HH13 <- left_join(HH13, education) %>%
  left_join(HH13_x) %>%
  left_join(death); rm(education, HH13_x, death)

#######################################
############### OUTPUT ################
#######################################

oput <- read_dta(file.path(dataPath, "/Post-Harvest/sect9_ph_w2.dta")) %>%
  select(holder_id, household_id2, parcel_id, field_id,
         crop_code, crop_qty_harv=ph_s9q05, inter_crop=ph_s9q01,
         harv_area=ph_s9q09, harv_month_start=ph_s9q07_a, harv_month_end=ph_s9q07_b)

oput$crop_name <- toupper(as_factor(oput$crop_code))
oput$crop_code <- as.integer(oput$crop_code)
oput$harv_month_start <- toupper(as_factor(oput$harv_month_start))
oput$harv_month_end <- toupper(as_factor(oput$harv_month_end))
oput$inter_crop <- toupper(as_factor(oput$inter_crop))

# -------------------------------------
# add a dummy if a legume was grown
# count number of crops per field
# crop codes taken from appendix in 
# the BID

legumes <- c(11:18, 36, 118)

oput_x <- group_by(oput, holder_id, household_id2, parcel_id, field_id) %>%
  summarise(crop_count=sum(!is.na(crop_code)),
            legume = ifelse(any(crop_name %in% legumes), 1, 0))

oput <- left_join(oput, oput_x); rm(oput_x)

# remove observations with quantity NA or 0
oput <- oput[! is.na(oput$crop_qty_harv) & !oput$crop_qty_harv %in% 0, ]

rm("legumes")

# the price of each crop and how much was
# sold is stored in a seperate section of 
# the household (section 11)

oput2 <- read_dta(file.path(dataPath, "/Post-Harvest/sect11_ph_w2.dta")) %>%
  select(holder_id, household_id2, crop_code,
         sold=ph_s11q01, sold_qty_kg=ph_s11q03_a, sold_qty_gr=ph_s11q03_b,
         value=ph_s11q04, sold_month=ph_s11q06_a, sold_year=ph_s11q06_b,
         trans_cost=ph_s11q09)
oput2$crop_code <- as.integer(oput2$crop_code)
oput2$sold_month <- toupper(as_factor(oput2$sold_month))
oput2$sold <- toupper(as_factor(oput2$sold))

oput <- left_join(oput, oput2) %>% unique; rm(oput2)

#######################################
############## CHEMICAL ###############
#######################################

# -------------------------------------
# variables recorded at parcel, field,
# and crop level

# parcel level
parcel <- read_dta(file.path(dataPath, "Post-Planting/sect2_pp_w2.dta")) %>%
  dplyr::select(holder_id, household_id2, parcel_id, fields=pp_s2q02,
                title=pp_s2q04, soil_type=pp_s2q14, soil_qlty=pp_s2q15)

parcel$soil_type <- toupper(as_factor(parcel$soil_type))
parcel$soil_qlty <- toupper(as_factor(parcel$soil_qlty))
parcel$title <- toupper(as_factor(parcel$title))

# field level variables
# WDswitch

field <- read_dta(file.path(dataPath, "Post-Planting/sect3_pp_w2.dta")) %>%
  dplyr::select(holder_id, household_id2, parcel_id, field_id,
                crop_stand=pp_s3q03b, fallow10=pp_s3q03c, fallow_year=pp_s3q03d,
                extension=pp_s3q11, irrig=pp_s3q12, fert_any=pp_s3q14,
                other_inorg=pp_s3q20a, manure=pp_s3q21, compost=pp_s3q23,
                other_org=pp_s3q25, eros_prot=pp_s3q32, mulch=pp_s3q37) 

field$crop_stand <- as_factor(field$crop_stand)

# crop level variables
# WDswitch

crop <- read_dta(file.path(dataPath, "/Post-Planting/sect4_pp_w2.dta")) %>%
  dplyr::select(holder_id, household_id2, parcel_id, field_id, crop_code,
                cropping=pp_s4q02, month=pp_s4q12_a, crop_area=pp_s4q03,
                herb=pp_s4q06, fung=pp_s4q07, seed_type=pp_s4q11, 
                seed_qty=pp_s4q11b)

crop$crop_code <- as_factor(crop$crop_code)
crop$cropping <- as_factor(crop$cropping)
crop$month <- as_factor(crop$month)
crop$crop_code <- as.integer(crop$crop_code)

# -------------------------------------
# unit of observation is not fertilizer

fert1 <- read_dta(file.path(dataPath, "/Post-Planting/sect3_pp_w2.dta")) %>%
  dplyr::select(holder_id, household_id2, parcel_id, field_id, typ=pp_s3q15, qty=pp_s3q16_a,
                purch=pp_s3q16b, purch_kg=pp_s3q16c, valu=pp_s3q16d)
  
fert1$typ <- ifelse(fert1$typ %in% 1, "UREA", NA)
fert1$purch <- ifelse(fert1$purch %in% 1, 1, 0)

fert2 <- read_dta(file.path(dataPath, "/Post-Planting/sect3_pp_w2.dta")) %>%
  dplyr::select(holder_id, household_id2, parcel_id, field_id, typ=pp_s3q18, qty=pp_s3q19_a,
                purch=pp_s3q19b, purch_kg=pp_s3q19c, valu=pp_s3q19d) 

fert2$typ <- ifelse(fert2$typ %in% 1, "DAP", NA)
fert2$purch <- ifelse(fert2$purch %in% 1, 1, 0)

# -------------------------------------
# read in nitrogen conversion file

conv <- read.csv(file.path(paste0(dataPath,"/../../.."), "Other/Fertilizer/Fert_comp.csv")) %>%
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
pp_lab <- read_dta(file.path(dataPath, "/Post-Planting/sect3_pp_w2.dta")) %>%
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
ph_lab <- read_dta(file.path(dataPath, "/Post-Harvest/sect10_ph_w2.dta")) %>%
  dplyr::select(holder_id, household_id2, parcel_id, field_id,
                crop_code, ph_s10q01_a:ph_s10q03_f) %>%
  transmute(holder_id, household_id2, parcel_id, field_id, crop_code,
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
# make all NA values zero
ph_lab[is.na(ph_lab)] <- 0

# sum all labour across a single plot - all measured in days
ph_lab <- transmute(ph_lab, holder_id, household_id2, parcel_id, field_id,
                    crop_code, harv_lab=lab1 + lab2 + lab3 + lab4 +
                    hirM + hirF + hirC + OHHlabM + OHHlabF + OHHlabC) %>% unique

ph_lab$harv_lab[ph_lab$harv_lab %in% 0] <- NA

#######################################
############### GEO ###################
#######################################

load(file.path(dataPath, "/ETH_geo_total_2013.RData"))
geo <- geo.total.plot %>% 
  dplyr::select(holder_id, household_id2, ea_id2, parcel_id, field_id, lon, lat, SPEI, RootDepth, region=NAME_1,
                AEZ=ssa_aez09, ph=ph_sd1_sd3, ph2=ph_sd1_sd5,
                SOC=SOC_sd1_sd3, SOC2=SOC_sd1_sd5, rain=gsRainfall, 
                YA, YW, YP, everything()) %>%
  unique()
rm(geo.total.plot)

#######################################
############### AREAs #################
#######################################

# -------------------------------------
# imputed and original gps measurements
# included

areas <- read_dta(paste(dataPath, "areas_ETH2013.dta", sep="/"))
areas <- select(areas, holder_id, household_id2,
                parcel_id, field_id, area_gps, area_gps_mi50,
                area_farmer=area_sr)

areas$area_gps <- ifelse(areas$area_gps %in% 0, NA, areas$area_gps)
areas$area_gps_mi50 <- ifelse(areas$area_gps_mi50 %in% 0, NA, areas$area_gps_mi50)

areaTotal <- group_by(areas, household_id2) %>%
  summarise(area_tot = sum(area_gps_mi50, na.rm=TRUE))

#######################################
############## COMMUNITY ##############
#######################################
  
com3 <- read_dta(file.path(dataPath, "Community/sect3_com_w2.dta")) %>%
  select(ea_id2, popEA=cs3q02, HHEA=cs3q03)

com4 <- read_dta(file.path(dataPath, "Community/sect4_com_w2.dta")) %>%
  select(ea_id2, road=cs4q01, cost2small_town=cs4q10,
         cost2large_town=cs4q13, bank=cs4q45, micro_finance=cs4q47)

com4$road <- toupper(as_factor(com4$road))
com4$bank <- toupper(as_factor(com4$bank))
com4$micro_finance <- toupper(as_factor(com4$micro_finance))

com6 <- read_dta(file.path(dataPath, "Community/sect6_com_w2.dta")) %>%
  select(ea_id2, plant_month1=cs6q03_a, plant_month2=cs6q03_b, plant_month3=cs6q03_c,
         harv_month1=cs6q04_a, harv_month2=cs6q04_b, harv_month3=cs6q04_c,
         ext_agent=cs6q08, dist2ext_agent=cs6q09, fert_source=cs6q12,
         pest_source=cs6q13, seed_source=cs6q14)

# make a seperate file for month data
com6$plant_month1 <- as_factor(com6$plant_month1)
com6$plant_month2 <- as_factor(com6$plant_month2)
com6$plant_month3 <- as_factor(com6$plant_month3)
com6$harv_month1 <- as_factor(com6$harv_month1)
com6$harv_month2 <- as_factor(com6$harv_month2)
com6$harv_month3 <- as_factor(com6$harv_month3)
com6$ext_agent <- as_factor(com6$ext_agent)
com6$fert_source <- as_factor(com6$fert_source)
com6$pest_source <- as_factor(com6$pest_source)
com6$seed_source <- as_factor(com6$seed_source)

com <- left_join(com3, com4) %>% left_join(com6)
rm(com3, com4, com6)

#######################################
########### CROSS SECTION #############
#######################################

# -------------------------------------
# community and location level joins

ETH2013 <- left_join(location, com); rm(com, location)

# -------------------------------------
# household level joins

ETH2013 <- left_join(HH13, ETH2013); rm(HH13) 
ETH2013 <- left_join(ETH2013, areaTotal); rm(areaTotal)

# -------------------------------------
# parcel level joins

ETH2013 <- left_join(ETH2013, parcel); rm(parcel)

# -------------------------------------
# field level joins

ETH2013 <- left_join(ETH2013, fert); rm(fert) 
ETH2013 <- left_join(ETH2013, areas); rm(areas)
ETH2013 <- left_join(ETH2013, pp_lab); rm(pp_lab)
ETH2013 <- left_join(ETH2013, field); rm(field)
ETH2013 <- left_join(ETH2013, geo); rm(geo)

# -------------------------------------
# crop level joins
ETH2013 <- left_join(ETH2013, oput); rm(oput)
ETH2013 <- left_join(ETH2013, crop); rm(crop)
ETH2013 <- left_join(ETH2013, ph_lab); rm(ph_lab)

rm(dataPath, F)
