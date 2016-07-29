# -------------------------------------
# Ethiopia data 2011 - 2012 survey
# -------------------------------------

if(Sys.info()["user"] == "Tomas"){
  dataPath <- "C:/Users/Tomas/Documents/LEI/data/ETH/2011/Data"
} else {
  dataPath <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/ETH/2011/Data"
}

library(haven)
library(dplyr)

options(scipen=999)

#######################################
############## LOCATION ###############
#######################################

# file containing the zone, region and district

location <- read_dta(file.path(dataPath, "/sect1_hh_w1.dta")) %>%
  select(household_id, REGCODE = saq01, ZONECODE = saq02, ea_id, rural) 

# recode the rural variable to match with wave 2
location$rural <- ifelse(location$rural %in% 1, 1, 2)
location$type <- factor(location$rural, levels=1:2, labels=c("RURAL", "SMALL TOWN"))
location$rural <- ifelse(location$rural %in% 1, 1, 0)
location$REGCODE <- as.integer(location$REGCODE)
location <- unique(location)

# match up with the names from the survey (prepared in a seperate file)

REGZONE <- read.csv(file.path(paste0(dataPath,"/../../.."), "Other/Spatial/ETH/REGZONEETH.csv"))

# join with household identifications
location <- left_join(location, REGZONE)

rm(REGZONE)

#######################################
########### SOCIO/ECONOMIC ############
#######################################

HH11 <- read_dta(file.path(dataPath, "sect1_hh_w1.dta")) %>%
  select(household_id, individual_id,
         ea_id, status=hh_s1q02, sex=hh_s1q03, age=hh_s1q04_a,
         religion=hh_s1q07, marital=hh_s1q08)

HH11$status <- toupper(as_factor(HH11$status))
HH11$religion <- toupper(as_factor(HH11$religion))
HH11$sex <- toupper(as_factor(HH11$sex)) 
HH11$marital <- toupper(as_factor(HH11$marital)) 

# make a new variable cage (cut age = cage) which splits
# individuals according to their age group with
# breaks at 15, 55 and the max age

HH11$cage <- cut(HH11$age, breaks = c(0, 15, 55, max(HH11$age, na.rm=TRUE)),
                 labels=c("0-15", "16-55", "56+"), include.lowest = TRUE, right = TRUE)

# education of household members and sum
# of education of all household members
# between the ages of 15 and 55

education <- read_dta(file.path(dataPath, "sect2_hh_w1.dta")) %>%
  select(household_id, individual_id, ea_id,
         literate=hh_s2q02, ed_any=hh_s2q03)

education$literate <- toupper(as_factor(education$literate))
education$ed_any <- toupper(as_factor(education$ed_any))

# summarise the data: get sum of education
# of household members 15-55 and number of
# household members 15:55

HH11_x <- group_by(HH11, household_id) %>%
  summarise(N1555=sum(cage %in% "16-55"),
            family_size=n())

# -------------------------------------
# death in the family
# -------------------------------------

death <- read_dta(file.path(dataPath, "sect8_hh_w1.dta")) %>%
  select(household_id, code=hh_s8q00, death=hh_s8q01) %>% 
  filter(code %in% c("101", "102")) %>% select(-code) %>%
  group_by(household_id) %>%
  summarise(death=ifelse(any(death %in% 1), 1, 0))

HH11 <- left_join(HH11, education) %>%
  left_join(HH11_x) %>%
  left_join(death); rm(education, HH11_x, death)

#######################################
############### OUTPUT ################
#######################################

# in the 2011 wave of the data, the respondents
# were asked about the output per 2 metre by 2 metre
# area. They were also asked how much the total field
# produced.

oput <- read_dta(file.path(dataPath, "sect9_ph_w1.dta")) %>%
  transmute(household_id, holder_id, parcel_id, field_id,
         crop_code, day_cut=ph_s9q02_a, month_cut=ph_s9q02_b,
         crop_qty_harv_fresh_kg=ph_s9q03_a,
         crop_qty_harv_fresh_g=ph_s9q03_b/100,
         crop_qty_harv_dry_kg=ph_s9q05_a,
         crop_qty_harv_dry_g=ph_s9q05_b/100,
         crop_qty_harv_tot_kg=ph_s9q12_a,
         crop_qty_harv_tot_g=ph_s9q12_b/100,
         harv_month_start=ph_s9q13_a,
         harv_month_end=ph_s9q13_b, crop_name)

oput$crop_code <- as.integer(oput$crop_code)
x <- c("crop_qty_harv_fresh_kg","crop_qty_harv_fresh_g","crop_qty_harv_dry_kg",
       "crop_qty_harv_dry_g","crop_qty_harv_tot_kg","crop_qty_harv_tot_g")
make0 <- is.na(oput[, x])
oput[, x][make0] <- 0

oput <- transmute(oput, household_id, holder_id, parcel_id, field_id,
                  crop_code, day_cut, month_cut, 
                  crop_qty_harv_fresh=crop_qty_harv_fresh_kg+crop_qty_harv_fresh_g,
                  crop_qty_harv_dry=crop_qty_harv_dry_kg+crop_qty_harv_dry_g,
                  crop_qty_harv_tot=crop_qty_harv_tot_kg+crop_qty_harv_tot_g,
                  crop_name)

# -------------------------------------
# add a dummy if a legume was grown
# count number of crops per field
# crop codes taken from appendix in 
# the BID

legumes <- c(11:18, 36, 118)

oput_x <- group_by(oput, holder_id, household_id, parcel_id, field_id) %>%
  summarise(crop_count=sum(!is.na(crop_code)),
            legume = ifelse(any(crop_code %in% legumes), 1, 0))

oput <- left_join(oput, oput_x); rm(oput_x)

# remove observations with quantity NA or 0
# oput <- oput[! is.na(oput$crop_qty_harv) & !oput$crop_qty_harv %in% 0, ]

rm("legumes")

# the price of each crop and how much was
# sold is stored in a seperate section of 
# the household (section 11)

oput2 <- read_dta(file.path(dataPath, "sect11_ph_w1.dta")) %>%
  select(holder_id, household_id, crop_code,
         sold=ph_s11q01, sold_qty_kg=ph_s11q03_a, sold_qty_gr=ph_s11q03_b,
         value=ph_s11q04_a, sold_month=ph_s11q06_a, sold_year=ph_s11q06_b,
         trans_cost=ph_s11q09) 
oput2$crop_code <- as.integer(oput2$crop_code)
oput2$sold <- toupper(as_factor(oput2$sold))

oput <- left_join(oput, oput2) %>% unique; rm(oput2)

#######################################
############## CHEMICAL ###############
#######################################

# -------------------------------------
# variables recorded at parcel, field,
# and crop level

# parcel level
parcel <- read_dta(file.path(dataPath, "sect2_pp_w1.dta")) %>%
  select(household_id, holder_id, parcel_id, fields=pp_s2q02,
                title=pp_s2q04)

parcel$title <- toupper(as_factor(parcel$title))

# field level variables

field <- read_dta(file.path(dataPath, "sect3_pp_w1.dta")) %>%
  select(holder_id, household_id, parcel_id, field_id,
                crop_stand=pp_s3q10,
                extension=pp_s3q11, irrig=pp_s3q12, fert_any=pp_s3q14,
                manure=pp_s3q21, compost=pp_s3q23,
                other_org=pp_s3q25) 

field$crop_stand <- toupper(as_factor(field$crop_stand))

# crop level variables
# WDswitch

crop <- read_dta(file.path(dataPath, "sect4_pp_w1.dta")) %>%
  select(household_id, holder_id, parcel_id, field_id, crop_code,
         cropping=pp_s4q02, month=pp_s4q12_a, crop_area=pp_s4q03,
         pest=pp_s4q06, herb=pp_s4q06, fung=pp_s4q07, seed_type=pp_s4q11,
        crop_name)

crop$cropping <- as_factor(crop$cropping)
crop$month <- as_factor(crop$month)
crop$crop_code <- as.integer(crop$crop_code)

# ADD THE SEED ROSTER


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

