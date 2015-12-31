#######################################
############ NIGERIA 2010 #############
#######################################


setwd("C:/Users/Tomas/Documents/lei/data/NGA/NGA_2010_GHSP_v02_M_STATA")

library(haven)
library(dplyr)

options(scipen=999)


#######################################
############### OUTPUT ################
#######################################

oput <- read_dta("Post Harvest Wave 1/Agriculture/secta3_harvestw1.dta") %>%
    dplyr::select(hhid, plotid, cropid, crop=sa3q1, qty=sa3q6a, qty_unit=sa3q6b,
           main_buyer=sa3q10, qty_sold=sa3q11a, qty_sold_unit=sa3q11b, qty_sold_naira=sa3q12)

oput$qty_unit <- as.integer(oput$qty_unit)
oput$qty_sold_unit <- as.integer(oput$qty_sold_unit)

# -------------------------------------
# Lots of mispellings in the crop names
# Tried to fix some of them. Also not
# clear what is a legume here
# -------------------------------------

bad_maize <- c("MAIZE.", "MAAIZE", "MAIZE FARM", "M AIZE", "MAZIE", "maize")
oput$crop <- ifelse(oput$crop %in% bad_maize, "MAIZE", oput$crop)

legumes <- c("PIGEON PEA", "SOYA BEANS", "LOCUST BEAN")

oput_x <- group_by(oput, hhid, plotid) %>%
  summarise(crop_count=length(crop[!is.na(crop)]),
            legume=ifelse(any(crop %in% legumes), 1, 0))

oput <- left_join(oput, oput_x); rm(oput_x)

# select on maize and remove observations with quantity NA or 0
oput_maize <- oput[oput$crop %in% "MAIZE" & ! is.na(oput$qty) & !oput$qty %in% 0,]

# unit labelled vector does not come through. Make conversion
# factor using information from survey

unit_code <- c(1, 2, 3, 11, 12, 13, 14, 21, 22, 23, 24, 31,
               32, 33, 34, 41, 42, 43, 51, 52, 53, 61,
               62, 63, 71, 72, 73, 74, 81, 82, 83,
               91, 92, 93, 94, 95)
weight <- c(1, 0.001, 1, 20, 50, 100, 120, 15, 30, 50, 75, 10, 25, 40, 75,
            5, 8, 15, 3, 5, 8, 15, 25, 40, 60, 85, 110, 150,
            1500, 2000, 2500, 10, 20, 25, 50, 200)

cnvrt <- data.frame(unit_code, weight)

oput_maize$qty_unit <- as.integer(oput_maize$qty_unit)
oput_maize <- left_join(oput_maize, cnvrt, by=c("qty_unit"="unit_code"))
oput_maize <- dplyr::mutate(oput_maize, qty_kg = qty*weight)

oput_maize <- dplyr::select(oput_maize, hhid, plotid, qty, crop_count, legume)

rm(list=c("bad_maize", "cnvrt", "legumes", "oput", "unit_code", "weight"))

#######################################
############## CHEMICAL ###############
#######################################

chem <- read_dta("Post Planting Wave 1/Agriculture/sect11c_plantingw1.dta") %>%
  dplyr::select(hhid, plotid, pest=s11cq1, herb=s11cq10)

chem$pest <- ifelse(chem$pest %in% 1, 1, 0)
chem$herb <- ifelse(chem$herb %in% 1, 1, 0)

chem <- dplyr::mutate(chem, hhid, plotid,
                         chem=ifelse(pest %in% 1 | herb %in% 1, 1, 0))

# COMMERCIAL FERTILIZER
fert1 <- read_dta("Post Planting Wave 1/Agriculture/sect11d_plantingw1.dta") %>%
  dplyr::select(hhid, plotid, typ=s11dq14, qty=s11dq15, valu=s11dq18)
fert2 <- read_dta("Post Planting Wave 1/Agriculture/sect11d_plantingw1.dta") %>%
    dplyr::select(hhid, plotid, typ=s11dq25, qty=s11dq26, valu=s11dq29)

# FREE OR LEFT OVER FERTILIZER
freeFert <-  read_dta("Post Planting Wave 1/Agriculture/sect11d_plantingw1.dta") %>%
    dplyr::select(hhid, plotid, typ=s11dq7, qty=s11dq8)
leftOverFert <- read_dta("Post Planting Wave 1/Agriculture/sect11d_plantingw1.dta") %>%
    dplyr::select(hhid, plotid, typ=s11dq3, qty=s11dq4)

# make factor variables into characters for easier joining
fert1$typ <- toupper(as.character(as_factor(fert1$typ)))
fert2$typ <- toupper(as.character(as_factor(fert2$typ)))
freeFert$typ <- toupper(as.character(as_factor(freeFert$typ)))
leftOverFert$typ <- toupper(as.character(as_factor(leftOverFert$typ)))

# to match conversion table make NPK, generic NPK (NGA)
fert1$typ <- gsub("NPK", "generic NPK (NGA)", fert1$typ)
fert2$typ <- gsub("NPK", "generic NPK (NGA)", fert2$typ)
freeFert$typ <- gsub("NPK", "generic NPK (NGA)", freeFert$typ)
leftOverFert$typ <- gsub("NPK", "generic NPK (NGA)", leftOverFert$typ)

# for now set composite manure and other values to NA
bad <- c("composite manure", "other (specify)")
fert1$typ <- ifelse(fert1$typ %in% bad, NA, fert1$typ)
fert2$typ <- ifelse(fert2$typ %in% bad, NA, fert2$typ)
freeFert$typ <- ifelse(freeFert$typ %in% bad, NA, freeFert$typ)
leftOverFert$typ <- ifelse(leftOverFert$typ %in% bad, NA, leftOverFert$typ)

# provide a nitrogen component value for npk and urea (from Michiel's file)
conv <- read.csv("c:/users/tomas/documents/lei/data/Fert_comp.csv") %>%
  transmute(typ=Fert_type2, n=N_share/100, p=P_share/100) %>%
  filter(typ %in% unique(fert1$typ))

fert1 <- left_join(fert1, conv)
fert2 <- left_join(fert2, conv)
freeFert <- left_join(freeFert, conv)
leftOverFert <- left_join(leftOverFert, conv)

fert <- rbind(fert1, fert2)

# make calculations for commercial fertilizer
fert <- mutate(fert,
               Vfert=valu/qty,
               Qn=qty*n,
               Qp=qty*p)

fert$Pn <- fert$Vfert/fert$n

fert <- group_by(fert, hhid, plotid) %>%
  summarise(N=sum(Qn, na.rm=TRUE),
            P=sum(Qp, na.rm=TRUE),
            WPn=sum((Qn/N)*Pn, na.rm=TRUE))

# now add back in the left over or free fert which does not have a price

otherFert <- rbind(freeFert, leftOverFert)

otherFert <- mutate(otherFert,
                    QnO=qty*n,
                    QpO=qty*p)

otherFert <- group_by(otherFert, hhid, plotid) %>%
    summarise(NO=sum(QnO, na.rm=TRUE),
              PO=sum(QpO, na.rm=TRUE))

# join the commercial and other fertilizers on quantity
# no change to price though!

fert <- left_join(fert, otherFert)
fert <- mutate(fert,
              N=N+NO,
              P=P+PO,
               WPn) %>%
    select(hhid, plotid, N, P, WPn)

# and join with other chemical variables
chem <- left_join(chem, fert)

rm(list=c("bad", "fert", "fert1", "fert2", "freeFert", "leftOverFert", "otherFert"))

#######################################
############### AREAS #################
#######################################

# world bank provides a complete set of
# area measurements
areas <- read_dta("../areas_nga_y1_imputed.dta") %>%
  select(hhid=case_id, plotid=plotnum,
         area_gps=area_gps_mi_50,
         area_farmer=area_sr)

areas$area_gps <- ifelse(areas$area_gps %in% 0, NA, areas$area_gps)

areaTotal <- group_by(areas, hhid) %>%
  summarise(area_tot = sum(area_gps, na.rm=TRUE))

areaTotal$area_tot <- ifelse(areaTotal$area_tot %in% 0, NA, areaTotal$area_tot)


#######################################
############### LABOUR ################
#######################################

# days spent on plot for hired and damily labour
# only available for harvest. no planting/weeding information

lab <- read_dta("Post Harvest Wave 1/Agriculture/secta2_harvestw1.dta") %>%
    select(hhid, plotid, sa2q1a1:sa2q9) %>%
        transmute(hhid, plotid,
                  id1=sa2q1a1, lab1=sa2q1a2*sa2q1a3,
                  id2=sa2q1b1, lab2=sa2q1b2*sa2q1b3,
                  id3=sa2q1c1, lab3=sa2q1c2*sa2q1c3,
                  id4=sa2q1d1, lab4=sa2q1d2*sa2q1d3,
                  hirM=sa2q2*sa2q3,
                  hirF=sa2q5*sa2q6,
                  hirC=sa2q8*sa2q9
                  )

# make all NA values zero
lab[is.na(lab)] <- 0

# sum all labour across a single plot - all measured in days
lab <- transmute(lab, hhid, plotid,
                 harv_lab=lab1 + lab2 + lab3 + lab4 +
                     hirM + hirF + hirC)



#######################################
############### Assets ################
#######################################

# -------------------------------------
# Agricultural assets - section A4 post harvest
# only in post harvest questionnaire
# -------------------------------------

implmt <- read_dta("Post Harvest Wave 1/Agriculture/secta42_harvestw1.dta") %>%
    dplyr::select(hhid, itemcode=item_cd, qty=item_seq, valu=sa4q4) %>%
        filter(!qty %in% 0, !is.na(qty), !valu %in% 0, !is.na(valu)) %>%
            transmute(hhid, valu=qty*valu) %>%
                group_by(hhid) %>%
                    summarise(implmt_value=sum(valu))

# -------------------------------------
# Livestock assets were recorded post
# planting and post harvest
# -------------------------------------

# POST PLANTING

lvstk <- read_dta("Post Planting Wave 1/Agriculture/sect11i_plantingw1.dta") %>%
    select(hhid, lvstk=item_cd, qty=s11iq2, valu=s11iq3) %>%
        mutate(prc=valu/qty)

# select only the larger animals - codes are
# in the survey but basically cows, pigs sheep and camels
big <- c(101, 102, 103, 104, 105, 106, 107,
         108, 109, 110, 111, 112, 122)

lvstk <- lvstk[lvstk$lvstk %in% big,]

lvstk <- group_by(lvstk, lvstk) %>% mutate(valu_avg=mean(prc, na.rm=TRUE)*qty)
lvstk$valu <- ifelse(is.na(lvstk$valu), lvstk$valu_avg, lvstk$valu)


# calculate per houshold livestock wealth
lvstk <- group_by(lvstk, hhid) %>%
        summarise(lvstk_valu=sum(valu*qty))

# POST HARVEST

lvstk2 <- read_dta("Post Harvest Wave 1/Agriculture/secta6_harvestw1.dta") %>%
    select(hhid, lvstk=animal_cd, qty=sa6q2, valu=sa6q3) %>%
        filter(!is.na(qty), !qty %in% 0) %>%
        mutate(prc=valu/qty)

# select only the larger animals - codes are
# in the survey but basically cows, pigs sheep and camels
big <- c(101, 102, 103, 104, 105, 106, 107,
         108, 109, 110, 111, 112, 122)

lvstk2 <- lvstk2[lvstk2$lvstk %in% big,]

lvstk2 <- group_by(lvstk2, lvstk) %>% mutate(valu_avg=mean(prc, na.rm=TRUE)*qty)
lvstk2$valu <- ifelse(is.na(lvstk2$valu), lvstk2$valu_avg, lvstk2$valu)

# calculate per houshold livestock wealth
lvstk2 <- group_by(lvstk2, hhid) %>%
        summarise(lvstk2_valu=sum(valu*qty))

rm("big")

#######################################
################ GEO ##################
#######################################

geo <- read_dta("Geodata/NGA_HouseholdGeovariables_Y1.dta") %>%
    dplyr::select(hhid, lon=lon_dd_mod, lat=lat_dd_mod, zone,
                  state, AEZ=ssa_aez09, rural=sector)

geo$zone <- as_factor(geo$zone)
geo$state <- as_factor(geo$state)
geo$rural <- ifelse(geo$rural %in% 2, 1, 0)
geo$AEZ <- as.integer(geo$AEZ)

#######################################
########### MISCELLANEOUS #############
#######################################

# -------------------------------------
# Intercropping variable has lots of
# options - make summy variables for
# all of them
# -------------------------------------

cropping <- read_dta("Post Planting Wave 1/Agriculture/sect11f_plantingw1.dta") %>%
    dplyr::select(hhid, plotid, cropcode, cropin=s11fq2)

# find only maize - crop code 1080
cropping <- filter(cropping, cropcode %in% 1080)

cropping <- dplyr::mutate(cropping,
                          monoCrop=ifelse(cropin %in% 1, 1, 0),
                          inter_crop=ifelse(cropin %in% 2, 1, 0),
                          relayCrop=ifelse(cropin %in% 3, 1, 0),
                          mixCrop=ifelse(cropin %in% 4, 1, 0),
                          alleyCrop=ifelse(cropin %in% 5, 1, 0),
                          stripCrop=ifelse(cropin %in% 4, 1, 0))

cropping <- dplyr::select(cropping, -cropcode, -cropin)

# ------------------------------------
# irrigation variable
# ------------------------------------

irrig <- read_dta("Post Planting Wave 1/Agriculture/sect11b_plantingw1.dta") %>%
    dplyr::select(hhid, plotid, irrig=s11bq24)

irrig$irrig <- ifelse(irrig$irrig %in% 1, 1, 0)

#######################################
########### CROSS SECTION #############
#######################################

NGA2010 <- left_join(oput_maize, chem)
NGA2010 <- left_join(NGA2010, areas)
NGA2010 <- left_join(NGA2010, lab)

# add in placeholder for planting labour in wave2
NGA2010$plant_lab <- NA

NGA2010 <- left_join(NGA2010, cropping)
NGA2010 <- left_join(NGA2010, irrig)
NGA2010 <- left_join(NGA2010, implmt)
NGA2010 <- left_join(NGA2010, lvstk)
NGA2010 <- left_join(NGA2010, lvstk2)
NGA2010 <- left_join(NGA2010, geo)
NGA2010 <- left_join(NGA2010, areaTotal)

# -------------------------------------
# Make some new variables
# -------------------------------------

# if there is an NA value for any type of
# asset set to zero to calculate total assets per plot

NGA2010$implmt_value <- ifelse(is.na(NGA2010$implmt_value), 0, NGA2010$implmt_value)
NGA2010$lvstk_valu <- ifelse(is.na(NGA2010$lvstk_valu), 0, NGA2010$lvstk_valu)
NGA2010$lvstk2_valu <- ifelse(is.na(NGA2010$lvstk2_valu), 0, NGA2010$lvstk2_valu)

# per hectacre
NGA2010 <- mutate(NGA2010,
             yld=qty/area_gps,
             N=N/area_gps,
             P=P/area_gps,
             asset=(implmt_value + lvstk2_valu)/area_tot
)

# -------------------------------------
# Inflate 2010 prices to 2012 prices
# using inflation rate for 2010 and 2011
# from world bank:
# http://data.worldbank.org/indicator/FP.CPI.TOTL.ZG/countries/NG?display=graph
# -------------------------------------

inflation <- read.csv("C:/Users/Tomas/Documents/lei/data/inflation.csv")
rate2011 <- inflation$inflation[inflation$code=="NG" & inflation$year==2011]
rate2013 <- inflation$inflation[inflation$code=="NG" & inflation$year==2013]

NGA2010 <- mutate(NGA2010,
                  asset = asset*(1 + rate2011)*(1 + rate2013),
                  # maize_prc = maize_prc*(1 + rate2011)*(1 + rate2013),
                  WPn = WPn*(1 + rate2011)*(1 + rate2013))

NGA2010 <- select(NGA2010, -qty)

# add final variables
NGA2010 <- mutate(NGA2010,
             N2=N^2,
             asset2=asset^2,
             area2=area_gps^2,
             harv_lab2=harv_lab^2,
             surveyyear=2010
)

# save to file
# write_dta(NGA2010, "C:/Users/Tomas/Documents/Work/LEI/NGA10_data.dta")
