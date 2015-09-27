#######################################
############ NIGERIA 2012 #############
#######################################

setwd("c:/USers/tomas/Documents/work/LEI/data/NGA/NGA_2012_LSMS_v03_M_STATA")

library(haven)
library(stringr)
library(plyr)
library(dplyr)

options(scipen=999)


#######################################
############### OUTPUT ################
#######################################

oput <- read_dta("Post Harvest Wave 2/Agriculture/secta3_harvestw2.dta") %>%
    dplyr::select(hhid, plotid, crop=cropname, qty=sa3q6a1, qty_unit=sa3q6a2,
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
oput <- ddply(oput, .(hhid, plotid), transform,
              crop_count=length(crop[!is.na(crop)]),
              legume=ifelse(any(crop %in% legumes), 1, 0))

# select on maize and remove observations with quantity NA or 0
oput_maze <- oput[oput$crop %in% "MAIZE" & ! is.na(oput$qty) & !oput$qty %in% 0,]

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

oput_maze$qty_unit <- as.integer(oput_maze$qty_unit)
oput_maze <- left_join(oput_maze, cnvrt, by=c("qty_unit"="unit_code"))
oput_maze <- dplyr::mutate(oput_maze, qty_kg = qty*weight)

oput_maze <- dplyr::select(oput_maze, hhid, plotid, qty, crop_count, legume)
oput_maze$hhid <- as.integer(oput_maze$hhid)

rm(list=c("bad_maize", "cnvrt", "legumes", "oput", "unit_code", "weight"))

#######################################
############## CHEMICAL ###############
#######################################

chem <- read_dta("Post Planting Wave 2/Agriculture/sect11c2_plantingw2.dta") %>%
  dplyr::select(hhid, plotid, pest=s11c2q1, herb=s11c2q10)

chem$pest <- ifelse(chem$pest %in% 1, 1, 0)
chem$herb <- ifelse(chem$herb %in% 1, 1, 0)

chem <- dplyr::mutate(chem, hhid, plotid,
                         chem=ifelse(pest %in% 1 | herb %in% 1, 1, 0))

# COMMERCIAL FERTILIZER
fert1 <- read_dta("Post Planting Wave 2/Agriculture/sect11d_plantingw2.dta") %>%
  dplyr::select(hhid, plotid, typ=s11dq15, qty=s11dq16, valu=s11dq19)
fert2 <- read_dta("Post Planting Wave 2/Agriculture/sect11d_plantingw2.dta") %>%
    dplyr::select(hhid, plotid, typ=s11dq27, qty=s11dq28, valu=s11dq29)

# FREE OR LEFT OVER FERTILIZER
freeFert <-  read_dta("Post Planting Wave 2/Agriculture/sect11d_plantingw2.dta") %>%
    dplyr::select(hhid, plotid, typ=s11dq7, qty=s11dq8)
leftOverFert <- read_dta("Post Planting Wave 2/Agriculture/sect11d_plantingw2.dta") %>%
    dplyr::select(hhid, plotid, typ=s11dq3, qty=s11dq4)

# make factor variables into characters for easier joining
fert1$typ <- as.character(as_factor(fert1$typ))
fert2$typ <- as.character(as_factor(fert2$typ))
freeFert$typ <- as.character(as_factor(freeFert$typ))
leftOverFert$typ <- as.character(as_factor(leftOverFert$typ))

# for now set composite manure and other values to NA
bad <- c("Composite Manure", "Other (specify)")
fert1$typ <- ifelse(fert1$typ %in% bad, NA, fert1$typ)
fert2$typ <- ifelse(fert2$typ %in% bad, NA, fert2$typ)
freeFert$typ <- ifelse(freeFert$typ %in% bad, NA, freeFert$typ)
leftOverFert$typ <- ifelse(leftOverFert$typ %in% bad, NA, leftOverFert$typ)

# provide a nitrogen component value for npk and urea (from Michiel's file)
typ <- c("NPK", "UREA")
n <- c(0.27, 0.46)
p <- c(0.05668, 0)
k <- c(0.1079, 0)
comp <- data.frame(typ, n, p, k)

fert1 <- left_join(fert1, comp)
fert2 <- left_join(fert2, comp)
freeFert <- left_join(freeFert, comp)
leftOverFert <- left_join(leftOverFert, comp)

rm(list=c("comp", "typ", "n", "p", "k"))

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
areas <- read_dta("../areas_nga_y2_imputed.dta") %>%
  select(hhid=case_id, plotid=plotnum, area=area_gps_mi_50)

areas$area <- ifelse(areas$area %in% 0, NA, areas$area)


#######################################
############### LABOUR ################
#######################################

# days spent on plot for hired and damily labour
# only available for harvest. no planting/weeding information

# POST PLANTING

lab1 <- read_dta("Post Planting Wave 2/Agriculture/sect11c1_plantingw2.dta") %>%
    select(hhid, plotid, s11c1q1a1:s11c1q9) %>%
        transmute(hhid, plotid,
                  id1=s11c1q1a1, lab1=s11c1q1a2*s11c1q1a3,
                  id2=s11c1q1b1, lab2=s11c1q1b2*s11c1q1b3,
                  id3=s11c1q1c1, lab3=s11c1q1c2*s11c1q1c3,
                  id4=s11c1q1d1, lab4=s11c1q1d2*s11c1q1d3,
                  hirM=s11c1q2*s11c1q3,
                  hirF=s11c1q5*s11c1q6,
                  hirC=s11c1q8*s11c1q9)

# make all NA values zero
lab1[is.na(lab1)] <- 0

# sum all labour across a single plot - all measured in days
lab1 <- transmute(lab1, hhid, plotid,
                 plant_lab=lab1 + lab2 + lab3 + lab4 +
                     hirM + hirF + hirC)



# POST HARVEST

lab2 <- read_dta("Post Harvest Wave 2/Agriculture/secta2_harvestw2.dta") %>%
    select(hhid, plotid, sa2q1a1:sa2q10) %>%
        transmute(hhid, plotid,
                  id1=sa2q1a1, lab1=sa2q1a2*sa2q1a3,
                  id2=sa2q1b1, lab2=sa2q1b2*sa2q1b3,
                  id3=sa2q1c1, lab3=sa2q1c2*sa2q1c3,
                  id4=sa2q1d1, lab4=sa2q1d2*sa2q1d3,
                  hirM=sa2q3*sa2q4,
                  hirF=sa2q6*sa2q7,
                  hirC=sa2q9*sa2q10
                  )

# make all NA values zero
lab2[is.na(lab2)] <- 0

# sum all labour across a single plot - all measured in days
lab2 <- transmute(lab2, hhid, plotid,
                 harv_lab=lab1 + lab2 + lab3 + lab4 +
                     hirM + hirF + hirC)
lab2$hhid <- as.integer(lab2$hhid)


#######################################
############### Assets ################
#######################################

# -------------------------------------
# Agricultural assets - section A4 post harvest
# only in post harvest questionnaire
# -------------------------------------

implmt <- read_dta("Post Harvest Wave 2/Agriculture/secta42_harvestw2.dta") %>%
    dplyr::select(hhid, itemcode=item_cd, qty=item_seq, valu=sa4q4) %>%
        dplyr::filter(!qty %in% 0, !is.na(qty), !valu %in% 0, !is.na(valu)) %>%
            dplyr::transmute(hhid, valu=qty*valu) %>%
                dplyr::group_by(hhid) %>%
                    dplyr::summarise(implmt_value=sum(valu))
implmt$hhid <- as.integer(implmt$hhid)

# -------------------------------------
# Livestock assets were recorded post
# planting and post harvest
# -------------------------------------

# POST PLANTING

lvstk <- read_dta("Post Planting Wave 2/Agriculture/sect11i_plantingw2.dta") %>%
    dplyr::select(hhid, lvstk=animal_cd, qty=s11iq2, valu=s11iq3) %>%
        dplyr::filter(!is.na(qty), !qty %in% 0) %>%
           dplyr:: mutate(prc=valu/qty)

# select only the larger animals - codes are
# in the survey but basically cows, pigs sheep and camels
big <- c(101, 102, 103, 104, 105, 106, 107,
         108, 109, 110, 111, 112, 122)

lvstk <- lvstk[lvstk$lvstk %in% big,]
lvstk[is.na(lvstk)] <- 0

lvstk <- ddply(lvstk, .(lvstk), transform,
               valu=ifelse(is.na(valu), mean(prc, na.rm=TRUE)*qty, valu))

# calculate per houshold livestock wealth
lvstk <- group_by(lvstk, hhid) %>%
        summarise(lvstk_valu=sum(valu*qty))

# POST HARVEST

lvstk2 <- read_dta("Post Harvest Wave 2/Agriculture/secta6_harvestw2.dta") %>%
    select(hhid, lvstk=animal_cd, qty=sa6q2, valu=sa6q3) %>%
        filter(!is.na(qty), !qty %in% 0) %>%
        mutate(prc=valu/qty)

# select only the larger animals - codes are
# in the survey but basically cows, pigs sheep and camels
big <- c(101, 102, 103, 104, 105, 106, 107,
         108, 109, 110, 111, 112, 122)

lvstk2 <- lvstk2[lvstk2$lvstk %in% big,]

lvstk2 <- ddply(lvstk2, .(lvstk), transform,
               valu=ifelse(is.na(valu), mean(prc, na.rm=TRUE)*qty, valu))

# calculate per houshold livestock wealth
lvstk2 <- group_by(lvstk2, hhid) %>%
    summarise(lvstk2_valu=sum(valu*qty))
lvstk2$hhid <- as.integer(lvstk2$hhid)

rm("big")

#######################################
################ GEO ##################
#######################################

geo <- read_dta("Geodata Wave 2/NGA_HouseholdGeovars_Y2.dta") %>%
    dplyr::select(hhid, lon=LON_DD_MOD, lat=LAT_DD_MOD, zone,
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
# options - make dummy variables for
# all of them
# -------------------------------------

cropping <- read_dta("Post Planting Wave 2/Agriculture/sect11f_plantingw2.dta") %>%
    dplyr::select(hhid, plotid, cropcode, cropin=s11fq2)

# find only maize - crop code 1080
cropping <- dplyr::filter(cropping, cropcode %in% 1080)

cropping <- dplyr::mutate(cropping,
                          monoCrop=ifelse(cropin %in% 1, 1, 0),
                          interCrop=ifelse(cropin %in% 2, 1, 0),
                          relayCrop=ifelse(cropin %in% 3, 1, 0),
                          mixCrop=ifelse(cropin %in% 4, 1, 0),
                          alleyCrop=ifelse(cropin %in% 5, 1, 0),
                          stripCrop=ifelse(cropin %in% 4, 1, 0))

cropping <- dplyr::select(cropping, -cropcode, -cropin)

# ------------------------------------
# irrigation variable
# ------------------------------------

irrig <- read_dta("Post Planting Wave 2/Agriculture/sect11b1_plantingw2.dta") %>%
    dplyr::select(hhid, plotid, irrig=s11b1q39)

irrig$irrig <- ifelse(irrig$irrig %in% 1, 1, 0)

#######################################
########### CROSS SECTION #############
#######################################

CS2 <- left_join(oput_maze, chem)
CS2 <- left_join(CS2, areas)
CS2 <- left_join(CS2, lab1)
CS2 <- left_join(CS2, lab2)
CS2 <- left_join(CS2, cropping)
CS2 <- left_join(CS2, irrig)
CS2 <- left_join(CS2, implmt)
CS2 <- left_join(CS2, lvstk)
CS2 <- left_join(CS2, lvstk2)
CS2 <- left_join(CS2, geo)

# -------------------------------------
# Make some new variables
# -------------------------------------

CS2 <- ddply(CS2, .(hhid), transform, area_tot=sum(area))

# if there is an NA value for any type of
# asset set to zero to calculate total assets per plot

CS2$implmt_value <- ifelse(is.na(CS2$implmt_value), 0, CS2$implmt_value)
CS2$lvstk_valu <- ifelse(is.na(CS2$lvstk_valu), 0, CS2$lvstk_valu)
CS2$lvstk2_valu <- ifelse(is.na(CS2$lvstk2_valu), 0, CS2$lvstk2_valu)

# per hectacre
CS2 <- mutate(CS2,
             yld=qty/area,
             N=N/area,
             P=P/area,
             asset=(implmt_value + lvstk2_valu)/area_tot
)

CS2 <- select(CS2, -plotid, -qty)

# add final variables
CS2 <- mutate(CS2,
             N2=N^2,
             asset2=asset^2,
             area2=area^2,
             harv_lab2=harv_lab^2,
             surveyyear=2012
)

# save to file
write_dta(CS2, "C:/Users/Tomas/Documents/Work/LEI/NGA12_data.dta")
