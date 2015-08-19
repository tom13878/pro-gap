#######################################
########## TANZANIA 2012-13 ###########
#######################################

setwd("c:/USers/tomas/Documents/work/LEI/data/TZA/TZA_2012_LSMS_v01_M_STATA_English_labels")

library(haven)
library(plyr)
library(dplyr)

options(scipen=999)

#######################################
############### OUTPUT ################
#######################################

oput <- read_dta("AG_SEC_4A.dta") %>%
  select(hhid=y3_hhid, plotnum, zaocode, inter_crop=ag4a_04,
         harv_area=ag4a_21, qty=ag4a_28, valu=ag4a_29, hybrd=ag4a_08)

oput$inter_crop <- ifelse(oput$inter_crop %in% 1, 1, 0)
oput$hybrd <- ifelse(oput$hybrd %in% 1, 1, 0)
oput$zaocode <- as.integer(oput$zaocode)

legumes <- c(31, 32, 33, 35, 36, 37, 41, 42, 43, 47, 48)
oput <- ddply(oput, .(hhid, plotnum), transform,
              crop_count=length(unique(zaocode[!is.na(zaocode)])),
              legume=ifelse(any(zaocode %in% legumes), 1, 0))

oput <- oput[!oput$crop_count > 7,]

oput$crop2 <- ifelse(oput$crop_count %in% 2, 1, 0)
oput$crop3 <- ifelse(oput$crop_count %in% 3, 1, 0)
oput$crop4 <- ifelse(oput$crop_count %in% 4, 1, 0)
oput$crop5 <- ifelse(oput$crop_count %in% 5, 1, 0)
oput$crop6 <- ifelse(oput$crop_count %in% 6, 1, 0)
oput$crop7 <- ifelse(oput$crop_count %in% 7, 1, 0)

oput_maze <- oput[ oput$zaocode %in% 11 & ! is.na(oput$qty) & !oput$qty %in% 0, ]
oput_maze$maize_prc <- oput_maze$valu/oput_maze$qty
oput_maze <- select(oput_maze, -zaocode, -valu)

rm(list=c("oput", "legumes"))

#######################################
############# CHEMICAL ################
#######################################

plot <- read_dta("AG_SEC_3A.dta") %>%
  select(hhid=y3_hhid, plotnum, maze=ag3a_07_2, irrig=ag3a_18,
         manure=ag3a_41, pest=ag3a_60, fallow_year=ag3a_22, fallow=ag3a_23)

plot$irrig <- ifelse(plot$irrig %in% 1, 1, 0)
plot$manure <- ifelse(plot$manure %in% 1, 1, 0)
plot$pest <- ifelse(plot$pest %in% 1, 1, 0)
plot$maze <- ifelse(plot$maze %in% 11, 1, 0)

# two questions on fallow - make sure they match up correctly
plot$fallow_year <- ifelse(plot$fallow_year %in% 98, NA, plot$fallow_year)
plot$fallow <- ifelse(plot$fallow_year %in% 0, 0, plot$fallow )
plot$fallow <- ifelse(is.na(plot$fallow_year), NA, plot$fallow)
plot <- select(plot, -fallow_year)

fert1 <- read_dta("AG_SEC_3A.dta") %>%
  select(hhid=y3_hhid, plotnum, typ=ag3a_48, qty=ag3a_49, vouch=ag3a_50, valu=ag3a_51)

fert2 <- read_dta("AG_SEC_3A.dta") %>%
  select(hhid=y3_hhid, plotnum, typ=ag3a_55, qty=ag3a_56, vouch=ag3a_57, valu=ag3a_58)

fert1$typ <- as_factor(fert1$typ)
fert1$vouch <- ifelse(fert1$vouch %in% 2, 0, fert1$vouch)

fert2$typ <- as_factor(fert2$typ)
fert2$vouch <- ifelse(fert2$vouch %in% 2, 0, fert2$vouch)

levels(fert1$typ) <- levels(fert2$typ) <-
  c("dap", "urea", "tsp", "can", "sa", "npk", "mrp")

# -------------------------------------
# reorganize data so that observations
# on fertilizer type occupy a single row
# fertilizer is unit of observation
# -------------------------------------

typ <- factor(levels(fert1$typ), levels=levels(fert1$typ))
n <- c(0.18, 0.46, NA, 0.26, 0.21, 0.17, NA)
p <- c(0.2, NA, 0.2056, NA, NA, 0.07412, 0.124696)
k <- c(NA, NA, NA, NA, NA, 0.1411, NA)
comp <- data.frame(typ, n, p, k)

fert1 <- left_join(fert1, comp)
fert2 <- left_join(fert2, comp)
fert <- rbind(fert1, fert2)

rm(list=c("comp", "typ", "n", "p", "k", "fert1", "fert2"))

fert <- mutate(fert,
               Vfert=valu/qty,
               Qn=qty*n,
               Qp=qty*p)

fert$Pn <- fert$Vfert/fert$n

fert <- group_by(fert, hhid, plotnum) %>%
  summarise(N=sum(Qn, na.rm=TRUE),
            P=sum(Qp, na.rm=TRUE),
            WPn=sum((Qn/N)*Pn, na.rm=TRUE))

# join back with the rest of the data
plot <- left_join(plot, fert)

rm(list=c("fert"))

#######################################
############### LABOUR ################
#######################################

lab <- read_dta("AG_SEC_3A.dta") %>%
  select(hhid=y3_hhid, plotnum, ag3a_72_id1:ag3a_74_16 )

# calculate the total labour days for hired and family labour.
bad <- grep( "ag3a_72_id", names( lab ) )
lab <- lab[, -bad]

bad <- names( lab )[( length( lab )-15 ):length( lab )][seq( from=4, to=16, by=4 )]
lab <- lab[, -which( names( lab ) %in% bad )]

lab <- transmute( lab, hhid, plotnum,
                  fam_lab_days=rowSums( lab[, 3:30], na.rm=TRUE ),
                  hir_lab_days=rowSums( lab[, 32:ncol( lab )], na.rm=TRUE ) )

lab <- transmute(lab, hhid, plotnum, lab=fam_lab_days + hir_lab_days) 

# doesn't make sense to have 0 labour on a plot so set values to zero
lab$lab <- ifelse(lab$lab %in% 0, NA, lab$lab)

#######################################
############### ASSETS ################
#######################################

implmt <- read_dta("AG_SEC_11.dta") %>%
  select(hhid=y3_hhid, itemname, qty=ag11_01, valu=ag11_02) %>%
  filter(!qty %in% 0, !is.na(qty), !valu %in% 0, !is.na(valu)) %>%
  transmute(hhid, valu=qty*valu) %>%
  group_by(hhid) %>%
  summarise(value=sum(valu))


#######################################
############### GEO ###################
#######################################

setwd("C:/Users/Tomas/Documents/Work/LEI")
geo <- read.csv("TZA_geo.total2012.csv") %>%
  select(hhid=y3_hhid, lon, lat, SPEI, RootDepth, region=NAME_1,
        AEZ=land03, ph=ph_sd1_sd3, ph2=ph_sd1_sd5,
        SOC=SOC_sd1_sd3, SOC2=SOC_sd1_sd5, rain=gsRainfall) %>%
  unique()

labels <- c("Tropic-cool/humid", "Tropic-cool/semiarid", "Tropic-cool/subhumid",
            "Tropic-warm/humid", "Tropic-warm/semiarid", "Tropic-warm/subhumid")
geo$AEZ <- factor(geo$AEZ, labels=labels)

geo$hhid <- as.character(geo$hhid)

# add a zone variable
geo$zone[geo$region %in% c("Kagera","Mwanza", "Mara")] <- "Lake"
geo$zone[geo$region %in% c("Shinyanga","Kigoma", "Tabora")] <- "Western"
geo$zone[geo$region %in% c("Arusha","Kilimanjaro", "Manyara", "Tanga")] <- "Northern"

geo$zone[geo$region %in% c("Singida","Dodoma")] <- "Central"
geo$zone[geo$region %in% c("Rukwa", "Mbeya","Iringa")] <- "Southern Highlands"
geo$zone[geo$region %in% c("Pwani","Morogoro", "Dar-Es-Salaam")] <- "Eastern"
geo$zone[geo$region %in% c("Lindi","Ruvuma", "Mtwara")] <- "Southern"
geo$zone[geo$region %in% c("Kaskazini-Unguja", "Zanzibar South and Central", "Kusini-Pemba",
                           "Kaskazini-Pemba", "Zanzibar West")] <- "Zanzibar"

geo$zone <- factor(geo$zone)

#######################################
############### AREAs #################
#######################################

areas <- read.csv("C:/Users/Tomas/Documents/Work/LEI/data/TZA/areas_w3.csv") %>%
  select(hhid=y3_hhid, plotnum, area=area_gps_imputed)
areas$hhid <- as.character(areas$hhid)
areas$plotnum <- as.character(areas$plotnum)
areas$area <- ifelse(areas$area %in% 0, NA, areas$area)

#######################################
########### CROSS SECTION #############
#######################################

CS2 <- left_join(oput_maze, plot)
CS2 <- left_join(CS2, lab)
CS2 <- left_join(CS2, areas)
CS2 <- left_join(CS2, implmt)
CS2 <- left_join(CS2, geo)

rm(list=ls()[!ls() %in% "CS2"])

# -------------------------------------
# Make some new variables
# -------------------------------------

CS2 <- ddply(CS2, .(hhid), transform, area_tot=sum(area))

# per hectacre
CS2 <- mutate(CS2,
              yld=qty/area,
              N=N/area,
              P=P/area,
              asset=value/area_tot
)

#######################################
############## CLEANING ###############
#######################################

# remove any Nitrogen values greater than 100
# and any yields greater than 6000
# and any assets greater than 50 million
CS2 <- CS2[!CS2$N > 100, ]
CS2 <- CS2[!CS2$yld > 6000, ]
CS2 <- CS2[!CS2$asset > 50000000, ]

# winsor the nitrogen and maize prices
source("c:/USers/tomas/Documents/work/LEI/winsor5.R")

CS2$maize_prc <- winsor5(CS2$maize_prc, 5)

CS2$WPn <- ifelse(CS2$WPn %in% 0, NA, CS2$WPn)
CS2$WPn <- winsor5(CS2$WPn, 5)
CS2$WPn <- ifelse(is.na(CS2$WPn), 0, CS2$WPn)

# remove zanzibar zones
CS2 <- CS2[!CS2$zone %in% "Zanzibar",]

CS2 <- select(CS2, -plotnum, -qty, -value)

# add final variables
CS2 <- mutate(CS2,
              N2=N^2,
              asset2=asset^2,
              area2=area^2,
              lab2=lab^2,
              y12=1
)

write_dta(CS2, "C:/Users/Tomas/Documents/Work/LEI/TZA12_data.dta")
