#######################################
########## TANZANIA 2010-11 ###########
#######################################

setwd("c:/USers/tomas/Documents/work/LEI")

library(haven)
library(stringr)
library(plyr)
library(dplyr)

options(scipen=999)


#######################################
############### OUTPUT ################
#######################################

oput <- read_dta("data/TZA/TZNPS2AGRDTA/AG_SEC4A.dta") %>%
  select(y2_hhid, plotnum, zaocode, inter_crop=ag4a_04,
         harv_area=ag4a_08, qty=ag4a_15, valu=ag4a_16, hybrd=ag4a_23)

oput$inter_crop <- ifelse(oput$inter_crop %in% 1, 1, 0)
oput$hybrd <- ifelse(oput$hybrd %in% 2, 1, 0)
oput$zaocode <- as.integer(oput$zaocode)

legumes <- c(31, 32, 33, 35, 36, 37, 41, 42, 43, 47, 48)
oput <- ddply(oput, .(y2_hhid, plotnum), transform,
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

plot <- read_dta("data/TZA/TZNPS2AGRDTA/AG_SEC3A.dta") %>%
  select(y2_hhid, plotnum, maze=zaocode, irrig=ag3a_17,
         manure=ag3a_39, pest=ag3a_58, fallow_year=ag3a_21, fallow=ag3a_22)

plot$maze <- ifelse(plot$maze %in% 11, 1, 0)
plot$irrig <- ifelse(plot$irrig %in% 1, 1, 0)
plot$manure <- ifelse(plot$manure %in% 1, 1, 0)
plot$pest <- ifelse(plot$pest %in% 1, 1, 0)

# two questions on fallow - make sure they match up correctly
plot$fallow_year <- ifelse(plot$fallow_year %in% 98, NA, plot$fallow_year)
plot$fallow <- ifelse(plot$fallow_year %in% 0, 0, plot$fallow )
plot$fallow <- ifelse(is.na(plot$fallow_year), NA, plot$fallow)
plot <- select(plot, -fallow_year)

fert1 <- read_dta("data/TZA/TZNPS2AGRDTA/AG_SEC3A.dta") %>%
  select(y2_hhid, plotnum, typ=ag3a_46, qty=ag3a_47, vouch=ag3a_48, valu=ag3a_49)

fert2 <- read_dta("data/TZA/TZNPS2AGRDTA/AG_SEC3A.dta") %>%
  select(y2_hhid, plotnum, typ=ag3a_53, qty=ag3a_54, vouch=ag3a_55, valu=ag3a_56)

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

rm(list=c("comp", "typ", "n", "p", "k"))

fert <- rbind(fert1, fert2)

fert <- mutate(fert,
               Vfert=valu/qty,
               Qn=qty*n,
               Qp=qty*p)

fert$Pn <- fert$Vfert/fert$n

fert <- group_by(fert, y2_hhid, plotnum) %>%
  summarise(N=sum(Qn, na.rm=TRUE),
            P=sum(Qp, na.rm=TRUE),
            WPn=sum((Qn/N)*Pn, na.rm=TRUE))

# join back with the rest of the data
plot <- left_join(plot, fert)

rm(list=c("fert1", "fert2", "fert"))

#######################################
############### LABOUR ################
#######################################

lab <- read_dta( "data/TZA/TZNPS2AGRDTA/AG_SEC3A.dta") %>%
  select( y2_hhid, plotnum, ag3a_70_id1:ag3a_72_9 )
                  
# remove houshold labour IDs and question ag3a_71 which we don't need
bad <- grep( "ag3a_70_id", names( lab ) )
lab <- lab[, -bad]
lab <- select( lab, -ag3a_71 )

# remove the wage paid in shillings to hired labour - might want this back later
bad <- names( lab )[( length( lab )-15 ):length( lab )][seq( from=4, to=16, by=4 )]
lab <- lab[, -which( names( lab ) %in% bad )]

# create a dataframe with just family and hired labour
lab <- transmute( lab, y2_hhid, plotnum,
                  fam_lab_days=rowSums( lab[, 3:26], na.rm=TRUE ),
                  hir_lab_days=rowSums( lab[, 27:ncol( lab )], na.rm=TRUE ) )

lab <- transmute(lab, y2_hhid, plotnum, lab=fam_lab_days + hir_lab_days) 

# doesn't make sense to have 0 labour on a plot so set values to zero
lab$lab <- ifelse(lab$lab %in% 0, NA, lab$lab)

#######################################
############### GEO ###################
#######################################

geo <- read.csv("data/TZA/TZA_geo.total2010.csv") %>%
  select(y2_hhid, lon, lat, SPEI, RootDepth, region=NAME_1,
         AEZ=land03) %>% unique()

geo$y2_hhid <- as.character(geo$y2_hhid)
geo$y2_hhid <- ifelse(str_length(geo$y2_hhid) < 16, paste("0", geo$y2_hhid, sep=""), geo$y2_hhid)


#######################################
############### AREAs #################
#######################################

areas <- read_dta("data/TZA/areas_tza_y2_imputed.dta") %>%
  select(y2_hhid=case_id, plotnum, area=area_gps_mi_50)

areas$area <- ifelse(areas$area %in% 0, NA, areas$area)

#######################################
############### ASSETS ################
#######################################

implmt <- read_dta("c:/USers/tomas/Documents/work/LEI/data/TZA/TZNPS2AGRDTA/AG_SEC11.dta") %>%
  select(y2_hhid, itemcode, qty=ag11_01, valu=ag11_02) %>%
  filter(!qty %in% 0, !is.na(qty), !valu %in% 0, !is.na(valu)) %>%
  transmute(y2_hhid, valu=qty*valu) %>%
  group_by(y2_hhid) %>%
  summarise(value=sum(valu))

#######################################
########### CROSS SECTION #############
#######################################

CS1 <- left_join(oput_maze, plot)
CS1 <- left_join(CS1, lab)
CS1 <- left_join(CS1, areas)
CS1 <- left_join(CS1, implmt)
CS1 <- left_join(CS1, geo)

# -------------------------------------
# change the y2_hhid to the y3_hhid
# for later use in the panel.
# -------------------------------------

setwd("c:/USers/tomas/Documents/work/LEI/data/TZA/TZA_2012_LSMS_v01_M_STATA_English_labels")
key <- read_dta('NPSY3.PANEL.KEY.dta')
key$y2_hhid <- zap_empty(key$y2_hhid)
key$y3_hhid <- zap_empty(key$y3_hhid)

key <- select(key, y2_hhid, y3_hhid) %>%
  na.omit() %>% unique()

CS1 <- left_join(CS1, key) %>%
  rename(hhid=y3_hhid) %>%
  select(hhid, everything(), -y2_hhid)

rm(list=ls()[!ls() %in% "CS1"])

# -------------------------------------
# Make some new variables
# -------------------------------------

CS1 <- ddply(CS1, .(hhid), transform, area_tot=sum(area))

# per hectacre
CS1 <- mutate(CS1,
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
CS1 <- CS1[!CS1$N > 100, ]
CS1 <- CS1[!CS1$yld > 6000, ]
CS1 <- CS1[!CS1$asset > 50000000, ]

# winsor the nitrogen and maize prices
source("c:/USers/tomas/Documents/work/LEI/winsor5.R")

CS1$maize_prc <- winsor5(CS1$maize_prc, 5)

CS1$WPn <- ifelse(CS1$WPn %in% 0, NA, CS1$WPn)
CS1$WPn  <- winsor5(CS1$WPn, 5)
CS1$WPn <- ifelse(is.na(CS1$WPn), 0, CS1$WPn)

# -------------------------------------
# Inflate 2010 prices to 2012 prices
# using inflation rate for 2010 and 2011
# from world bank
# -------------------------------------

CS1$asset <- CS1$asset*(1+0.062)*(1+0.12)
CS1$maize_prc <- CS1$maize_prc*(1+0.062)*(1+0.12)
CS1$WPn <- CS1$WPn*(1+0.062)*(1+0.12)

CS1 <- select(CS1, -plotnum, -qty, -value)

# add final variables
CS1 <- mutate(CS1,
             N2=N^2,
             asset2=asset^2,
             area2=area^2,
             lab2=lab^2,
             y12=0
)

# save to file
write_dta(CS1, "C:/Users/Tomas/Documents/Work/LEI/TZA10_data.dta")
