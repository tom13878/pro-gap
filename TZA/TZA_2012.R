#######################################
########## TANZANIA 2012-13 ###########
#######################################

# WDswitch
# dataPath <- "D:\\Data\\IPOP\\SurveyData\\"
# wdPath <- "D:\\Dropbox\\Michiel_research\\2285000066 Africa Maize Yield Gap"
# setwd(wdPath)

dataPath <- "C:/Users/Tomas/Documents/LEI/data/TZA/TZA_2012_LSMS_v01_M_STATA_English_labels"


library(haven)
library(dplyr)

options(scipen=999)

#######################################
############### OUTPUT ################
#######################################

# WDswitch
# oput <- read_dta(file.path(dataPath, "TZA\\2012\\Data\\AG_SEC_4A.dta")) %>%
oput <- read_dta(file.path(dataPath, "AG_SEC_4A.dta")) %>%
  dplyr::select(y3_hhid, plotnum, zaocode, inter_crop=ag4a_04,
         harv_area=ag4a_21, qty=ag4a_28, valu=ag4a_29, hybrd=ag4a_08)

oput$inter_crop <- ifelse(oput$inter_crop %in% 1, 1, 0)
oput$hybrd <- ifelse(oput$hybrd %in% 1, 1, 0)
oput$zaocode <- as.integer(oput$zaocode)

legumes <- c(31, 32, 33, 35, 36, 37, 41, 42, 43, 47, 48)

oput_x <- group_by(oput, y3_hhid, plotnum) %>%
  summarise(crop_count=length(unique(zaocode[!is.na(zaocode)])),
            legume=ifelse(any(zaocode %in% legumes), 1, 0))

oput <- left_join(oput, oput_x); rm(oput_x)

oput_maize <- oput[ oput$zaocode %in% 11 & !is.na(oput$qty) & !oput$qty %in% 0, ]
oput_maize$maize_prc <- oput_maize$valu/oput_maize$qty
oput_maize <- dplyr::select(oput_maize, -zaocode, -valu)

rm(list=c("oput", "legumes"))

#######################################
############# CHEMICAL ################
#######################################

# WDswitch
# plot <- read_dta(file.path(dataPath, "TZA\\2012\\Data\\AG_SEC_3A.dta"))%>%
plot <- read_dta(file.path(dataPath, "AG_SEC_3A.dta")) %>%
  dplyr::select(y3_hhid, plotnum, maize=ag3a_07_2,  soil=ag3a_10, slope_farmer=ag3a_17, irrig=ag3a_18, title=ag3a_28, 
         manure=ag3a_41, pest=ag3a_60, pest_q=ag3a_62_1, pest_q_unit=ag3a_62_2, fallow_year=ag3a_22, fallow=ag3a_23)

plot$maize <- ifelse(plot$maize %in% 11, 1, 0)
plot$soil <- factor(plot$soil, levels=c(1,2,3,4), labels=c("Sandy", "Loam", "Clay", "Other"))
plot$slope_farmer <- factor(plot$slope_farmer, levels=c(1,2,3,4), labels=c("Flat bottom", "Flat top", "Slightly sloped", "Very steep"))
plot$title <- as.numeric(as_factor(plot$title))
plot$title <- ifelse(plot$title %in% c(1:10), 1, 0)
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
plot <- dplyr::select(plot, -fallow_year, -pest_q_unit)

# WDswitch
# fert1 <- read_dta(file.path(dataPath, "TZA\\2012\\Data\\AG_SEC_3A.dta")) %>%
fert1 <- read_dta(file.path(dataPath, "AG_SEC_3A.dta")) %>%
  dplyr::select(y3_hhid, plotnum, typ=ag3a_48, qty=ag3a_49, vouch=ag3a_50, valu=ag3a_51)

# WDswitch
# fert2 <- read_dta(file.path(dataPath, "TZA\\2012\\Data\\AG_SEC_3A.dta")) %>%
fert2 <- read_dta(file.path(dataPath, "AG_SEC_3A.dta")) %>%
  dplyr::select(y3_hhid, plotnum, typ=ag3a_55, qty=ag3a_56, vouch=ag3a_57, valu=ag3a_58)

fert1$typ <- as_factor(fert1$typ)
fert1$vouch <- ifelse(fert1$vouch %in% 2, 0, fert1$vouch)

fert2$typ <- as_factor(fert2$typ)
fert2$vouch <- ifelse(fert2$vouch %in% 2, 0, fert2$vouch)

levels(fert1$typ) <- levels(fert2$typ) <-
  c("DAP", "UREA", "TSP", "CAN", "SA", "generic NPK (TZA)", "MRP")

# -------------------------------------
# reorganize data so that observations
# on fertilizer type occupy a single row
# fertilizer is unit of observation
# Data on NPK composition from Sheahan et al (2014), Food Policy
# -------------------------------------

conv <- read.csv(paste(dataPath, "..", "Fert_comp.csv", sep="/")) %>%
  transmute(typ=Fert_type2, n=N_share/100, p=P_share/100) %>%
  filter(typ %in% levels(fert1$typ))

fert1 <- left_join(fert1, conv)
fert2 <- left_join(fert2, conv)
rm(conv)

# -------------------------------------
# organize fertilizer data for analysis

fert <- rbind(fert1, fert2)

fert <- mutate(fert,
               Vfert=valu/qty,
               Qn=qty*n,
               Qp=qty*p)

fert$Pn <- fert$Vfert/fert$n

# Compute subsidised, non-subsidised and mix fertilizer prices per plot
# As valu or N is sometimes 0, all prices that are 0 are set to NA
fertnosub   <- filter(fert, vouch==0) %>%
  group_by(y3_hhid, plotnum) %>%
  summarise(N=sum(Qn, na.rm=TRUE),
            WPnnosub=sum((Qn/N)*Pn, na.rm=TRUE)) %>%
  dplyr::select(-N) %>%
  mutate(WPnnosub = replace(WPnnosub, WPnnosub==0, NA))


fertsub   <- filter(fert, vouch==1) %>%
  group_by(y3_hhid, plotnum) %>%
  summarise(N=sum(Qn, na.rm=TRUE),
            WPnsub=sum((Qn/N)*Pn, na.rm=TRUE)) %>%
  dplyr::select(-N) %>% 
  mutate(WPnsub = replace(WPnsub, WPnsub==0, NA))

fertmix <- filter(fert, vouch %in% c(0,1)) %>%
  group_by(y3_hhid, plotnum) %>%
  summarise(N=sum(Qn, na.rm=TRUE),
            P=sum(Qp, na.rm=TRUE),
            WPn=sum((Qn/N)*Pn, na.rm=TRUE)) %>%
  mutate(WPn = replace(WPn, WPn==0, NA))

# join back with the rest of the data and set N and P to 0 for NA values
plot <- left_join(plot, fertmix) %>%
  left_join(., fertnosub) %>%
  left_join(., fertsub) %>%
  mutate(N = ifelse(is.na(N), 0, N),
         P = ifelse(is.na(P), 0, P))
rm(list=c("fert1", "fert2", "fert", "fertsub", "fertnosub", "fertmix"))

#######################################
############### LABOUR ################
#######################################

# WDswitch
# lab <- read_dta(file.path(dataPath, "TZA\\2012\\Data\\AG_SEC_3A.dta")) %>%
lab <- read_dta(file.path(dataPath, "AG_SEC_3A.dta")) %>%
  dplyr::select(y3_hhid, plotnum, ag3a_72_id1:ag3a_74_16 )

# calculate the total labour days for hired and family labour.
bad <- grep( "ag3a_72_id", names( lab ) )
lab <- lab[, -bad]

bad <- names( lab )[( length( lab )-15 ):length( lab )][seq( from=4, to=16, by=4 )]
lab <- lab[, -which( names( lab ) %in% bad )]

lab <- transmute( lab, y3_hhid, plotnum,
                  fam_lab_days=rowSums( lab[, 3:30], na.rm=TRUE ),
                  hir_lab_days=rowSums( lab[, 32:ncol( lab )], na.rm=TRUE ) )

lab <- transmute(lab, y3_hhid, plotnum, lab=fam_lab_days + hir_lab_days)

# doesn't make sense to have 0 labour on a plot so set values to zero
lab$lab <- ifelse(lab$lab %in% 0, NA, lab$lab)

#######################################
############### ASSETS ################
#######################################

# WDswitch
# implmt <- read_dta(file.path(dataPath, "TZA\\2012\\Data\\AG_SEC_11.dta")) %>%
implmt <- read_dta(file.path(dataPath, "AG_SEC_11.dta")) %>%
  dplyr::select(y3_hhid, itemname, qty=ag11_01, valu=ag11_02) %>%
  filter(!qty %in% 0, !is.na(qty), !valu %in% 0, !is.na(valu)) %>%
  transmute(y3_hhid, valu=qty*valu) %>%
  group_by(y3_hhid) %>%
      summarise(value=sum(valu))

#######################################
############ rural/urban ##############
#######################################

# WDswitch
# rural <-  read_dta(file.path(dataPath, "TZA\\2012\\Data\\HH_SEC_A.dta")) %>%
rural <-  read_dta(file.path(dataPath, "HH_SEC_A.dta")) %>%
    dplyr::select(y3_hhid, rural=y3_rural)

rural$rural <- ifelse(rural$rural %in% 1, 1, 0)

#######################################
############### GEO ###################
#######################################

# WDswitch
# geo <- read.csv(file.path(wdPath, "Analysis\\TZA\\Data\\TZA_geo_total_2012.csv"), stringsAsFactors=F) %>%
geo <- readRDS("C:/Users/Tomas/Documents/LEI/Data/TZA/TZA_data_2012.rds") %>% 
  dplyr::select(y3_hhid, lon, lat, plotnum, SPEI, RootDepth, region=NAME_1,
                AEZ=land03, ph=ph_sd1_sd3, ph2=ph_sd1_sd5,
                SOC=SOC_sd1_sd3, SOC2=SOC_sd1_sd5, rain=gsRainfall,
                plot01,
                dist01, dist02, dist03, dist04, dist05,
                clim02,clim04, clim05, 
                soil01, soil02, soil03, soil04, soil05, soil06, soil07, soil08, soil09, soil10, soil11,
                crops04, crops05, crops07, crops08,
                YA, YW, YP) %>%
  unique()

geo$AEZ <- as.factor(geo$AEZ)
geo$y3_hhid <- as.character(geo$y3_hhid)

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

# WDswitch
# areas <- read.csv("Analysis/TZA/Data/areas_w3.csv") %>%
areas <- read.csv("C:/Users/Tomas/Documents/LEI/data/TZA/areas_w3.csv") %>%
  dplyr::select(y3_hhid, plotnum,
                area_farmer=area.est,
                area_gps=gps_imputed)
areas$y3_hhid <- as.character(areas$y3_hhid)
areas$plotnum <- as.character(areas$plotnum)
areas$area_gps <- ifelse(areas$area_gps %in% 0, NA, areas$area_gps)

# 2012 areas are in acres, change to hecacres in line
# with the 2010 data
areas$area_gps <- areas$area_gps*0.404686 # from wikipedia

areaTotal <- group_by(areas, y3_hhid) %>%
  summarise(area_tot = sum(area_gps, na.rm=TRUE))

areaTotal$area_tot <- ifelse(areaTotal$area_tot %in% 0, NA, areaTotal$area_tot)

#######################################
########## TRANSPORT COSTS ############
#######################################

# WDswitch
# tc <- read_dta(file.path(dataPath, "TZA\\2012\\Data\\AG_SEC_5A.dta")) %>%
tc <- read_dta(file.path(dataPath, "AG_SEC_5A.dta")) %>%
  dplyr::filter(zaocode %in% 11) %>%
  dplyr::select(y3_hhid, trans=ag5a_18, trans_dist=ag5a_19, trans_cost=ag5a_22)

tc$trans <- ifelse(tc$trans %in% 1, 1, 0)

#######################################
########### SOCIO/ECONOMIC ############
#######################################

# WDswitch
# HH12 <- read_dta(file.path(dataPath, "TZA\\2012\\Data\\HH_SEC_B.dta")) %>%
HH12 <- read_dta(file.path(dataPath, "HH_SEC_B.dta")) %>%
  select(y3_hhid, indidy3, status=hh_b05, sex=hh_b02,
         yob=hh_b03_1, age=hh_b04, years=hh_b26)

# 99 in years variable means that the household member has
# lived in the community all his/her life
HH12$years <- as.numeric(HH12$years)
HH12$years <- ifelse(HH12$years %in% 99, HH12$age, HH12$years)
HH12$status <- as_factor(HH12$status)
HH12$sex <- as_factor(HH12$sex)
HH12$yob <- as.integer(HH12$yob)

HH12$cage <- cut(HH12$age, breaks = c(0, 15, 55, max(HH12$age, na.rm=TRUE)),
                 labels=1:3, include.lowest = TRUE, right = TRUE)

# -------------------------------------
# education of household head and sum of
# education of all household members
# between the ages of 15 and 55

ed <- read_dta(file.path(dataPath, "HH_SEC_C.dta")) %>%
  select(y3_hhid, indidy3, ed_any=hh_c03, start=hh_c04, end=hh_c08)

ed$end <- as.integer(as.character(ed$end))
ed$end <- ifelse(ed$end %in% 9999, NA, ed$end)
ed$ed_any <- as_factor(ed$ed_any) # any education

# join HH10 and ed to find amount of schooling
HH12 <- left_join(HH12, ed)
HH12$education <- HH12$end - (HH12$yob + HH12$start)
HH12$education <- ifelse(HH12$ed_any %in% "NO", 0, HH12$education)
HH12 <- select(HH12, -start, -end, -yob)

# if anyone received negative years of schooling, set to NA
HH12$education <- ifelse(HH12$education < 0, NA, HH12$education)

# summarise the data: get sum of education
# of household members 15-55 and number of
# household members 15-55
HH12_x <- group_by(HH12, y3_hhid) %>%
  summarise(education1555=sum(education[cage %in% 2], na.rm=T),
            N1555=sum(cage %in% 2))
HH12 <- left_join(HH12, HH12_x); rm(HH12_x)
HH12 <- filter(HH12, status %in% "HEAD") %>% select(-indidy3, -status, -cage, -ed_any)

# -------------------------------------
# plot ownership

# WDswitch
# own <- read_dta(file.path(dataPath, "TZA\\2012\\Data\\AG_SEC_3A.dta")) %>%
own <- read_dta(file.path(dataPath, "AG_SEC_3A.dta")) %>%
  dplyr::select(y3_hhid, plotnum, own=ag3a_25)

own$own <- ifelse(own$own %in% 1 | own$own %in% 5, 1, 0)

#######################################
########### CROSS SECTION #############
#######################################

# joins at the plot level
TZA2012 <- left_join(oput_maize, plot)
TZA2012 <- left_join(TZA2012, lab)
TZA2012 <- left_join(TZA2012, areas)
TZA2012 <- left_join(TZA2012, own)

# joins at the household level
TZA2012 <- left_join(TZA2012, implmt)
TZA2012 <- left_join(TZA2012, geo)
TZA2012 <- left_join(TZA2012, rural)
TZA2012 <- left_join(TZA2012, HH12)
TZA2012 <- left_join(TZA2012, tc)
TZA2012 <- left_join(TZA2012, areaTotal)

rm(list=ls()[!ls() %in% "TZA2012"])

# -------------------------------------
# Make some new variables
# -------------------------------------

# per hectacre
TZA2012 <- mutate(TZA2012,
              yld=qty/area_gps,
              N=N/area_gps,
              P=P/area_gps,
              lab=lab/area_gps,
              pest_q=pest_q/area_gps,
              asset=value/area_tot
)

TZA2012 <- dplyr::select(TZA2012, -qty, -value)

# add final variables
TZA2012 <- mutate(TZA2012,
              N2=N^2,
              asset2=asset^2,
              area2=area_gps^2,
              lab2=lab^2,
              surveyyear=2012
)

# save to file
# save(TZA2012, file=".\\Analysis\\TZA\\Data\\TZA12_data.RData")
# write.csv(TZA2012, "C:/Users/Tomas/Documents/LEI/TZA12_data.csv", row.names=FALSE)

