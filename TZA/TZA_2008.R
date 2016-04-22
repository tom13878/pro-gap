#######################################
########## TANZANIA 2008-09 ###########
#######################################

dataPath <- "C:/Users/Tomas/Documents/LEI/data/TZA"

library(haven)
library(stringr)
library(plyr)
library(dplyr)

options(scipen=999)

#######################################
############### OUTPUT ################
#######################################

oput <- read_dta(file.path(dataPath, "TZA2008/TZNPS1AGDTA_E/SEC_4A.dta")) %>%
  dplyr::select(hhid, plotnum, zaocode, inter_crop=s4aq6,
                harv_area=s4aq8, qty=s4aq15, valu=s4aq16, hybrd=s4aq22)

oput$inter_crop <- ifelse(oput$inter_crop %in% 1, 1, 0)
oput$hybrd <- ifelse(oput$hybrd %in% 2, 1, 0)
oput$zaocode <- as.integer(oput$zaocode)

# -------------------------------------
# create dummy variables for crop groups
# (fruit, cash crops (permanent),
# Cereals/Tubers/Roots, cash crops (not permanent),
# vegetables, legumes)
# -------------------------------------

fruit <- c(70:74, 76:85, 97:99, 67, 38, 39)
cashCropsPerm <- c(53:61, 63:66, 18, 34, 21, 75, 44:46) # permanent cash crops
CTR <- c(11:17, 22:27) # Cereals, Tubers, Roots
cashCropNPerm <- c(50, 51, 53, 62, 19) # non permanent cash crops
vegetables <- c(86:96, 100, 101)
legumes <- c(31, 32, 33, 35, 36, 37, 41, 42, 43, 47, 48)

oput_x <- group_by(oput, hhid, plotnum) %>%
  summarise(crop_count=length(unique(zaocode[!is.na(zaocode)])),
            fruit=ifelse(any(zaocode %in% fruit), 1, 0),
            cashCropsPerm=ifelse(any(zaocode %in% cashCropsPerm), 1, 0),
            CTR=ifelse(any(zaocode %in% CTR), 1, 0),
            cashCropNPerm=ifelse(any(zaocode %in% cashCropNPerm), 1, 0),
            vegetables=ifelse(any(zaocode %in% vegetables), 1, 0),
            legume=ifelse(any(zaocode %in% legumes), 1, 0),
            maize=ifelse(any(zaocode %in% 11), 1, 0), # maize has crop code 11
            wheat=ifelse(any(zaocode %in% 16), 1, 0)) # wheat has crop code 16

oput <- left_join(oput, oput_x); rm(oput_x)

# for productivity of maize farmers we are only interested
# in the maize farmers, exclude everyone else, and farmers
# who responded they produced zero maize, or did not respond (NA)

oput_maize <- oput[ oput$zaocode %in% 11 & ! is.na(oput$qty) & !oput$qty %in% 0, ]
oput_maize$maize_prc <- oput_maize$valu/oput_maize$qty
oput_maize <- dplyr::select(oput_maize, -zaocode, -valu)

rm(list=c("legumes", "cashCropNPerm", "cashCropsPerm",
          "CTR", "fruit", "vegetables"))

#######################################
############# CHEMICAL ################
#######################################

plot <- read_dta(file.path(dataPath, "TZA2008/TZNPS1AGDTA_E/SEC_3A.dta")) %>%
  dplyr::select(hhid, plotnum, maize=s3aq5code, soil=s3aq7, slope_farmer=s3aq14, irrig=s3aq15, title=s3aq25,
                manure=s3aq37, pest=s3aq49, pest_q=s3aq51_amount, pest_q_unit=s3aq51_measure, fallow_year=s3aq19, fallow=s3aq20)

plot$maize <- ifelse(plot$maize %in% 11, 1, 0)
plot$soil <- factor(plot$soil, levels=c(1,2,3,4), labels=c("Sandy", "Loam", "Clay", "Other"))
plot$slope_farmer <- factor(plot$slope_farmer, levels=c(1,2,3,4), labels=c("Flat bottom", "Flat top", "Slightly sloped", "Very steep"))
plot$title <- ifelse(plot$title %in% 1, 1, 0) # assume that they don't have a title if NA
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
plot <- dplyr::select(plot, -fallow_year, - pest_q_unit)

# inorganic fertilizer - note that there is no inorganic fertilizer
# voucher question as in 2010 and 2012 surveys. Farmers are only asked
# about one inorganic fertilizer

fert <- read_dta(file.path(dataPath, "TZA2008/TZNPS1AGDTA_E/SEC_3A.dta")) %>%
  dplyr::select(hhid, plotnum, typ=s3aq44, qty=s3aq45, valu=s3aq46)

fert$typ <- as_factor(fert$typ)

levels(fert$typ) <- 
  c("DAP", "UREA", "TSP", "CAN", "SA", "generic NPK (TZA)", "MRP")

# -------------------------------------
# reorganize data so that observations
# on fertilizer type occupy a single row
# fertilizer is unit of observation
# Data on NPK composition from Sheahan et al (2014), Food Policy
# -------------------------------------

conv <- read.csv(paste(dataPath, "Fert_comp.csv", sep="/")) %>%
  transmute(typ=Fert_type2, n=N_share/100, p=P_share/100) %>%
  filter(typ %in% levels(fert$typ))

fert <- left_join(fert, conv)

# -------------------------------------
# organize fertilizer data for analysis

fert <- mutate(fert,
               Vfert=valu/qty,
               Qn=qty*n,
               Qp=qty*p)

fert$WPn <- fert$Vfert/fert$n

# join back with the rest of the data and set N and P to 0 for NA values
plot <- left_join(plot, fert) 

rm(list=c("conv", "fert"))

#######################################
############### LABOUR ################
#######################################

lab <- read_dta(file.path(dataPath, "TZA2008/TZNPS1AGDTA_E/SEC_3A.dta")) %>%
  dplyr::select( hhid, plotnum, s3aq61_id1:s3aq63_9)

# remove houshold labour IDs and question ag3a_71 which we don't need
bad <- grep( "s3aq61_id", names( lab ) )
lab <- lab[, -bad]
lab <- dplyr::select( lab, -s3aq62 )

# remove variables that refer to wage paid for hired labour
# this could be added later to consider input costs
bad <- names( lab )[( length( lab )-8 ):length( lab )][seq( from=3, to=9, by=3 )]
lab <- lab[, -which( names( lab ) %in% bad )]

# create a dataframe with just family and hired labour
lab <- transmute( lab, hhid, plotnum,
                  fam_lab_days=rowSums( lab[, 3:38], na.rm=TRUE ),
                  hir_lab_days=rowSums( lab[, 39:ncol( lab )], na.rm=TRUE ) )

lab <- transmute(lab, hhid, plotnum, lab=fam_lab_days + hir_lab_days)

# doesn't make sense to have 0 labour on a plot so set values to zero
lab$lab <- ifelse(lab$lab %in% 0, NA, lab$lab)

#######################################
############### GEO ###################
#######################################

loc <- read_dta(file.path(dataPath, "TZA2008/TZNPS1HHDTA_E/SEC_A_T.dta")) %>%
  dplyr::select(ea_id2008 = ea, hhid, rural, region_code_lsms=region, district_code_lsms=district) %>%
  mutate(rural = ifelse(rural == "Rural", 1, 0))

loc$region_name_lsms <- as_factor(loc$region_code_lsms)
loc$region_code_lsms <- as.integer(loc$region_code_lsms)
loc$district_code_lsms <- as.integer(loc$district_code_lsms)

# add a zone variable
loc$zone[loc$region_name_lsms %in% c("Kagera","Mwanza", "Mara")] <- "Lake"
loc$zone[loc$region_name_lsms %in% c("Shinyanga","Kigoma", "Tabora")] <- "Western"
loc$zone[loc$region_name_lsms %in% c("Arusha","Kilimanjaro", "Manyara", "Tanga")] <- "Northern"
loc$zone[loc$region_name_lsms %in% c("Singida","Dodoma")] <- "Central"
loc$zone[loc$region_name_lsms %in% c("Rukwa", "Mbeya","Iringa")] <- "Southern Highlands"
loc$zone[loc$region_name_lsms %in% c("Pwani","Morogoro", "Dar-Es-Salaam")] <- "Eastern"
loc$zone[loc$region_name_lsms %in% c("Lindi","Ruvuma", "Mtwara")] <- "Southern"
loc$zone[loc$region_name_lsms %in% c("Dar es Salaam")] <- "Dar es Salaam" 
loc$zone[loc$region_name_lsms %in% c("Kaskazini Unguja", "Kaskazini Unguja", "Kusini Pemba", "Kusini Unguja",
                                     "Kaskazini Pemba", "Mjini Magharibi")] <- "Zanzibar"

# we do not need the islands
loc <- loc[!loc$region_code_lsms > 21,]

loc$zone <- factor(loc$zone)

# read in the household geovariables

geo <- read_dta(file.path(dataPath, "TZA2008/TZNPS1_consdta/HH.Geovariables_Y1.dta")) %>%

  
#######################################
############### ASSETS ################
#######################################

# WDswitch
implmt <- read_dta(file.path(dataPath, "TZA2008/TZNPS1AGDTA_E/SEC_11_ALL.dta")) %>%
  dplyr::select(hhid, impcode, qty=s11q1, valu=s11q2) %>%
  filter(!qty %in% 0, !is.na(qty), !valu %in% 0, !is.na(valu)) %>%
  transmute(hhid, valu=qty*valu) %>%
  group_by(hhid) %>%
  summarise(value=sum(valu))
