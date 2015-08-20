#######################################
####### GHANA data preparation ########
#######################################  

# 20/08/2015

# for munging
library(haven)
library(reshape2)
library(plyr)
# library(Hmisc)
detach(package:dplyr)
library(dplyr)

fp1 <- "C:/Users/Tomas/Documents/Work/LEI/"
setwd(fp1)


#######################################
############### output ################
####################################### 

# -------------------------------------
# output section has to be manipulated 
# because it has been recorded in a bad 
# way. Not one observation per row!
# -------------------------------------

oput_maj <- read_dta("data/GHA/S4AV1.dta")
oput_maj$id1 <- as.character(as_factor(oput_maj$id1))
oput_maj$s4v_a78i <- as_factor(oput_maj$s4v_a78i)
oput_maj$s4v_a78ii <- as_factor(oput_maj$s4v_a78ii)
oput_maj$s4v_a78iii <- as_factor(oput_maj$s4v_a78iii)
oput_maj$s4v_a78iv <- as_factor(oput_maj$s4v_a78iv)
oput_maj$s4v_a78v <- as_factor(oput_maj$s4v_a78v)


# need to select out the data seperately and rbind everything together
# first crop
crop_maj_1 <- select(oput_maj, reg=id1, id3, hhno, plot_no=s4av1_plotno, s4v_a78i, s4v_a80i:s4v_a87 )
crop_maj_2 <- select(oput_maj, reg=id1, id3, hhno, plot_no=s4av1_plotno, s4v_a78ii, s4v_a88i:s4v_a95)
crop_maj_3 <- select(oput_maj, reg=id1, id3, hhno, plot_no=s4av1_plotno, s4v_a78iii, s4v_a96i:s4v_a103)
crop_maj_4 <- select(oput_maj, reg=id1, id3, hhno, plot_no=s4av1_plotno, s4v_a78iv, s4v_a104i:s4v_a111)
crop_maj_5 <- select(oput_maj, reg=id1, id3, hhno, plot_no=s4av1_plotno, s4v_a78v, s4v_a112i:s4v_a119)

# drop any NA values which are only there because of weird format
crop_maj_1 <- crop_maj_1[!is.na(crop_maj_1$s4v_a78i),]
crop_maj_2 <- crop_maj_2[!is.na(crop_maj_2$s4v_a78ii),]
crop_maj_3 <- crop_maj_3[!is.na(crop_maj_3$s4v_a78iii),]

# the 4th and fifth rows have a problem in them because of classes of columns
crop_maj_4$s4v_a111 <- as.numeric(crop_maj_4$s4v_a111)
crop_maj_5$s4v_a119 <- as.numeric(crop_maj_5$s4v_a119)

# then drop NA values
crop_maj_4 <- crop_maj_4[!is.na(crop_maj_4$s4v_a78iv),]
crop_maj_5 <- crop_maj_5[!is.na(crop_maj_5$s4v_a78v),]

# now need to change the names so they are all the same
names(crop_maj_1) <- names(crop_maj_2) <- names(crop_maj_3) <- names(crop_maj_4) <- names(crop_maj_5) <-
        c("reg", "id3", "hhno", "plot_no", "crop", "crop_id", "type", "quantity", "unit", "value_c",
          "value_p", "revenue_c", "revenue_p", "left_over", "left_over_value_c",
          "left_over_value_p", "disease", "percent_lost")

# Bind together all of the output values
oput_maj_tot <- rbind(crop_maj_1, crop_maj_2, crop_maj_3, crop_maj_4, crop_maj_5)
rm(list=c("crop_maj_1", "crop_maj_2", "crop_maj_3", "crop_maj_4", "crop_maj_5"))

# make some factor variables
oput_maj_tot$crop <- as_factor(oput_maj_tot$crop)
oput_maj_tot$type <- as_factor(oput_maj_tot$type)
oput_maj_tot$left_over <- as_factor(oput_maj_tot$left_over)
oput_maj_tot$disease <- as_factor(oput_maj_tot$disease)

# add a variable for the number of crops on a plot
# and whether or not a legume was grown on that plot

oput_maj_tot <- ddply(oput_maj_tot, .(hhno, plot_no), transform,
                      crop_count=length(crop[!is.na(crop)]),
                      legume=ifelse(any(crop %in% "Beans/Peas"), 1, 0))

# select only maize oput and chosen variables
oput_maj_mze <- filter(oput_maj_tot, crop %in% "Maize") %>%
        select(hhno, plot_no, qty=quantity, unit, value_c, crop_count, legume)


# -------------------------------------
# There are many units of output and 
# it is necessary to make conversions
# unfortunately conversions are not 
# part of the data. Instead, section 5A
# of the community questionnaire deals
# with conversions from local units
# to kilograms at the market level. 
# these are used as the conversions for
# output but note that there is a lot of
# variation in units across communities.
# -------------------------------------

# convert to kilograms using conversions in sec 5A of
# rural community questionnaire

SEC5A <- read_dta("data/GHA/SEC 5A.dta")

# construct an auxillary conversion table
aux <- melt(SEC5A, id.vars=c("reg", "EA_No", "commcode", "s5", "s5a_1"))
aux <- aux[!is.na(aux$value),]
aux <- ddply(aux, .(s5a_1, variable), summarize, kilo_bar = mean(value, na.rm=TRUE))

# Add unit codes by hand from BID
variable <- as.character(unique(aux$variable))[order(as.character(unique(aux$variable)))]
unit <- c(2, 27, 4, 6, 7, 29, 8, 9, 11, 12, 14, 17, 18, 19, 34, 37, 23, 24, 26)
cnvrt <- data.frame(variable, unit)

# join conversion table with unit table
aux <- left_join(aux, cnvrt)
aux_mze <- filter(aux, s5a_1 %in% "MAIZE")

# join unit table with maize output. 
# For goods other than maize this
# conversions are difficult because of spelling
# mistakes

oput_maj_mze <- left_join(oput_maj_mze, select(aux_mze, unit, kilo_bar))

# calculate quantity in kilograms

oput_maj_mze <- mutate(oput_maj_mze, qty = qty *  kilo_bar)

# get rid of the unit variable and NA values for maize quantity
oput_maj_mze <- select(oput_maj_mze, -unit, -kilo_bar)
oput_maj_mze <- oput_maj_mze[!is.na(oput_maj_mze$qty) & !oput_maj_mze$qty %in% 0,]
oput_maj_mze$maze_prc <- oput_maj_mze$qty/oput_maj_mze$value_c
oput_maj_mze <- select(oput_maj_mze, -value_c)

rm(list=c("aux", "aux_mze", "cnvrt", "oput_maj", "oput_maj_tot", "SEC5A",
          "unit", "variable"))

#######################################
################ area #################
#######################################

area <- read_dta("data/GHA/S4AII.dta") 

area <- select(area, reg=id1, id2, hhno, plotno=plot_no,
                    size=s4aii_a10, unit=s4aii_a11, test_area=s4aii_a12,
                    test_compare=s4aii_a13, test_in=s4aii_a14, in_test=s4aii_a15a,
                    area_ha)

# use as_factor to get labels. 
area$reg <- as.character(as_factor(area$reg))
area$unit <- as_factor(area$unit)
area$test_area <- as_factor(area$test_area)
area$test_compare <- as_factor(area$test_compare)

# for now we just need the area in hectacres
area <- select(area, reg, hhno, plotno,  area_ha)


#######################################
############## CHEMICALS ##############
#######################################

# -------------------------------------
# Similar to output chemical inputs have
# been recorded in a bizarre way and need
# to be rearranged for both seasons
# -------------------------------------

# -------------------------------------
# Major season
# -------------------------------------

chem_maj <- read_dta("data/GHA/S4AVI1.dta") 

chem_maj_1 <- select(chem_maj, hhno, plotno=s4avi1_plotno, s4avi_a162:s4avi_a169iv)
chem_maj_2 <- select(chem_maj, hhno, plotno=s4avi1_plotno, s4avi_a170:s4avi_a177iv)
chem_maj_3 <- select(chem_maj, hhno, plotno=s4avi1_plotno, s4avi_a178:s4avi_a185iv)
chem_maj_4 <- select(chem_maj, hhno, plotno=s4avi1_plotno, s4avi_a186:s4avi_a193iv)
chem_maj_5 <- select(chem_maj, hhno, plotno=s4avi1_plotno, s4avi_a194:s4avi_a201iv)

chem_maj_1 <- chem_maj_1[!is.na(chem_maj_1$s4avi_a162),]
chem_maj_2 <- chem_maj_2[!is.na(chem_maj_2$s4avi_a170),]
chem_maj_3 <- chem_maj_3[!is.na(chem_maj_3$s4avi_a178),]
chem_maj_4 <- chem_maj_4[!is.na(chem_maj_4$s4avi_a186),]
chem_maj_5 <- chem_maj_5[!is.na(chem_maj_5$s4avi_a194),]

names(chem_maj_1) <-
        names(chem_maj_2) <-
        names(chem_maj_3) <-
        names(chem_maj_4) <-
        names(chem_maj_5) <- c("hhno", "plotno", "chem_use", "type",
                          "qty", "unit", "value_c", "value_p", "gov", "gov_qty",
                          "gov_unit", "gov_value_c", "gov_value_p", "crop1",
                          "crop2", "crop3", "crop4")


# bind all chemicals together and make factors from labelled vectors
chem_maj_tot <- rbind(chem_maj_1, chem_maj_2, chem_maj_3, chem_maj_4, chem_maj_5)
rm(list=c("chem_maj_1", "chem_maj_2", "chem_maj_3", "chem_maj_4", "chem_maj_5"))

# for now only select variables you need for analysis
chem_maj_tot <- select(chem_maj_tot, -chem_use, -value_p, -gov, - gov_qty, -gov_unit, -gov_value_c, -gov_value_p)

# make factors of important variables
chem_maj_tot$type <- as_factor(chem_maj_tot$type)
chem_maj_tot$unit <- as_factor(chem_maj_tot$unit)
chem_maj_tot$crop1 <- as_factor(chem_maj_tot$crop1)
chem_maj_tot$crop2 <- as_factor(chem_maj_tot$crop2)
chem_maj_tot$crop3 <- as_factor(chem_maj_tot$crop3)
chem_maj_tot$crop4 <- as_factor(chem_maj_tot$crop4)

# grab only chemicals applied to maize
chem_maj_tot <- melt(chem_maj_tot, measure.vars=c("crop1", "crop2", "crop3", "crop4")) %>%
        rename(crop=value) %>% select(-variable)

# remove any NA values for chemical type
chem_maj_tot <- chem_maj_tot[!is.na(chem_maj_tot$type),]
chem_maj_mze <- chem_maj_tot[chem_maj_tot$crop %in% "Maize",]

# -------------------------------------
# create binary variables for whether or
# not there is certain chemicals used
# -------------------------------------

# maize level - so pesticide or organic fertilizer used on maize
chem_maj_mze <- ddply(chem_maj_mze, .(hhno, plotno), summarise,
                          pest=ifelse(any(type %in% c("Herbicide", "Insecticide", "Fungicide")), 1, 0),
                          manure=ifelse(any(type %in% "Fertilizer (organic)"), 1, 0),
                          inorg=ifelse(any(type %in% "Fertilizer (inorganic)"), 1, 0))

chem_maj_mze <- na.omit(chem_maj_mze)

rm(list=c("chem_maj", "chem_maj_tot"))

#######################################
################ seeds ################
#######################################

# seeds <- read_dta("data/GHA/S4AVIII1.dta")
# seeds$id1 <- as.character(as_factor(seeds$id1))
# 
# seeds1 <- select(seeds, hhno, plotno=s4aviii1_plotno, crop=s4aviii_248ii, seed_type=s4aviii_249, srce=s4aviii_250)
# seeds2 <- select(seeds, hhno, plotno=s4aviii1_plotno, crop=s4aviii_253ii, seed_type=s4aviii_254, srce=s4aviii_255)
# seeds3 <- select(seeds, hhno, plotno=s4aviii1_plotno, crop=s4aviii_258ii, seed_type=s4aviii_259, srce=s4aviii_260)
# seeds4 <- select(seeds, hhno, plotno=s4aviii1_plotno, crop=s4aviii_263ii, seed_type=s4aviii_264, srce=s4aviii_265)
# 
# seeds_tot <- rbind(seeds1, seeds2, seeds3, seeds4)
# rm(list=c("seeds", "seeds1", "seeds2", "seeds3", "seeds4"))
# 
# seeds_tot$seed_type <- as_factor(seeds_tot$seed_type)
# seeds_tot$srce <- as_factor(seeds_tot$srce)
# seeds_tot$crop <- zap_empty(seeds_tot$crop)
# 
# seeds_tot <- filter(seeds_tot, !(is.na(crop)))
# seeds_mze <- seeds_tot[seeds_tot$crop %in% "MAIZE", ]


#######################################
############### Assets ################
#######################################

# -------------------------------------
# follow Sheahan and define assets as
# value of the livestock and farm
# implements
# -------------------------------------

# -------------------------------------
# livestock - note there is a question on
# value of all animals sold
# -------------------------------------

lvstk <- read_dta("data/GHA/S3AI.dta") %>% 
        select(hhno, lvstk=animal_id, qty=s3ai_1, valu=s3ai_3i) %>%
        mutate(prc=valu/qty)

lvstk$lvstk <- as_factor(lvstk$lvstk)

# select only the larger animals
big <- c("Drought Animal", "Cattle", "Sheep", "Goats", "Pigs")
lvstk <- lvstk[lvstk$lvstk %in% big,]

lvstk <- ddply(lvstk, .(lvstk), transform,
               valu=ifelse(is.na(valu), mean(prc, na.rm=TRUE)*qty, valu))

# calculate per houshold livestock wealth
lvstk <- group_by(lvstk, hhno) %>%
        summarise(lvstk_valu=sum(valu))

cut <- quantile(lvstk$lvstk_valu, probs=seq(0,1, 0.05))
hist(lvstk$lvstk_valu[lvstk$lvstk_valu >= cut[2] & lvstk$lvstk_valu <= cut[20]],
     col="red", breaks=30, main="household wealth (lvstk)", xlab="Cedis")

# -------------------------------------
# value of farm equipment
# -------------------------------------

implmt <- read_dta("data/GHA/S3AII.dta") %>%
        select(hhno, implmt=s3aii_0, qty=s3aii_a, valu=s3aii_c1) %>%
        filter(!is.na(implmt) & !qty %in% 0)

implmt$implmt <- as_factor(implmt$implmt)

# drop any misisng values for valu variable
# only 8 of them. get implmt valu per hh

implmt <- filter(implmt, !is.na(valu)) %>%
        group_by(hhno) %>% 
        summarise(implmt_valu=sum(valu))


#######################################
############### LABOUR ################
#######################################

lab_val1 <- read_dta("data/GHA/S4AIX1.dta") %>%
  select(-id1, -id2, -hhno, s4aix1_plotno)
lab_val1[is.na(lab_val1)] <- 0
lab_val1 <- rowSums(lab_val1)
lab1 <- read_dta("data/GHA/S4AIX1.dta") %>%
  select(hhno, plotno=s4aix1_plotno) %>% cbind(lab_val1)

lab_val2 <- read_dta("data/GHA/S4AIX2.dta") %>%
  select(-id2, -id2, -hhno, s4aix2_plotno)
lab_val2[is.na(lab_val2)] <- 0
lab_val2 <- rowSums(lab_val2)
lab2 <- read_dta("data/GHA/S4AIX2.dta") %>%
  select(hhno, plotno=s4aix2_plotno) %>% cbind(lab_val2)

lab_val3 <- read_dta("data/GHA/S4AIX3.dta") %>%
  select(-id3, -id2, -hhno, s4aix3_plotno)
lab_val3[is.na(lab_val3)] <- 0
lab_val3 <- rowSums(lab_val3)
lab3 <- read_dta("data/GHA/S4AIX3.dta") %>%
  select(hhno, plotno=s4aix3_plotno) %>% cbind(lab_val3)

lab_val4 <- read_dta("data/GHA/S4AIX4.dta") %>%
  select(-id4, -id2, -hhno, s4aix4_plotno)
lab_val4[is.na(lab_val4)] <- 0
lab_val4 <- rowSums(lab_val4)
lab4 <- read_dta("data/GHA/S4AIX4.dta") %>%
  select(hhno, plotno=s4aix4_plotno) %>% cbind(lab_val4)

#######################################
########### CROSS SECTION #############
#######################################
chem_maj_tot <- select(chem_maj_tot, -inorg, -qty, -n, -p ) %>%
        filter(crop %in% "Maize")
names(oput_maj_mze)[2] <- "plotno"        

CS <- left_join(oput_maj_mze, chem_maj_tot)

CS$hhno <- as.character(CS$hhno)
area$hhno <- as.character(area$hhno)
implmt$hhno <- as.character(implmt$hhno)
lvstk$hhno <- as.character(lvstk$hhno)

CS <- left_join(CS, area)
CS <- left_join(CS, implmt)
CS <- left_join(CS, lvstk)

# make some NAs into not NAs
CS$pest <- as.integer(ifelse(is.na(CS$pest), 0, CS$pest) )
CS$org <- as.integer(ifelse(is.na(CS$org), 0, CS$org) )

CS$implmt_valu <- as.integer(ifelse(is.na(CS$implmt_valu), 0, CS$implmt_valu) )
CS$lvstk_valu <- as.integer(ifelse(is.na(CS$lvstk_valu), 0, CS$lvstk_valu) )

CS$asset <- CS$implmt_valu + CS$lvstk_valu 

CStst <- mutate(CS,
             yld=qty/area_ha,
             N=N/area_ha,
             P=P/area_ha,
             asset2=asset^2,
             area2=area_ha^2,
             N2= N^2
)

CStst2 <- select(CStst, hhno, yld, N, N2, P, area_ha, area2, org, legume, crop_count,
             pest, asset, asset2)

# try a cheekey regression
summary(lm(yld ~ N + N2 + N:P + area_ha + area2 + org  + asset +
                   asset2 + legume + crop_count  + pest ,
           data=CStst2))

