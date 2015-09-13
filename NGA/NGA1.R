#######################################
############ NIGERIA 2010 #############
#######################################


setwd("c:/USers/tomas/Documents/work/LEI/data/NGA/NGA_2010_GHSP_v02_M_STATA")

library(haven)
library(stringr)
library(plyr)
library(dplyr)

options(scipen=999)

# Section A1 has info on the dry seaons intercropping Q 29
# Section 11b in the post planting has a question on irrigation Q24
# Section 11c PP has info on input costs



#######################################
############### OUTPUT ################
#######################################

# easiest to filter on maizeas early as possible

oput <- read_dta("Post Harvest Wave 1/Agriculture/secta3_harvestw1.dta") %>%
    dplyr::select(hhid, plotid, cropid, crop=sa3q1, qty=sa3q6a, qty_unit=sa3q6b,
           main_buyer=sa3q10, qty_sold_buyer=sa3q11a,
                  qty_sold_buyer_unit=sa3q11b, qty_sold_naira=sa3q12)

oput$qty_unit <- as.integer(oput$qty_unit)
oput$qty_sold_buyer_unit <- as.integer(oput$qty_sold_buyer_unit)

# also what does fallow fallow and fallow fallow fallow mean
# in the crop variable????

# this will take a long time if we want to make an output index!
bad_maize <- c("MAIZE.", "MAAIZE", "MAIZE FARM", "M AIZE", "MAZIE", "maize")
oput$crop <- ifelse(oput$crop %in% bad_maize, "MAIZE", oput$crop)

# could be some other legumes but don't recognize the names
# first make a crop count variable and a legumes variable

legumes <- c("PIGEON PEA", "SOYA BEANS", "LOCUST BEAN")
oput <- ddply(oput, .(hhid, plotid), transform,
              crop_count=length(crop[!is.na(crop)]),
              legume=ifelse(any(crop %in% legumes), 1, 0))

# now select on maize
oput_maze <- oput[oput$crop %in% "MAIZE" & ! is.na(oput$qty) & !oput$qty %in% 0,]


# need to sort units. Kilogram=1, gram=2, Litre=3. Other units are
# offered but survey does not mention them. Basic unit is Kilorgrams
# change everything into kilograms


# units are not included in the data for a lot of values but
# in the documentattion there is a supplementary table
# sadly this means adding unit codes by hand.
# labels that do exits in the data go missing half the time!

unit_code <- c(1, 2, 3, 11, 12, 13, 14, 21, 22, 23, 24, 31,
               32, 33, 34, 41, 42, 43, 51, 52, 53, 61,
               62, 63, 71, 72, 73, 74, 81, 82, 83,
               91, 92, 93, 94, 95)
weight <- c(1, 0.001, 1, 20, 50, 100, 120, 15, 30, 50, 75, 10, 25, 40, 75,
            5, 8, 15, 3, 5, 8, 15, 25, 40, 60, 85, 110, 150,
            1500, 2000, 2500, 10, 20, 25, 50, 200)

cnvrt <- data.frame(unit_code, weight)
oput_maze$qty_unit <- as.integer(oput_maze$qty_unit)
oput_maze2 <- left_join(oput_maze, cnvrt, by=c("qty_unit"="unit_code"))
oput_maze2 <- left_join(oput_maze, cnvrt, by=c("qty_sold_buyer_unit"="unit_code"))
oput_maze2 <- dplyr::mutate(oput_maze2, qty_kg = qty*weight)

# pretty limited information on the prices paid by buyers
sum(table(oput_maze$qty_sold_buyer_unit))

oput_maze <- dplyr::select(oput_maze, hhid, plotid, crop, qty, maize_price, crop_count, legumes)

#######################################
############## CHEMICAL ###############
#######################################

plot <- read_dta("Post Planting Wave 1/Agriculture/sect11c_plantingw1.dta") %>%
  select(hhid, plotid, pest=s11cq1, herb=s11cq10)

# COMMERCIAL FERTILIZER
fert1 <- read_dta("Post Planting Wave 1/Agriculture/sect11d_plantingw1.dta") %>%
  select(hhid, plotid, typ=s11dq14, qty=s11dq15, valu=s11dq18)
fert2 <- read_dta("Post Planting Wave 1/Agriculture/sect11d_plantingw1.dta") %>%
    select(hhid, plotid, typ=s11dq25, qty=s11dq26, valu=s11dq29)

# FREE OR LEFT OVER FERTILIZER
freeFert <-  read_dta("Post Planting Wave 1/Agriculture/sect11d_plantingw1.dta") %>%
    select(hhid, plotid, typ=s11dq7, qty=s11dq8)
leftOverFert <- read_dta("Post Planting Wave 1/Agriculture/sect11d_plantingw1.dta") %>%
    select(hhid, plotid, typ=s11dq3, qty=s11dq4)

# make factor variables into characters for easier joining
fert1$typ <- as.character(as_factor(fert1$typ))
fert2$typ <- as.character(as_factor(fert2$typ))
freeFert$typ <- as.character(as_factor(freeFert$typ))
leftOverFert$typ <- as.character(as_factor(leftOverFert$typ))

# for now set composite manure and other values to NA
bad <- c("composite manure", "other (specify)")
fert1$typ <- ifelse(fert1$typ %in% bad, NA, fert1$typ)
fert2$typ <- ifelse(fert2$typ %in% bad, NA, fert2$typ)
freeFert$typ <- ifelse(freeFert$typ %in% bad, NA, freeFert$typ)
leftOverFert$typ <- ifelse(leftOverFert$typ %in% bad, NA, leftOverFert$typ)

# provide a nitrogen component value for npk and urea (from Michiel's file)
typ <- c("npk", "urea")
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



#######################################
############### LABOUR ################
#######################################




#######################################
################ SEEDS ################
#######################################

# section 11e of post planting
seed <- read_dta("Post Planting Wave 1/Agriculture/sect11e_plantingw1.dta") %>%
  select(hhid, plotid, )

#######################################
############### Assets ################
#######################################

# section A4 has info on household assets

implmt <- read_dta("") %>%


  select(y2_hhid, itemcode, qty=ag11_01, valu=ag11_02) %>%
  filter(!qty %in% 0, !is.na(qty), !valu %in% 0, !is.na(valu)) %>%
  transmute(y2_hhid, valu=qty*valu) %>%
  group_by(y2_hhid) %>%
  summarise(value=sum(valu))

# section A6 contains info on the animal holdings.
# including value of animal if sold

# in the post planting questionnaire there is also
# information on animal holdings section 11i
