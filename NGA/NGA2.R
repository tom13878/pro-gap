# Nigeria Wave 2 data


setwd("c:/USers/tomas/Documents/work/LEI/data/NGA/NGA_2012_LSMS_v03_M_STATA")

library(haven)
library(stringr)
library(plyr)
library(dplyr)

options(scipen=999)





#######################################
############### OUTPUT ################
#######################################

# easiest to filter on maize as early as possible

oput <- read_dta("Post Harvest Wave 2/Agriculture/secta3_harvestw2.dta") %>%
    dplyr::select(hhid, plotid, cropid, crop=cropname, qty=sa3q6a1, qty_unit=sa3q6a2,
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
oput_maze <- left_join(oput_maze, cnvrt, by=c("qty_unit"="unit_code"))
oput_maze <- left_join(oput_maze, cnvrt, by=c("qty_sold_buyer_unit"="unit_code"))
oput_maze <- dplyr::mutate(oput_maze, qty_kg = qty*weight)

# pretty limited information on the prices paid by buyers
sum(table(oput_maze$qty_sold_buyer_unit))

# so basically done but need maize price!

oput_maze <- dplyr::select(oput_maze, hhid, plotid, crop, qty, maize_price, crop_count, legumes)



#######################################
############## CHEMICAL ###############
#######################################

plot <- read_dta("Post Planting Wave 2/Agriculture/sect11c2_plantingw2.dta") %>%
  dplyr::select(hhid, plotid, pest=s11c2q1, herb=s11c2q10)

plot$pest <- ifelse(plot$pest %in% 1, 1, 0)
plot$herb <- ifelse(plot$herb %in% 1, 1, 0)

plot <- dplyr::mutate(plot, hhid, plotid,
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
plot <- left_join(plot, fert)
