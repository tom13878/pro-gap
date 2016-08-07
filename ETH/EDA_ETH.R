# -------------------------------------
# exploratory analysis file for the
# 2011 wave of EThiopia data
# -------------------------------------

# filepath
if(Sys.info()["user"] == "Tomas"){
  filePath <- "C:/Users/Tomas/Documents/LEI/"
} else {
  filePath <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/ETH/2011/Data"
}

# packages
library(reshape2)
library(dplyr)

# source in the data
source(file.path(filePath, "pro-gap/ETH/ETH_2013PP.R"))

# first make some counts of each household
# and type of household represented in the data
# this can be checked against the codebook
count1 <- group_by(ETH2013, REGNAME, ZONENAME, type) %>% summarise(n=n()) %>%
  dcast(REGNAME + ZONENAME ~ type, fill=0)
names(count1)[3:4] <- paste(names(count1)[3:4], "HH", sep=" ")
count2 <- select(ETH2013, -household_id, -household_id) %>% unique %>%
  group_by(REGNAME, ZONENAME, type) %>% summarise(n=n()) %>%
  dcast(REGNAME + ZONENAME ~ type, fill=0)
names(count2)[3:4] <- paste(names(count2)[3:4], "EA", sep=" ")
count <- left_join(count1, count2); rm(count1, count2)
count <- count[c(1, 2, 5, 3, 6, 4)]

# focus on maize producing farmers
maize <- filter(ETH2013, status=="HEAD", crop_code==2); rm(ETH2013)

# regions and main harvest month
table(maize$REGNAME, maize$harv_month1)

# maize produced per region
ggplot( data=maize, aes( x=REGNAME, y=crop_qty_harv ) ) + geom_boxplot( ) +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  ylab("maize harvested per region (kg)") + xlab(" ")

ggplot( data=maize, aes( x=REGNAME, y=log(crop_qty_harv) ) ) + geom_boxplot( ) +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  ylab("maize harvested per region (kg)") + xlab(" ")

# and a histogram
par(mfrow=c(1, 2))
with(maize, {
  hist(crop_qty_harv, breaks="FD", freq=FALSE, ylab="Density")
  lines(density(crop_qty_harv), lwd=2)
  lines(density(crop_qty_harv, adjust=0.5), lwd=1)
  rug(crop_qty_harv)
  box()
})
with(maize, {
  hist(log(crop_qty_harv), breaks="FD", freq=FALSE, ylab="Density")
  lines(density(log(crop_qty_harv)), lwd=2)
  lines(density(log(crop_qty_harv), adjust=0.5), lwd=1)
  rug(log(crop_qty_harv))
  box()
})

# have a similar look at areas, using imputed values
par(mfrow=c(1, 2))
with(maize[!is.na(maize$area_gps_mi50),], {
  hist(area_gps_mi50, breaks="FD", freq=FALSE, ylab="Density")
  lines(density(area_gps_mi50), lwd=2)
  lines(density(area_gps_mi50, adjust=0.5), lwd=1)
  rug(area_gps_mi50)
  box()
})
with(maize[!is.na(maize$area_gps_mi50),], {
  hist(log(area_gps_mi50), breaks="FD", freq=FALSE, ylab="Density")
  lines(density(log(area_gps_mi50)), lwd=2)
  lines(density(log(area_gps_mi50), adjust=0.5), lwd=1)
  rug(log(area_gps_mi50))
  box()
})

# crucial variabe is yield. 
yld <- maize$crop_qty_harv/maize$area_gps_mi50
yld <- yld[!is.na(yld)]

# level -> likely outlying values
hist(yld, breaks="FD", freq=FALSE, ylab="Density")
lines(density(yld), lwd=2)
lines(density(yld, adjust=0.5), lwd=1)
rug(yld)
box()

# log
hist(log(yld), breaks="FD", freq=FALSE, ylab="Density")
lines(density(log(yld)), lwd=2)
lines(density(log(yld), adjust=0.5), lwd=1)
rug(log(yld))
box()

# alternativley we can use the farmers self reported
# harvested area to scale the 
yld2 <- maize$crop_qty_harv/maize$area_gps_mi50*maize$harv_area/100
yld2 <- yld2[!is.na(yld2)]

# level -> likely outlying values
hist(yld2, breaks="FD", freq=FALSE, ylab="Density")
lines(density(yld2), lwd=2)
lines(density(yld2, adjust=0.5), lwd=1)
rug(yld2)
box()

# log
hist(log(yld2), breaks="FD", freq=FALSE, ylab="Density")
lines(density(log(yld2)), lwd=2)
lines(density(log(yld2), adjust=0.5), lwd=1)
rug(log(yld2))
box()

# second important variable is nitrogen
par(mfrow=c(1, 2))
with(maize[!maize$N==0,], {
  hist(N, breaks="FD", freq=FALSE, ylab="Density")
  lines(density(N), lwd=2)
  lines(density(N, adjust=0.5), lwd=1)
  rug(N)
  box()
})
with(maize[!maize$N==0,], {
  hist(log(N), breaks="FD", freq=FALSE, ylab="Density")
  lines(density(log(N)), lwd=2)
  lines(density(log(N), adjust=0.5), lwd=1)
  rug(log(N))
  box()
})


# -------------------------------------
# mutivariate analysis
# -------------------------------------

# focus on yield and nitrogen
