# -------------------------------------
# creating a panel key for balancing
# the data. 
# -------------------------------------

library(haven)
library(stringr)
library(dplyr)

dataPath <- "C:/Users/Tomas/Documents/LEI/data/TZA"

# using the panel key we could join households up
# across years

# household and individual ids
keyAll <- read_dta(file.path(dataPath, "2012/Data/NPSY3.PANEL.KEY.dta")) %>%
  rename(hhid2008=y1_hhid, hhid2010=y2_hhid, hhid2012=y3_hhid)
table(str_length(keyAll$hhid2010))

# change empty characters to NA
bad <- is.na(as.numeric(keyAll$hhid2008))
keyAll$hhid2008[bad] <- NA

bad <- is.na(as.numeric(keyAll$hhid2010))
keyAll$hhid2010[bad] <- NA

keyAll$hhid2012 <- zap_empty(keyAll$hhid2012)

table(str_length(keyAll$hhid2010))

------------------------------------------------------------------

# only household ids that have an entry for all three years
keyHH <- unique(select(keyAll, hhid2008, hhid2010, hhid2012))
keyHH <- na.omit(keyHH)

# however therse is still a problem, as some
# households in 2008 have more than one id in
# 2010 because some households have split.
x <- group_by(keyHH, hhid2008) %>% summarise(N=n())
table(x$N)

# in wave 2 households may be 
key2010 <- read_dta(file.path(dataPath, "2010/Data/TZNPS2HH1DTA/HH_SEC_A.dta")) %>%
  select(hhid2010=y2_hhid, hhtype2010 = hh_a11) %>%
  mutate(hhtype2010 = factor(hhtype2010, levels = c(1 ,2, 3),
                         labels = c("ORIGINAL-SAME-LOC", "ORIGINAL-DIFF-LOC", "SPLIT-OFF")))
table(str_length(keyAll$hhid2010))
table(str_length(key2010$hhid2010))
table(key2010$hhid2010 %in% keyHH$hhid2010)
table(keyHH$hhid2010 %in% key2010$hhid2010)

# by wave three there is a difference between those 
# observations that are the original household those 
# that are distance tracked and those that are tracked 
# locally

key2012 <- read_dta(file.path(dataPath, "2012/Data/HH_SEC_A.dta")) %>%
  dplyr::select(hhid2012=y3_hhid, hhtype2012 = hh_a10, hhloc = hh_a11)
key2012$hhtype2012 <- as.character(as_factor(key2012$hhtype2012))
key2012$hhloc <- as.character(as_factor(key2012$hhloc))

diff_loc <- c("LOCAL TRACKING", "DISTANCE TRACKING")
key2012$hhtype2012 <- ifelse(key2012$hhtype2012 %in% "SPLIT-OFF HOUSEHOLD", "SPLIT-OFF", key2012$hhtype2012)
key2012$hhtype2012 <- ifelse(key2012$hhtype2012 %in% "ORIGINAL HOUSEHOLD" & key2012$hhloc %in% diff_loc,
                      "ORIGINAL-DIFF-LOC", key2012$hhtype2012)
key2012$hhtype2012 <- ifelse(key2012$hhtype2012 %in% "ORIGINAL HOUSEHOLD" & key2012$hhloc %in% "IN SAME LOCATION",
                      "ORIGINAL-SAME-LOC", key2012$hhtype2012)
key2012$hhloc <- NULL


x <- left_join(keyHH, key2012) %>% left_join(key2010)
x <- x[x$hhtype2010 %in% "ORIGINAL-SAME-LOC" & x$hhtype2012 %in% "ORIGINAL-SAME-LOC",]
