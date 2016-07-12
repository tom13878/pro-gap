# -------------------------------------
# creating a panel key for balancing
# the data. This is difficult because
# households were not surveyed in every
# year and in some cases households have
# split or merged together.
# -------------------------------------

library(haven)
library(stringr)
library(dplyr)

dataPath <- "C:/Users/Tomas/Documents/LEI/data/TZA"

# -------------------------------------
# The data for wave 3 comes with a 
# panel key file called NPSY3.PANEL.KEY.dta
# in which every household and individual
# is matched across all three years.
# -------------------------------------

# read in panel key to get allhousehold
# and individual ids

keyAll <- read_dta(file.path(dataPath, "2012/Data/NPSY3.PANEL.KEY.dta")) %>%
  rename(hhid2008=y1_hhid, hhid2010=y2_hhid, hhid2012=y3_hhid)

# change empty characters to NA
# 2008 and 2010 required care because
# of numeric/character strings and 
# leading zeros

bad <- is.na(as.numeric(keyAll$hhid2008))
keyAll$hhid2008[bad] <- NA

bad <- is.na(as.numeric(keyAll$hhid2010))
keyAll$hhid2010[bad] <- NA

keyAll$hhid2012 <- zap_empty(keyAll$hhid2012)

# -------------------------------------
# we are only interested in households
# that have entries for all three waves
# of the data. Hence we can remove any
# rows that have missing data
# -------------------------------------

keyHH <- unique(select(keyAll, hhid2008, hhid2010, hhid2012))
keyHH <- na.omit(keyHH)

# -------------------------------------
# However therse is still a problem, as
# some households in 2008 have more than
# one id in 2010 because some households
# This situation also occurs in the later
# years and in some cases households hav
# merged together.

# In the household section A of the 2010
# and 2012 survey, it is recorded whether
# the household is original and in the
# same location. Using this information we
# can select only those households that
# have one observation and have not split
# or merged across any of the waves.
# -------------------------------------

# 2010 key
key2010 <- read_dta(file.path(dataPath, "2010/Data/TZNPS2HH1DTA/HH_SEC_A.dta")) %>%
  select(hhid2010=y2_hhid, hhtype2010 = hh_a11) %>%
  mutate(hhtype2010 = factor(hhtype2010, levels = c(1 ,2, 3),
                         labels = c("ORIGINAL-SAME-LOC", "ORIGINAL-DIFF-LOC", "SPLIT-OFF")))

# 2012 key (slightly more work due to new questions in wave 3)
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

# -------------------------------------
# finally we can join all the data 
# together and select only those 
# households who are both original and
# in the same location through all three
# years of the data.
# -------------------------------------

KEY <- left_join(keyHH, key2012) %>% left_join(key2010)
KEY <- filter(KEY, hhtype2010 %in% "ORIGINAL-SAME-LOC", hhtype2012 %in% "ORIGINAL-SAME-LOC")

# -------------------------------------
# however, there are still a very small
# number of households that still show 
# a sifferent household number, even
# though they are both orginal and in the
# same location.
# -------------------------------------

bad2008 <- group_by(KEY, hhid2008) %>% summarise(n=n()) %>% filter(n > 1)
bad2010 <- group_by(KEY, hhid2010) %>% summarise(n=n()) %>% filter(n > 1)
bad2012 <- group_by(KEY, hhid2012) %>% summarise(n=n()) %>% filter(n > 1)

# KEY[KEY$hhid2008 %in% bad2008$hhid2008,]
# KEY[KEY$hhid2010 %in% bad2010$hhid2010,]
# KEY[KEY$hhid2012 %in% bad2012$hhid2012,]

# -------------------------------------
# Due to the lack of a good explanation
# why these households are not available
# they are removed from the key
# -------------------------------------

bad <- KEY$hhid2008 %in% bad2008$hhid2008 | KEY$hhid2010 %in% bad2010$hhid2010 | KEY$hhid2012 %in% bad2012$hhid2012
KEY <- KEY[!bad,]

rm("bad", "bad2008", "bad2010", "bad2012", "dataPath", "diff_loc",
   "key2010", "key2012", "keyAll", "keyHH")

