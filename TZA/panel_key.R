setwd("c:/users/tomas/documents/lei/data/TZA/TZA_2012_LSMS_v01_M_STATA_English_labels")

key1 <- read_dta("NPSY3.PANEL.KEY.dta") %>%
  rename(hhid2012=y3_hhid, hhid2010=y2_hhid)
key2 <- read_dta("HH_SEC_A.dta") %>%
  dplyr::select(hhid2012=y3_hhid, hhtype = hh_a10, hhloc = hh_a11) %>%
  mutate(hhtype = factor(hhtype, levels = c(1 ,2), labels = c("Original", "Split-off")),
         hhloc = factor(hhloc, levels = c(1,2,3), labels = c("In same location", "Local tracking", "Distance tracking")))

# Create key
key1$hhid2010 <- zap_empty(key1$hhid2010)
key1$hhid2012 <- zap_empty(key1$hhid2012)

key <- dplyr::select(key1, hhid2010, hhid2012) %>%
  na.omit() %>%
  unique() %>%
  left_join(., key2) %>%
  arrange(hhid2012)

# Remove hh that have merged between y2 and y3
dupl2012 <- dplyr::select(key, hhid2012) %>% do(filter(., duplicated(.)))
key <- key[!(key$hhid2012 %in% dupl2012$hhid2012),]
saveRDS(key, "c:/users/tomas/documents/panel_key.rds")
