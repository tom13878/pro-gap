# -------------------------------------
# area imputation file for ETHiopia 2013
# missing gps measurements

library(haven)
library(dplyr)

dataPath <- "C:/Users/Tomas/Documents/LEI/data/ETH"

# -------------------------------------
# plot characteristics

plots <- read_dta(file.path(dataPath, "Post-Planting/sect3_pp_w2.dta")) %>%
  dplyr::select(holder_id, household_id, parcel_id, field_id, status = pp_s3q03,
                irrigated = pp_s3q12, urea = pp_s3q15, dap = pp_s3q18,
                other_inorg = pp_s3q20a, manure = pp_s3q21, compost = pp_s3q23,
                other_org = pp_s3q25) %>%
  filter(!(household_id %in% ""))

plots$rented <- ifelse(plots$status %in% 6, 1, 0) # 6 for rentedout, household use otherwise
plots$fallowed <- ifelse(plots$status %in% 3, 1, 0) # 3 if plot left fallow
plots$irrigated <- ifelse(plots$irrigated %in% 1, 1, 0)

# fertilizer - inorganic

plots$dap <- ifelse(plots$dap %in% 1, 1, 0)
plots$urea <- ifelse(plots$urea %in% 1, 1, 0)
plots$other_inorg <- ifelse(plots$other_inorg %in% 1, 1, 0)
plots$inorgFert <- with(plots, ifelse(dap == 1 | urea == 1 | other_inorg ==1, 1, 0))

# fertilizer - organic

plots$manure <- ifelse(plots$manure %in% 1, 1, 0)
plots$compost <- ifelse(plots$compost %in% 1, 1, 0)
plots$other_org <- ifelse(plots$other_org %in% 1, 1, 0)
plots$orgFert <- with(plots, ifelse(manure == 1 | compost == 1 | other_org ==1, 1, 0))

plots <- select(plots, holder_id, household_id, parcel_id, field_id, irrigated,
                rented, fallowed, inorgFert, orgFert)

# count the number of plots owned by a household

plotCount <- group_by(plots, household_id) %>% summarise(n=n())

# join with other plot information

plots <- left_join(plots, plotCount)

# soil quality is a parcel level variable

parcel <- read_dta(file.path(dataPath, "Post-Planting/sect2_pp_w2.dta")) %>%
  dplyr::select(holder_id, household_id, parcel_id, soil_qlty=pp_s2q15) %>%
  filter(!(household_id %in% "")) %>%
  unique()

parcel$soil_qlty <- as_factor(parcel$soil_qlty)
parcel$soil_qlty <- relevel(parcel$soil_qlty, ref = "Poor")

plots <- left_join(plots, parcel)

# Hired labour

pp_lab <- read_dta(file.path(dataPath, "Post-Planting/sect3_pp_w2.dta")) %>%
  dplyr::select(holder_id, household_id, parcel_id, field_id, pp_s3q28_a:pp_s3q28_h) %>%
  filter(!(household_id %in% "")) %>%
  transmute(holder_id, household_id, parcel_id, field_id,
            hirM=pp_s3q28_a*pp_s3q28_b,
            hirF=pp_s3q28_d*pp_s3q28_e,
            hirC=pp_s3q28_g*pp_s3q28_h
  )

# make all NA values zero
pp_lab[is.na(pp_lab)] <- 0

# sum all labour across a single plot - all measured in days
pp_lab <- transmute(pp_lab, holder_id, household_id, parcel_id, field_id,
                  hir_lab=hirM + hirF + hirC)

# post harvest hired labour - measured at the crop level

ph_lab <- read_dta(file.path(dataPath, "Post-Harvest/sect10_ph_w2.dta")) %>%
  dplyr::select(holder_id, household_id, parcel_id, field_id, crop=crop_code, ph_s10q01_b:ph_s10q01_h) %>%
  filter(!(household_id %in% "")) %>%
  transmute(holder_id, household_id, parcel_id, field_id, crop,
            hirM=ph_s10q01_b,
            hirF=ph_s10q01_e,
            hirC=ph_s10q01_h
  )

ph_lab[is.na(ph_lab)] <- 0

# sum all labour across a single crop - all measured in days
ph_lab <- transmute(ph_lab, holder_id, household_id, parcel_id, field_id,
                  ph_hir_lab = hirM + hirF + hirC)

# group by crops to find total labour used on
# a field

ph_lab <- group_by(ph_lab, holder_id, household_id, parcel_id, field_id) %>%
  summarise(hir_lab = sum(ph_hir_lab))

# bind and sumarise the data
hir_lab <- rbind(pp_lab, ph_lab) %>%
  group_by(holder_id, household_id, parcel_id, field_id) %>%
  summarise(hiredLabour = sum(hir_lab))

# join the labour with the rest of the plot
# information

plots <- left_join(plots, hir_lab)

# -------------------------------------
# household characteristics

# rural
rural <- read_dta(file.path(dataPath, "Household/sect1_hh_w2.dta")) %>%
  dplyr::select(household_id, rural) %>% unique

rural$rural <- ifelse(rural$rural %in% 1, 1, 0)

# count number of people of each age group in
# the husehold

ag <- read_dta(file.path(dataPath, "Household/sect1_hh_w2.dta")) %>%
  dplyr::select(household_id, individual_id, age=hh_s1q04_a, sex=hh_s1q03) %>%
  filter(!household_id %in% "") %>% 
  mutate(age_group = cut(age, c(0, 5, 14, 39, 59, 100),
                         include.lowest = TRUE, right = FALSE))
  
ag$sex <- ifelse(ag$sex %in% 2, 1, 0)

# set ages without sex
ag$less5 <- ifelse(ag$age_group == '[0,5)', 1, NA)
ag$g5l15 <- ifelse(ag$age_group == '[5,14)', 1, NA)
ag$above60 <- ifelse(ag$age_group == '[59,100]', 1, NA)

# ages with sex
ag$male15_39 <- ifelse(ag$age_group=='[14,39)' & ag$sex==0, 1, NA)
ag$female15_39 <- ifelse(ag$age_group=='[14,39)' & ag$sex==1, 1, NA)
ag$male40_59 <- ifelse(ag$age_group=='[39,59)' & ag$sex==0, 1, NA)
ag$female40_59 <- ifelse(ag$age_group=='[39,59)' & ag$sex==1, 1, NA)

# summarise to get variables per household.
by_hhid <- group_by(ag, household_id) %>% summarise(
  less5 = sum(less5, na.rm = TRUE),
  above60 = sum(above60, na.rm = TRUE),
  male15_39 = sum(male15_39, na.rm = TRUE),
  female15_39 = sum(female15_39, na.rm = TRUE),
  male40_59 = sum(male40_59, na.rm = TRUE),
  female40_59 = sum(female40_59, na.rm = TRUE),
  g5l15 = sum(g5l15, na.rm = TRUE))

# join the age group information with the
# age and sex of the plot manager (assumed
# to be the household head)

pmc <- read_dta(file.path(dataPath, "Household/sect1_hh_w2.dta")) %>%
  filter(hh_s1q02 %in% 1) %>% # 1 for head of household
  dplyr::select(household_id, sex=hh_s1q03, age=hh_s1q04_a) %>%
  filter(!(household_id %in% ""))

pmc$sex <- ifelse(pmc$sex %in% 2, 1, 0)

hc <- left_join(pmc, by_hhid)


# ------------------------------------
# interaction variables (sr-area w/ plots
# and manager characteristics)

# get the areas information
areas <- read_dta(file.path(dataPath, "Post-Planting/sect3_pp_w2.dta")) %>%
  dplyr::select(holder_id, household_id, parcel_id, field_id, area_sr=pp_s3q02_a,
                area_sr_unit=pp_s3q02_c, area_gps=pp_s3q05_a) %>%
  filter(!(household_id %in% ""))

# sample weight

weight <- read_dta(file.path(dataPath, "Household/sect1_hh_w2.dta")) %>%
  dplyr::select(household_id, weight=pw2) %>% unique

# could add education level as an ordered factor somewhere