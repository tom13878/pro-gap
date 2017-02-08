# -------------------------------------
# on and off farm income for Tanzania
# wave 2 (2010)
# output is a file containing income for
# each household from
# 1. off farm jobs and self employment
# 2. crop production (including fruit
# trees and permanent crops) in both
# the long rainy (lr) and short rainy (sr)
# seasons.
# 3. rented land during the lr and sr
# seasons
# 4. livestock sales in the last 12 months
# -------------------------------------

library(dplyr)
library(sjmisc)
library(haven)

if(Sys.info()["user"] == "Tomas"){
  dataPath <- "C:/Users/Tomas/Documents/LEI/data/TZA/2010/Data"
} else {
  dataPath <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/TZA/2010/Data"
}

#######################################
########## off farm income ############
#######################################

# -------------------------------------
# off-farm income (last 12 months)
# unfortunately we do not see how many
# days are worked per week!!!
# -------------------------------------

off_farm_income <- read_dta(file.path(dataPath, "TZNPS2HH1DTA/HH_SEC_E1.dta")) %>%
  select(y2_hhid, indidy2,
         main_job=hh_e12,
         mj_industry=hh_e17_2, mj_employer=hh_e15,
         mj_months=hh_e26, mj_weekspm=hh_e27,
         mj_hourspw=hh_e28,mj_wage=hh_e22_1,
         mj_pay_period=hh_e22_2, mj_grat=hh_e23,
         mj_grat_wage=hh_e24_1,
         mj_grat_period=hh_e24_2,second_job=hh_e29,
         sj_industry=hh_e31_2, sj_employer=hh_e30,
         sj_months=hh_e41, sj_weekspm=hh_e42,
         sj_hourspw=hh_e43, sj_wage=hh_e37_1,
         sj_pay_period=hh_e37_2, sj_grat=hh_e38,
         sj_grat_wage=hh_e39_1,
         sj_grat_period=hh_e39_2,
         self_employed_week=hh_e51, self_employed_year=hh_e52,
         se_industry=hh_e53_2, se_owner1=hh_e54_1, se_owner2=hh_e54_2,
         se_co_owners=hh_e55, se_business_id=hh_e56,
         se_years=hh_e58_1, se_months=hh_e58_2,
         se_capital1=hh_e59_1,
         se_capital2=hh_e59_2,
         se_capital3=hh_e59_3,
         se_consumer1=hh_e60_1,
         se_consumer2=hh_e60_2,
         se_capital_value=hh_e61,
         se_inputs_value=hh_e62,
         se_stock_value=hh_e63,
         se_income=hh_e64_2,
         se_income_period=hh_e64_1,
         se_profit=hh_e65_2,
         se_profit_period=hh_e65_1,
         se_employees=hh_e66,
         se_wages_last_month=hh_e67,
         se_input_cost_last_month=hh_e68,
         se_other_costs=hh_e69,
         se_months_py=hh_e70,
         se_avg_income_pm=hh_e71,
         other_se_week=hh_e72,
         other_se_year=hh_e73)

off_farm_income$mj_pay <- NA
off_farm_income$mj_grat_pay <- NA
off_farm_income$sj_pay <- NA
off_farm_income$sj_grat_pay <- NA

off_farm_income <- transmute(off_farm_income, y2_hhid, indidy2,
                            main_job, mj_industry, mj_employer,
                             mj_pay=ifelse(mj_pay_period %in% 1, mj_months*mj_weekspm*mj_hourspw*mj_wage, mj_pay),
                             mj_pay=ifelse(mj_pay_period %in% 3, mj_months*mj_weekspm*mj_wage, mj_pay),
                             mj_pay=ifelse(mj_pay_period %in% 4, mj_months*mj_weekspm*mj_wage/2, mj_pay),
                             mj_pay=ifelse(mj_pay_period %in% 5, mj_months*mj_wage, mj_pay),
                             mj_pay=ifelse(mj_pay_period %in% 6, mj_months*mj_wage/3, mj_pay),
                             mj_pay=ifelse(mj_pay_period %in% 7, mj_months*mj_wage/6, mj_pay),
                             mj_pay=ifelse(mj_pay_period %in% 8, mj_months*mj_wage/12, mj_pay),
                             
                             mj_grat_pay=ifelse(mj_grat_period %in% 1, mj_months*mj_weekspm*mj_hourspw*mj_grat, mj_grat_pay),
                             mj_grat_pay=ifelse(mj_grat_period %in% 3, mj_months*mj_weekspm*mj_grat, mj_grat_pay),
                             mj_grat_pay=ifelse(mj_grat_period %in% 4, mj_months*mj_weekspm*mj_grat/2, mj_grat_pay),
                             mj_grat_pay=ifelse(mj_grat_period %in% 5, mj_months*mj_grat, mj_grat_pay),
                             mj_grat_pay=ifelse(mj_grat_period %in% 6, mj_months*mj_grat/3, mj_grat_pay),
                             mj_grat_pay=ifelse(mj_grat_period %in% 7, mj_months*mj_grat/6, mj_grat_pay),
                             mj_grat_pay=ifelse(mj_grat_period %in% 8, mj_months*mj_grat/12, mj_grat_pay),
                             
                             second_job, sj_industry, sj_employer,
                             sj_pay=ifelse(sj_pay_period %in% 1, sj_months*sj_weekspm*sj_hourspw*sj_wage, sj_pay),
                             sj_pay=ifelse(sj_pay_period %in% 3, sj_months*sj_weekspm*sj_wage, sj_pay),
                             sj_pay=ifelse(sj_pay_period %in% 4, sj_months*sj_weekspm*sj_wage/2, sj_pay),
                             sj_pay=ifelse(sj_pay_period %in% 5, sj_months*sj_wage, sj_pay),
                             sj_pay=ifelse(sj_pay_period %in% 6, sj_months*sj_wage/3, sj_pay),
                             sj_pay=ifelse(sj_pay_period %in% 7, sj_months*sj_wage/6, sj_pay),
                             sj_pay=ifelse(sj_pay_period %in% 8, sj_months*sj_wage/12, sj_pay),
                             
                             sj_grat_pay=ifelse(sj_grat_period %in% 1, sj_months*sj_weekspm*sj_hourspw*sj_grat, sj_grat_pay),
                             sj_grat_pay=ifelse(sj_grat_period %in% 3, sj_months*sj_weekspm*sj_grat, sj_grat_pay),
                             sj_grat_pay=ifelse(sj_grat_period %in% 4, sj_months*sj_weekspm*sj_grat/2, sj_grat_pay),
                             sj_grat_pay=ifelse(sj_grat_period %in% 5, sj_months*sj_grat, sj_grat_pay),
                             sj_grat_pay=ifelse(sj_grat_period %in% 6, sj_months*sj_grat/3, sj_grat_pay),
                             sj_grat_pay=ifelse(sj_grat_period %in% 7, sj_months*sj_grat/6, sj_grat_pay),
                             sj_grat_pay=ifelse(sj_grat_period %in% 8, sj_months*sj_grat/12, sj_grat_pay),
                            
                             se_industry, se_business_id,
                             se_capital_value, se_inputs_value, se_stock_value,
                             se_income_py=se_months_py*se_avg_income_pm)

# create variable for total pay received
# from all jobs. 0s are returned to NAs 
# where necessary

off_farm_income$off_farm_income <- with(off_farm_income,
                                        rowSums(cbind(mj_pay,mj_grat_pay,sj_pay, sj_grat_pay, se_income_py),
                                                na.rm=TRUE))
miss <- with(off_farm_income,
             is.na(mj_pay) & is.na(mj_grat_pay & is.na(sj_pay) & is.na(sj_grat_pay) & is.na(se_income_py)))
off_farm_income$off_farm_income[miss] <- NA; rm(miss)

# summarise at the household level to get
# a measure of total household off farm
# income

off_farm_income_hh <- group_by(off_farm_income, y2_hhid) %>%
  summarise(off_farm_income_hh=sum(off_farm_income, na.rm=TRUE))
rm(off_farm_income)

#######################################
########## on farm income #############
#######################################

# -------------------------------------
# on farm income from the sale of crops
# -------------------------------------

lr_crop <- read_dta(file.path(dataPath, "TZNPS2AGRDTA/AG_SEC5A.dta")) %>%
  select(y2_hhid, crop_code=zaocode, lr_crop_sold=ag5a_01,
         lr_crop_qty=ag5a_02, lr_crop_value=ag5a_03,
         lr_customer1=ag5a_04_1,
         lr_customer1_qty=ag5a_05,
         lr_customer1_value=ag5a_06,
         lr_customer1_month=ag5a_07_1,
         lr_customer1_year=ag5a_07_2,
         lr_customer2=ag5a_04_2,
         lr_customer2_qty=ag5a_10,
         lr_customer2_value=ag5a_11,
         lr_customer2_month=ag5a_12_1,
         lr_customer2_year=ag5a_12_2) 

sr_crop <- read_dta(file.path(dataPath, "TZNPS2AGRDTA/AG_SEC5B.dta")) %>%
  select(y2_hhid, crop_code=zaocode, sr_crop_sold=ag5b_01,
         sr_crop_qty=ag5b_02, sr_crop_value=ag5b_03,
         sr_customer1=ag5b_04_1,
         sr_customer1_qty=ag5b_05,
         sr_customer1_value=ag5b_06,
         sr_customer1_month=ag5b_07_1,
         sr_customer1_year=ag5b_07_2,
         sr_customer2=ag5b_04_2,
         sr_customer2_qty=ag5b_10,
         sr_customer2_value=ag5b_11,
         sr_customer2_month=ag5b_12_1,
         sr_customer2_year=ag5b_12_2) 

# treat seasons together as income for one
# whole year
names(lr_crop) <- gsub("lr_", "", names(lr_crop))
lr_crop$season <- "lr"
names(sr_crop) <- gsub("sr_", "", names(sr_crop))
sr_crop$season <- "sr"

crop <- rbind(sr_crop, lr_crop); rm(lr_crop, sr_crop)

# calculate the full value of crops
# per household

on_farm_income_crop <- group_by(crop, y2_hhid) %>%
  summarise(crop_value_hh=sum(crop_value, na.rm=TRUE))
rm(crop)

# -------------------------------------
# from permanent crops (fruit trees or 
# other perm crops crops)
# -------------------------------------

# fruit trees
fruit <- read_dta(file.path(dataPath, "TZNPS2AGRDTA/AG_SEC7A.dta")) %>%
  select(y2_hhid, crop_code=zaocode,
         sold_fruit=ag7a_02, sold_fruit_kg=ag7a_03, fruit_value=ag7a_04)

# summarise to the household level
on_farm_income_fruit <- group_by(fruit, y2_hhid) %>%
  summarise(fruit_value_hh=sum(fruit_value, na.rm=TRUE))
rm(fruit)

# permanent crops
perm <- read_dta(file.path(dataPath, "TZNPS2AGRDTA/AG_SEC7B.dta")) %>%
  select(y2_hhid, crop_code=zaocode,
         sold_perm=ag7b_02, sold_perm_kg=ag7b_03, perm_value=ag7b_04)

# summarise to the household level
on_farm_income_perm <- group_by(perm, y2_hhid) %>%
  summarise(perm_value_hh=sum(perm_value, na.rm=TRUE))
rm(perm)

# -------------------------------------
# income from renting out plots in 
# both seasons
# -------------------------------------

# from rented land - long rainy season
lr_rent <- read_dta(file.path(dataPath, "TZNPS2AGRDTA/AG_SEC3A.dta")) %>%
  select(y2_hhid, plotnum, lr_rent=ag3a_04)

# from rented land - short rainy season
sr_rent <- read_dta(file.path(dataPath, "TZNPS2AGRDTA/AG_SEC3B.dta")) %>%
  select(y2_hhid, plotnum, sr_rent=ag3b_04)

# treat seasons together as income for one
# whole year
names(lr_rent) <- gsub("lr_", "", names(lr_rent))
lr_rent$season <- "lr"
names(sr_rent) <- gsub("sr_", "", names(sr_rent))
sr_rent$season <- "sr"

rent <- rbind(sr_rent, lr_rent); rm(lr_rent, sr_rent)

# summarise to the household level
on_farm_income_rent <- group_by(rent, y2_hhid) %>%
  summarise(rent_value_hh=sum(rent, na.rm=TRUE))
rm(rent)

# -------------------------------------
# income from livestock sales
# -------------------------------------

lvstock <- read_dta(file.path(dataPath, "TZNPS2AGRDTA/AG_SEC10A.dta")) %>%
  select(y2_hhid, lvstkcode, lvstk_sold=ag10a_19,
         lvstk_number_sold=ag10a_20, lvstk_value=ag10a_21,
         slaughter=ag10a_24, slaughter_qty=ag10a_25,
         slaughter_qty_sold=ag10a_26, slaughter_value=ag10a_27)

on_farm_income_lvstock <- group_by(lvstock, y2_hhid) %>%
  summarise(lvstock_value_hh=sum(lvstk_value, na.rm=TRUE),
            slaughter_value_hh=sum(slaughter_value, na.rm=TRUE))
rm(lvstock)

#######################################
# -------------------------------------
# calcualte total income
# -------------------------------------
#######################################

income_2010 <- full_join(on_farm_income_lvstock, on_farm_income_rent)
income_2010 <- full_join(income_2010, on_farm_income_perm)
income_2010 <- full_join(income_2010, on_farm_income_fruit)
income_2010 <- full_join(income_2010, on_farm_income_crop)
income_2010 <- full_join(income_2010, off_farm_income_hh)

income_2010$income <- with(income_2010,
                  rowSums(cbind(off_farm_income_hh, crop_value_hh,
                                fruit_value_hh, perm_value_hh,
                                lvstock_value_hh, slaughter_value_hh,
                                rent_value_hh),
                          na.rm=TRUE))

# remove all the other data
rm(off_farm_income_hh, on_farm_income_lvstock,
   on_farm_income_rent, on_farm_income_perm,
   on_farm_income_fruit, on_farm_income_crop,
   dataPath)
