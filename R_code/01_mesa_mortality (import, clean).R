## Joon Chung

## Joon Chung
## Brigham and Women's Hospital, Harvard Medical School
## jchung26@bwh.harvard.edu
## jchung@g.harvard.edu

## Title:
# Racial/ethnic Differences in Actigraphy, Questionnaire, 
#   and Polysomnography-measured Indicators of Healthy Sleep: 
#   The Multi-Ethnic Study of Atherosclerosis 

## Last update: 4/19/2020

##################################################################################
##################################################################################
setwd("C:/Users/joonc/Desktop/Dropbox/MESA - racial-ethnic disparities/datasets") # home pc
 setwd("C:/Users/Ae and Jong/Dropbox (Partners HealthCare)/MESA/MESA") # Work PC

 setwd("C:/Users/jj261/Dropbox (Partners HealthCare)/Dropbox/MESA - racial-ethnic disparities/datasets") # home pc
 
 
 
 library(haven)
 MESAe5_SleepActigRhy_20170905 <- read_dta("~/Dropbox (Partners HealthCare)/MESA/MESA/MESAe5_SleepActigRhy_20170905.dta")
 View(MESAe5_SleepActigRhy_20170905)

library(foreign)
library(dplyr)
library(ggplot2)
library(mosaic)

mesa_1 <- read.dta("MESAe1FinalLabel01022018.dta")
mesa_2 <- read.dta("MESAe2FinalLabel10052017.dta")
mesa_3 <- read.dta("MESAe3FinalLabel10052017.dta")
mesa_4 <- read.dta("MESAe4FinalLabel10052017.dta")
mesa_5 <- read.dta("MESAe5_FinalLabel_20171005.dta")
mesa_6 <- read.dta("MESAe6_FinalLabel_20190819.dta")
# mesa_events <- read.dta("MESAEvThru2016_20190215.dta")

mesa_psg <- read.dta("MESAe5_SleepPolysomn_20160922.dta")
# mesa_eegband <- read.dta("MESAe5_SleepEEGBand_20150501.dta")
# mesa_eegspect <- read.dta("MESAe5_SleepEEGSpectral_20150501.dta")
mesa_act <- read.dta("MESAe5_SleepActigraphy_20180201.dta")
# mesa_act_rhy <- read.dta("MESAe5_SleepActigRhy_20170905.dta")
mesa_sleepq <- read.dta("MESAe5_SleepQ_20140617.dta")

# Join exams 3, 5, act, act_rhy, psg, sleepq
mesa_df <- merge(mesa_5, mesa_psg, by = "idno")
# mesa_df <- merge(mesa_df, mesa_6, by = "idno")
mesa_df <- merge(mesa_df, mesa_act, by = "idno")
mesa_df <- merge(mesa_df, mesa_sleepq, by = "idno")
# mesa_df <- merge(mesa_df, mesa_events, by = "idno")

##################################################################################
##################################################################################
## Covariate recode

# sex/gender
mesa_df$female <- ifelse(mesa_df$gender1 == "0: FEMALE", 1, 0)
mesa_df$male <- ifelse(mesa_df$female == 0, 1, 0)

# age
mesa_df$age <- mesa_df$age5c

# income
mesa_df$income <- mesa_df$income5

# degree attainment
mesa_ed <- mesa_1 %>% dplyr::mutate(edattain = educ1) %>%
  dplyr::select(edattain, idno)

mesa_df <- merge(mesa_df, mesa_ed, by = "idno")

table(mesa_df$edattain)

mesa_df$ed <- mesa_df$edattain
levels(mesa_df$ed)

mesa_df$degree_attain <- factor(NA, levels = c("Less than high school",
                                               "High school or some college",
                                               "College degree", "Graduate"))
mesa_df$degree_attain[mesa_df$ed == "0: NO SCHOOLING" |
                        mesa_df$ed == "1: GRADES 1-8" |
                        mesa_df$ed == "2: GRADES 9-11"] <- "Less than high school"

mesa_df$degree_attain[mesa_df$ed == "3: COMPLETED HIGH SCHOOL/GED" | 
                        mesa_df$ed == "4: SOME COLLEGE BUT NO DEGREE" |
                        mesa_df$ed == "5: TECHNICAL SCHOOL CERTIFICATE" |
                        mesa_df$ed == "6: ASSOCIATE DEGREE"] <- "High school or some college"


mesa_df$degree_attain[mesa_df$ed == "7: BACHELOR'S DEGREE"] <- "College degree"
mesa_df$degree_attain[mesa_df$ed == "8: GRADUATE OR PROFESSIONAL SCHOOL"] <- "Graduate"


table(mesa_df$ed, mesa_df$degree_attain) # Looks good

# race
mesa_df$race <- mesa_df$race1c.x
mesa_df$race <- factor(mesa_df$race, labels = c("White", "Chinese", "Black", "Hispanic"))
table(mesa_df$race)

## Marital status
summary(mesa_df$marital5)
levels(mesa_df$marital5)
mesa_df$married <- ifelse(mesa_df$marital5 == "1: MARRIED / LIVING WITH PARTNER", 1, 0)

# income: income (midpoint of each category)
mesa_df$income_cat <- mesa_df$income5
mesa_df$income <- mesa_df$income
income_cat_levels <- levels(mesa_df$income_cat)
# "< $5,000"       "$5,000-7,999"   "$8,000-11,999'" "$12,000-15,999"
# "$16,000-19,999" "$20,000-24,999" "$25,000-29,999" "$30,000-34,999" "$35,000-39,999"
# "$40,000-49,999" "$50,000-74,999" "$75,000-99,999" "$100,000 +"
mesa_df$income <- NA
mesa_df$income[mesa_df$income_cat == "1: < $5000"] <- 2500
mesa_df$income[mesa_df$income_cat == "2: $5000 - $7999"] <- 6499.5
mesa_df$income[mesa_df$income_cat == "3: $8000 - $11999"] <- 9999.5
mesa_df$income[mesa_df$income_cat == "4: $12000 - $15999"] <- 13999.5
mesa_df$income[mesa_df$income_cat == "5: $16000 - $19999"] <- 17999.5
mesa_df$income[mesa_df$income_cat == "6: $20000 - $24999"] <- 22499.5
mesa_df$income[mesa_df$income_cat == "7: $25000 - $29999"] <- 27499.5
mesa_df$income[mesa_df$income_cat == "8: $30000 - $34999"] <- 32499.5
mesa_df$income[mesa_df$income_cat == "9: $35000 - $39999"] <- 37499.5
mesa_df$income[mesa_df$income_cat == "10: $40000 - $49999"] <- 44999.5
mesa_df$income[mesa_df$income_cat == "11: $50000 - $74999"] <- 62499.5
mesa_df$income[mesa_df$income_cat == "12: $75000 - $99999"] <- 87499.5
mesa_df$income[mesa_df$income_cat == "13: $100,000 - $124,999"] <- 112499.5
mesa_df$income[mesa_df$income_cat == "14: $125,000 - $149,999"] <- 137499.5
mesa_df$income[mesa_df$income_cat == "15: $150,000 or more"] <- 150000

qplot(mesa_df$income)

mesa_df$income <- mesa_df$income/10000

ApplyQuintiles <- function(x) {
  cut(x, breaks=c(quantile(mesa_df$income, probs = seq(0, 1, by = 0.33), na.rm=T)), 
      labels=c("lowest tertile","middle tertile","highest tertile"), include.lowest=TRUE)
}

mesa_df$income_tertile <- sapply(mesa_df$income, ApplyQuintiles)
qplot(mesa_df$income_tertile)

## Depressive symptoms minus the sleep component
#  cesd5c - (badslp5)
library(foreign)
setwd("~/Dropbox (Partners HealthCare)/MESA/MESA/MESAe5_IndividualDomains_20190823/MESAe5_IndividualDomains_20190823/MESA_Exam5_HealthLife")
mesa_healthlife_df <- read.dta("MESAe5_HealthLife_20120701.dta")
mesa_healthlife_df <- mesa_healthlife_df %>% dplyr::mutate(cesd_sleep_item = as.numeric(badslp5) - 1) %>%
  dplyr::select(cesd_sleep_item, badslp5, cesd5c, idno) 

table(mesa_healthlife_df$badslp5, mesa_healthlife_df$cesd_sleep_item)

mesa_df <-  merge(mesa_df, mesa_healthlife_df, by = "idno")

mesa_df$cesd_nosleep <- mesa_df$cesd5c - mesa_df$cesd_sleep_item

# setwd("C:/Users/joonc/Desktop/Dropbox/MESA - racial-ethnic disparities")

## Medications (sleep): benzos and ssris
# mesa_df$benzo <- mesa_df$benzod5c
# 
# mesa_df$ssri <- mesa_df$ssri5c
# 
# mesa_df$snri <- mesa_df$snri5c
# 
# table(mesa_df$benzo)
# table(mesa_df$ssri)
# table(mesa_df$snri)
# 
# table(mesa_df$benzo, mesa_df$ssri)

# Pack years
mesa_df$pack_years <- mesa_df$pkyrs5c

# Moderate physical activity
mesa_df$modvig_pa <- mesa_df$pamvcm5c

# smoking, pack years: pkyrs3c
mesa_df$pack_years <- mesa_df$pkyrs5c
summary(mesa_df$pack_years)

# employment status
mesa_df$employed <- mesa_df$empstat5
table(mesa_df$employed)

# Other
mesa_df$bmi <- mesa_df$bmi5c
summary(mesa_df$bmi)

mesa_df$site <- mesa_df$site5c

## current alcohol consumption
mesa_df$alcohol <- mesa_df$curalc5

# smoke status
mesa_df$smoke <- mesa_df$smkstat5

## Back to cluster dropbox
setwd("/Users/jj261/Dropbox (Partners HealthCare)/2022_sleep_clusters_mesa")

# End script

