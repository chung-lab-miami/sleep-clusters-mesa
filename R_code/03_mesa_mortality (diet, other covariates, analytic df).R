## Joon Chung
## jchung26@bwh.harvard.edu
## 01/17/2022

## Add diet code + prevalent outcomes
## Select to analytical df

## AEHI-10 scoring

## Summary score of:
##  Vegetable score
##  Fruit score
##  Grain score
##  Legume score
##  Meat score
##  Transfat score
##  Longchain score
##  PUFA score
##  Sodium score
##  Alcohol score

library(haven)
library(tidyverse)
library(ggplot2)

setwd("/Users/jj261/Dropbox (Partners HealthCare)/MESA/MESA/MESAe5_IndividualDomains_20190823")
## Folders within:
## MESA_Exam5_Diet_FFQ/MESAe5_FFQ_20150928.dta
## MESA_Exam5_Diet_Nutrients/MESAe5_Nutrient_20150331.dta

# read_dta("C:/Users/jj261/Dropbox (Partners HealthCare)/MESA/MESAe5_IndividualDomains_20190823/MESAe5_IndividualDomains_20190823/MESA_Exam5_Diet_FFQ/MESAe5_FFQ_20150928.dta")

mesa_ffq5 <- read_dta("MESAe5_IndividualDomains_20190823/MESA_Exam5_Diet_FFQ/MESAe5_FFQ_20150928.dta")
mesa_nutrients5 <- read_dta("MESAe5_IndividualDomains_20190823/MESA_Exam5_Diet_Nutrients/MESAe5_Nutrient_20150331.dta")

## Code variables
mesa_ffq5 <- mesa_ffq5 %>% 
  dplyr::mutate(veg = fgavocado5c + fgppsalad5c +
                  fgtomato5c + fgvcrucifer5c + fgvdyellow5c +
                  fgvgreenleafy5c + fgvother5c + fgvpotato5c) %>%
  dplyr::mutate(bev = fgfruitjuice5c + fghotchoc5c + fgsoda5c) %>%
  dplyr::mutate(legumes = fglegumes5c + fgseedsnuts5c + fgsoy5c) %>%
  dplyr::mutate(meat = fghfprocmeat5c + fgredmeat5c) %>%
  dplyr::mutate(alcohol = fgbeer5c + fgotheralcohol5c)

mesa_nutrients5 <- mesa_nutrients5 %>%
  dplyr::mutate(longchain = (pf205n5c + pf226n5c)* 1000)

## Code composite scores
#     Vegetable score
mesa_ffq5$vegscore <- NA
mesa_ffq5$vegscore <- (mesa_ffq5$veg/5)*10
mesa_ffq5$vegscore[mesa_ffq5$veg >= 5] <- 10

qplot(mesa_ffq5$vegscore)

#     Fruit score
mesa_ffq5$fruitscore <- NA
mesa_ffq5$fruitscore <- (mesa_ffq5$fgfruit5c/4)*10
mesa_ffq5$fruitscore[mesa_ffq5$fgfruit5c >= 4] <- 10

qplot(mesa_ffq5$fruitscore)

#     Whole grain score (sex-specific)
#     Code after joining with main wave

#     Beverage score
mesa_ffq5$bevscore <- NA
mesa_ffq5$bevscore <- 10
mesa_ffq5$bevscore_01 <- (10-(mesa_ffq5$bev*10)) 

mesa_ffq5 <- mesa_ffq5 %>% 
  dplyr::mutate(bevscore = case_when( bev == 0 ~ 10,
                                      bev>0 & bev < 1 ~ 10-(bev*10),
                                      bev>=1 ~ 0)) %>%
  #     Legume score
  dplyr::mutate(legumescore = case_when(legumes >= 1 ~ 10,
                                        legumes >=0 & legumes <1 ~ (legumes*10))) %>%
  
  #     Meat score  
  
  dplyr::mutate(meatscore = case_when(meat >= 1.5 ~ 0,
                                      meat >= 0 & meat < 1.5 ~ (10-(meat/1.5)*10)))

#     Transfat score
mesa_nutrients5 <- mesa_nutrients5 %>%
  dplyr::mutate(middle_transfatscore = (10 - ((pcltfn5c - 0.5)/3.5) * 10)) %>%
  dplyr::mutate(transfatscore = case_when(pcltfn5c >= 0  & pcltfn5c <= 0.5 ~ 10,
                                          pcltfn5c > 0.5 & pcltfn5c < 4 ~ middle_transfatscore,
                                          pcltfn5c >= 4 ~ 0)) %>%
  
  #     Longchain score
  dplyr::mutate(longchain = (pf205n5c + pf226n5c)*1000) %>%
  dplyr::mutate(longchainscore = case_when(longchain == 0 ~ 0,
                                           longchain > 0 & longchain < 250 ~ ((longchain/250)*10),
                                           longchain >= 250 ~ 10)) %>%
  
  #     PUFA
  dplyr::mutate(pufa_2_10 = pclpfn5c/2) %>%
  dplyr::mutate(pufascore = case_when(pclpfn5c >= 10 ~ 10,
                                      pclpfn5c > 2 & pclpfn5c < 10 ~ pufa_2_10,
                                      pclpfn5c <= 2 ~ 0)) %>%
  dplyr::mutate(nan5cq = ntile(nan5c, 10)) %>%
  dplyr::mutate(sodiumscore = 10-nan5cq)



qplot(mesa_nutrients5$transfatscore)
qplot(mesa_nutrients5$longchainscore)
qplot(mesa_nutrients5$pufascore)
qplot(mesa_nutrients5$sodiumscore)

## Join ffq, nutrients, gender1 for sex-based cutoffs
female_df <- mesa_df %>% dplyr::select(female, idno)

mesa_ffq_nut <- merge(mesa_ffq5, mesa_nutrients5, by = "idno")
mesa_diet <- merge(female_df, mesa_ffq_nut, by = "idno")

mesa_diet$gender1 <- ifelse(mesa_diet$female == 1, 0, 1)

mesa_diet <- mesa_diet %>% dplyr::mutate(wholegrainscore = case_when(gender1 == 0 & 
                                                                       fgwholegrain5c >5 ~ 10,
                                                                     gender1 == 0 & fgwholegrain5c <=5 &
                                                                       fgwholegrain5c >= 0 ~ (fgwholegrain5c/5)*10,
                                                                     
                                                                     gender1 == 1 & fgwholegrain5c < 6 ~ 10,
                                                                     gender1 == 1 & fgwholegrain5c >= 0 &
                                                                       fgwholegrain5c <=6 ~ (fgwholegrain5c/6)*10)) %>%
  dplyr::mutate(alcoholscore = case_when(gender1 == 0 & alcohol >=0.5 & alcohol <=1.5 ~ 10,
                                         gender1 == 0 & alcohol >=2.5 ~ 0,
                                         gender1 == 0 & alcohol >1.5 & alcohol <2.5 ~ (10 - ((alcohol -1.5) * 10)),
                                         gender1 == 0 & alcohol >0  & alcohol <0.5 ~ (alcohol/0.5 * 7.5 + 2.5),
                                         gender1 == 0 & alcohol == 0 ~ 2.5,
                                         
                                         gender1 == 1 & alcohol >= 0.5 & alcohol <=2 ~ 10,
                                         gender1 == 1 & alcohol >=3.5 ~ 0,
                                         gender1 == 1 & alcohol >2 & alcohol <3.5 ~ (10 - ((alcohol - 2.0) / 1.5) *10),
                                         gender1 == 1 & alcohol > 0 & alcohol <0.5 ~ ( alcohol/0.5 * 7.5 + 2.5),
                                         gender1 == 1 & alcohol == 0 ~ 2.5
  ))

qplot(mesa_diet$wholegrainscore)
qplot(mesa_diet$alcoholscore)

## AEHI-10 scoring

## Summary score of:
##  Vegetable score
##  Fruit score
##  Grain score
##  Legume score
##  Meat score
##  Transfat score
##  Longchain score
##  PUFA score
##  Sodium score
##  Alcohol score

mesa_diet$aehi_10 <- with(mesa_diet, vegscore + fruitscore + wholegrainscore + 
                            legumescore + meatscore + transfatscore + longchainscore+
                            pufascore + sodiumscore + alcoholscore)


qplot(mesa_diet$aehi_10) + xlab("AEHI-10 (MESA 5)") + theme_bw()

aehi_df <- mesa_diet %>% dplyr::select(aehi_10, idno,
                                       vegscore, fruitscore, wholegrainscore,
                                       legumescore, meatscore, transfatscore,
                                       longchainscore, pufascore, sodiumscore,
                                       alcoholscore)

## 3 data-frames: 1) mesa_df (socio, lifestyle, sleep)
##                2) aehi_df (diet)
##                3) mesa_all_events (mortality, cvd, copd, cancer)

## Back to cluster dropbox
setwd("/Users/jj261/Dropbox (Partners HealthCare)/2023_sleep_clusters_mesa")

## End script

