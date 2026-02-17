## Joon Chung
## Contact: see README
## 01/17/2022

## 3 data-frames: 1) mesa_df (socio, lifestyle, sleep)
##                2) aehi_df (diet)
##                3) mesa_all_events (mortality, cvd, copd, cancer)

library(tidyverse)
library(labelled)
mesa_df <- mesa_df %>% dplyr::select(-pc1_scaled.x, -pc1_scaled.y,
                                     -cesd5c.x, -cesd5c.y)

mesa_subset_df <- mesa_df %>% 

  dplyr::mutate(tst_hr = tst/60) %>%

  dplyr::select(
  # Socio-demographics
  age5c, female, race, idno,
  degree_attain, income_tertile, married,
  
  # Lifestyle
  smoke, modvig_pa, bmi, 
  
  # Sleep vars (continuous)
  tst_hr, tst, mpsd, log_mpsd, sdtst,
  ahi, sws_per, rem_per, frag,
  sme, waso, quality, ess, log_timing,
  sol, sol_sub, slpexam1, cluster_f, cluster_num, cluster_notst_onlyreg_f,
  cluster_notst_onlyreg_f_k3,
  
  # Dichotomous
  tst_di, mpsd_di, sdtst_di, ahi_di,
  swsper_di, remper_di, frag_di, 
  sme_di, waso_di, quality_di, ess_di,
  timing_di, sol_di,
  
  # Sleep scores
  shs, pc1_scaled
) %>% 
  dplyr::mutate(pc1_scaled = as.numeric(pc1_scaled)) 

dim(mesa_subset_df)

## mesa_all_events
names(mesa_all_events)
# [1] "idno"     "cvda"     "cvdatt"   "dth"      "dthtt"    "cancer"   "cancertt" "copd"    
# [9] "copdtt"  

## diet aehi_df
names(aehi_df)

## merge mesa_subset_df, aehi_df, mesa_all_events
shs_df <- merge(mesa_subset_df, aehi_df, by = "idno")
shs_df <- merge(shs_df, mesa_all_events, by = "idno")

## Check follow-up times and generate in years
shs_df <- shs_df %>% dplyr::mutate(death_tt_years = (dthtt - slpexam1)/360) %>%
  dplyr::filter(death_tt_years >= 0) %>% 
  mutate(death = dth,
         prev_cvd = ifelse(cvdatt <= slpexam1, 1, 0),
         prev_cancer = ifelse(cancertt <= slpexam1, 1, 0),
         prev_copd = ifelse(copdtt <= slpexam1, 1, 0),
         
         shs_scaled = scale(shs)
         ) 


shs_df <- shs_df %>% 
  set_variable_labels(shs = "Sleep health score",
                      shs_scaled = "Sleep health score (scaled)",
                      pc1_scaled = "Sleep health score (PC1)",
                      
                      ## Socio-demographics
                      age5c = "Age (years)",
                      female = "Female",
                      race = "Race-Ethnicity",
                      income_tertile = "Income (tertiles)",
                      degree_attain = "Education",
                      married = "Married",
                      
                      ## Lifestyle factors
                      modvig_pa = "Moderate-vigorous physical activity",
                      smoke = "Smoker status",
                      aehi_10 = "Alternative Healthy Eating Index - 10",
                      
                      ## Prevalent disease
                      prev_cvd = "Prevalent cardiovascular disease",
                      prev_cancer = "Prevalent cancer",
                      prev_copd = "Prevalent COPD",
                      
                      ## Sleep metrics
                      log_mpsd = "Midpoint sd (log; min)",
                      mpsd_di = "Midpoint sd <30 min",
                      tst_hr = "Total sleep time (hrs)",
                      tst_di = "Total sleep time 6-8 hrs",
                      sdtst = "Duration sd (min)",
                      sdtst_di = "Duration sd <90 min",
                      ahi = "The Apnea-Hypopnea Index (events/hr)",
                      ahi_di = "The Apnea-Hypopnea Index <=15 events/hr",
                      sme = "Sleep maintenance efficiency (%)",
                      sme_di = "Sleep maintenance efficiency >90%",
                      waso = "Wake after sleep onset (min)",
                      quality = "Quality",
                      ess = "Epworth Sleepiness Scale",
                      sws_per = "% N3",
                      rem_per = "% R",
                      frag = "Fragmentation Index",
                      sol = "Sleep onset latency (min)",
                      sol_sub = "Difficulties initiating sleep",
                      log_timing = "Timing (log; min)"
                      
                      
                      ## REST OF VARIABLEAS
                      )
dim(shs_df)
summary(shs_df)

table(shs_df$death)
# 0     1 
# 1555  171 

summary(shs_df$death_tt_years)

## 171 deaths of 1726 observations

## Back to cluster dropbox
setwd("<PROJECT_DIR>")

## End script

