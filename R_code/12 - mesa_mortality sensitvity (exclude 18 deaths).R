## Sensitivity analyses: Exclude deaths within 1 year of exam

cluster_df$death_lag <- cluster_df$dthtt/365 - cluster_df$slpexam1/365

sensitivity_df <- cluster_df %>% dplyr::mutate(death_excl = ifelse(death_lag < 1 & death == 1, 1, 0)) %>%
  mutate(death_1 = ifelse(death_excl==0 & death == 1, 1, 0)) %>%
  filter(death_lag > 1)


## Sleep Health Score regressions
clus_model_0_sens <- coxph(Surv(dthtt-slpexam1, death_1) ~ cluster_f + age5c + female + race + 
                            work_sched + married, data = sensitivity_df)

summary(clus_model_0_sens)
ShowRegTable(clus_model_0_sens) # HR: 0.59 [0.43, 0.81]


## Lifestyle
clus_model_1_sens <- coxph(Surv(dthtt-slpexam1, death) ~ cluster_f + age5c + female + race + 
                       work_sched + married + 
                         modvig_pa + smoke , data = sensitivity_df)

summary(clus_model_1_sens)
ShowRegTable(clus_model_1_sens) # HR: 0.57 [0.42, 0.80]

## Medical comorbidity
clus_model_2_sens <- coxph(Surv(dthtt-slpexam1, death) ~ cluster_f + age5c + female + race + 
                            work_sched + married + 
                            modvig_pa + smoke +
                            bmi + cesd_nosleep + prev_cvd + totmed5, data = sensitivity_df)

summary(clus_model_2_sens)
ShowRegTable(clus_model_2_sens)

## Sleep disorder
clus_model_3_sens <- coxph(Surv(dthtt-slpexam1, death) ~ cluster_f + age5c + female + race + 
                             work_sched + married + 
                             modvig_pa + smoke +
                             bmi + cesd_nosleep + prev_cvd + totmed5 + 
                             ahi + whiirs5c + rstlesslgs5, data = sensitivity_df)

summary(clus_model_3_sens)
ShowRegTable(clus_model_3_sens) # HR: 0.62 [0.44, 0.86]

## Medical comorbidity w/ AGE as time metric
clus_model_age_sens <- coxph(Surv(age5c, death) ~ cluster_f  + female + race + 
                             work_sched + married + 
                             modvig_pa + smoke +
                             bmi + cesd_nosleep + prev_cvd + totmed5, data = sensitivity_df)

summary(clus_model_age_sens) # n= 1740, number of events= 157 
ShowRegTable(clus_model_age_sens) # HR: 0.71 [0.51, 0.99]

