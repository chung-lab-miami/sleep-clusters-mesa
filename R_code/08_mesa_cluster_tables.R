## mesa mortality: table one and Cox model result tables
library(tableone)
library(tidyverse)

shs_df <- shs_df %>% 
  dplyr::mutate(death_years_slpexam = (dthtt/365 - slpexam1/365))
  
# skewed vars IQR 
skewed_vars <- c("mpsd", "sdtst", "death_tt_years", "totmed5")

              # Outcome

soc_vars <- c("death", "death_tt_years",
              
              # sleep vars
              "mpsd", "sdtst", "tst_hr",
              
              # Socio-demographics
              "age5c", "female", "race", 
              
              # Lifestyle
              "smoke", "modvig_pa", "cluster_f",
              
              "married", "work_sched",
              
              # Health status indicators
              "bmi", "prev_cvd", "cesd_nosleep",
              
              "totmed5",
              
              # sleep disorder
              "rstlesslgs5", "ahi", "whiirs5c" 
              
              )

cat_vars <- c("female", "race", 
              "prev_cvd", "death", "rstlesslgs5",
              "cluster_f")           

mesa_mortality_tableone_overall <- CreateTableOne(vars = soc_vars, data = cluster_df, factorVars = cat_vars)
mesa_mortality_tableone_stratified <- CreateTableOne(vars = soc_vars, 
                                                     data = cluster_df, 
                                                     factorVars = cat_vars,
                                                     strata = "cluster_f")

tableone_overall <- print(mesa_mortality_tableone_overall, nonnormal = skewed_vars)
tableone_stratified <- print(mesa_mortality_tableone_stratified, nonnormal = skewed_vars)

write.csv(tableone_overall, file = "mesa_mortality_tableone_overall.csv")
write.csv(tableone_stratified, file = "mesa_mortality_tableone_stratified.csv")

## 



# Supplemental table
sleep_vars_tableone <- c("tst_hr", "mpsd", "sdtst", "sme", "frag", "waso", "ahi", "sws_per", "rem_per",
                         "quality_f", "sol", "dis_f", "ess", "log_timing")

sleep_cat_vars <- c("pc1_tertiles", "quality_f", "dis_f")

sleep_skewed_vars <- c("mpsd", "frag",
                       "waso", "sol", "sws_per", 
                       "ahi")

mesa_mortality_supplement_pctertiles <- CreateTableOne(vars = sleep_vars_tableone, data = shs_df, factorVars = sleep_cat_vars,
                                           strata = "pc1_tertiles")

stratified_supplement_pctertiles <- print(mesa_mortality_supplement_pctertiles, nonnormal = sleep_skewed_vars)

write.csv(stratified_supplement_pctertiles, file = "mesa_mortality_appendix_pctertile_table.csv")
