gg_miss_var(mesa_subset_df)

## mesa_subset_df + add controls (bmi, cvd)
mesa_subset_df <- mesa_subset_df %>% dplyr::select(-income_tertile) %>%
  set_variable_labels(
                      pc1_scaled = "Sleep health score (PC1)",
                      
                      ## Socio-demographics
                      age5c = "Age (years)",
                      female = "Female",
                      race = "Race-Ethnicity",
                      degree_attain = "Education",
                      married = "Married",
                      
                      ## Lifestyle factors
                      modvig_pa = "Moderate-vigorous physical activity",
                      smoke = "Smoker status",

                      ## Prevalent disease
                      # prev_cvd = "Prevalent cardiovascular disease",
                      
                      ## Sleep metrics
                      log_mpsd = "Midpoint sd (log; min)",
                      tst_hr = "Total sleep time (hrs)",
                      sdtst = "Duration sd (min)",
                      ahi = "The Apnea-Hypopnea Index (events/hr)",
                      sme = "Sleep maintenance efficiency (%)",
                      waso = "Wake after sleep onset (min)",
                      quality = "Quality",
                      ess = "Epworth Sleepiness Scale",
                      sws_per = "% N3",
                      rem_per = "% R",
                      frag = "Fragmentation Index",
                      sol_sub = "Difficulties initiating sleep",
                      log_timing = "Timing (log; min)"
                      
                      
                      ## REST OF VARIABLEAS
  )

cvd_df <- mesa_all_events %>% dplyr::select(cvda, idno)

mesa_subset_miss_df <- merge(mesa_subset_df, cvd_df, by = "idno")
gg_miss_var(mesa_subset_miss_df)

missing_shs_df <- mesa_subset_miss_df[c(1291, 1447, 1501),]

## Compare income tertile between analytic and given sample

# dim(mesa_subset_df)
# [1] 1814   42
# 
# dim(na.omit(mesa_subset_miss_df)) Large missingness in income
# [1] 1793   43