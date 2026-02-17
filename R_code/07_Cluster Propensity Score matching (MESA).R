## Matching with sleep clusters
## Matching variables:
## age5c, female, race, income_tertiles, degree_attain,
## married, aehi_10, modvig_pa, smoke
set.seed(8675309)

library(MatchIt)
library(coxrobust)
library(survival)
library(rms)

with(shs_df, boxplot(tst_hr ~ cluster_f))
with(shs_df, boxplot(mpsd ~ cluster_f))
with(shs_df, boxplot(sdtst ~ cluster_f))
table(shs_df$cluster_f)

## CESD scores
# mesa_healthlife_df$cesd_nosleep <- as.numeric(mesa_healthlife_df$cesd5c) - as.numeric(mesa_healthlife_df$badslp5)
# 
# cesd_df <- mesa_healthlife_df %>% dplyr::select(cesd_nosleep, idno)
# 
# shs_df <- merge(shs_df, cesd_df, by = "idno")
# Matching (Full)

# dim(na.omit(shs_df))
# 
# cluster_df <- na.omit(shs_df)

m.out = matchit(cluster_f ~ age5c + female + race + 
                modvig_pa + smoke + work_sched + married + aehi_10, 
                
                
                
                
                data = shs_df, method= "full",
                ratio = 1,
                caliper = 0.2,
                distance = "glm",
                link = "probit",
                estimand = "ATE")

summary(m.out)

var_names <- c(age5c = "Age (years)",
               female = "Female",
               race = "Race/ethnicity",
               modvig_pa = "Mod/Vig physical activity",
               smoke = "Smoke status",
               income_tertile = "Income (tertiles)",
               degree_attain = "Education",
               married = "Married", 
               aehi_10 = "AEHI-10 (diet)",
               bmi = "Body Mass Index",
               prev_copd = "COPD",
               prev_cancer = "Cancer",
               prev_cvd = "CVD",
               ahi_di = "AHI<=15",
               cesd_nosleep = "CES-D"
               )
set.cobalt.options(binary = "std")
primary_love_plot <- love.plot(m.out, 
          drop.distance = TRUE, 
          var.order = "unadjusted",
          abs = TRUE,
          line = TRUE, 
          thresholds = c(m = .1),
          var.names = var_names,
          colors = c("red", "blue"),
          shapes = c("triangle filled", "circle filled"),
          sample.names = c("Before PSM", "After PSM"),
          limits = c(0, .5),
          position = c(.75, .25)) +
  theme(legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1)) + 
  theme_classic(base_size = 15)


ggpubr::ggexport(primary_love_plot, 
                 filename = "primary_love_plot.png",
                 width = 600,
                 height = 500,
                 pointsize = 24) 
love.plot(m.out)

# # Matching with splines - no substantive difference
# m.out_splines = matchit(cluster_f ~ rcs(age5c, 4) + female + race + 
#                   rcs(modvig_pa, 4) + smoke + income_tertile + 
#                   degree_attain + married + rcs(aehi_10, 4), 
#                 data = cluster_df, method= "full",
#                 ratio = 1,
#                 caliper = 0.02,
#                 distance = "glm",
#                 link = "probit")
# 
# summary(m.out)
# summary(m.out_splines)


# Matching with ahi, bmi, depressive symptoms, prevalent disease
totmed_df <- mesa_df %>% dplyr::select(totmed5, idno)
shs_df <- merge(shs_df, totmed_df, by = "idno")

kitchen_sink_df <- shs_df[complete.cases(shs_df$cesd_nosleep),]
kitchen_sink_df <- kitchen_sink_df[complete.cases(kitchen_sink_df$totmed5),]

htn_dm_totmed_df <- mesa_df %>% dplyr::select(htn5c, dm035c, totmed5, idno)
kitchen_sink_df <- na.omit(merge(kitchen_sink_df, htn_dm_totmed_df, by = "idno"))

m.out_ks = matchit(cluster_f ~ age5c + female + race + 
                          modvig_pa + smoke + income_tertile + 
                          degree_attain + married + aehi_10 + ahi_di +
                          bmi + totmed5.x + htn5c.x + dm035c.x +  
                          cesd_nosleep, 
                        data = kitchen_sink_df, method= "full",
                        ratio = 1,
                        caliper = 0.2,
                        distance = "glm",
                        link = "probit",
                   estimand = "ATE")

set.cobalt.options(binary = "std")
primary_love_plot_ks <- love.plot(m.out_ks, 
                               drop.distance = TRUE, 
                               var.order = "unadjusted",
                               abs = TRUE,
                               line = TRUE, 
                               thresholds = c(m = .1),
                               var.names = var_names,
                               colors = c("red", "blue"),
                               shapes = c("triangle filled", "circle filled"),
                               sample.names = c("Before PSM", "After PSM"),
                               limits = c(0, .5),
                               position = c(.75, .25)) +
  theme(legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1)) + 
  theme_classic(base_size = 15)

summary(m.out_ks)

ggpubr::ggexport(primary_love_plot_ks, 
                 filename = "primary_love_plot_ks.png",
                 width = 600,
                 height = 500,
                 pointsize = 24) 
## Summaries
balance_write_all <- summary(m.out)$sum.all
balance_write_matched <- summary(m.out)$sum.matched

write.csv(balance_write_all, file = "balance_all_stats.csv")
write.csv(balance_write_matched, file = "balance_matched_stats.csv")

plot(m.out, type = "hist")
plot(m.out_ks, type = "hist")

m.data1 <- match.data(m.out)
m.data1_ks <- match.data(m.out_ks)

m.data1 <- m.data1 %>% 
  set_variable_labels(
                      age5c = "Age (years)",
                      female = "Female",
                      race = "Race-Ethnicity",
                      modvig_pa = "Moderate-vigorous physical activity",
                      smoke = "Smoker status",
                      income_tertile = "Income (tertiles)",
                      degree_attain = "Education",
                      married = "Married",
                      aehi_10 = "Alternative Healthy Eating Index - 10",
                      cluster_f = "Sleep cluster",
                      prev_cvd = "Prevalent cardiovascular disease",
                      prev_cancer = "Prevalent cancer",
                      prev_copd = "Prevalent COPD",
                      cluster_f = "Un/Favorable sleep clusters",
                      ahi_di = "Apnea-Hypopnea Index >15")

# Generate IPTW weights
m.data1$iptw <- ifelse(m.data1$cluster_f == "Favorable sleep",
                       1/m.data1$distance,
                       1/(1-m.data1$distance))
favstats(m.data1$iptw)
qplot(m.data1$iptw)


# Model on matched data
fit_matched <- coxph(Surv(dthtt-slpexam1, death) ~ cluster_f, data = m.data1,
                             weights = weights, cluster = subclass)
summary(fit_matched)
ShowRegTable(fit_matched)

fit_matched_ks <- coxph(Surv(dthtt-slpexam1, death) ~ cluster_f, data = m.data1_ks,
                                weights = weights, cluster = subclass)
summary(fit_matched_ks)
ShowRegTable(fit_matched_ks)




fit_matched_cov <- coxph(Surv(dthtt-slpexam1, death) ~ cluster_f + female +
                       race + age5c, data = m.data1,
                     weights = weights, cluster = subclass)
summary(fit_matched_cov)
ShowRegTable(fit_matched_cov)





# Model on original data with normal adjustment #### REPORT
fit_unmatched <- coxph(Surv(dthtt-slpexam1, death) ~ cluster_f + age5c + female + race + income_tertile + 
                       degree_attain + married + aehi_10 + smoke + modvig_pa, data = shs_df)
summary(fit_unmatched)

fit_unmatched_mpsd <- coxph(Surv(dthtt-slpexam1, death) ~ mpsd_di + age5c + female + race + income_tertile + 
                         degree_attain + married + aehi_10 + smoke + modvig_pa, data = shs_df)
summary(fit_unmatched_mpsd)

fit_unmatched_tstsd <- coxph(Surv(dthtt-slpexam1, death) ~ sdtst_di + age5c + female + race + income_tertile + 
                         degree_attain + married + aehi_10 + smoke + modvig_pa, data = shs_df)
summary(fit_unmatched_tstsd)

fit_unmatched_tst <- coxph(Surv(dthtt-slpexam1, death) ~ tst_di + age5c + race + income_tertile + 
                         degree_attain + married + aehi_10 + smoke + modvig_pa, data = shs_df)
summary(fit_unmatched_tst)


# Model on original data with extra adjustment #### REPORT
## BMI, prevalent cvd, copd, cancer, the AHI, cesd_nosleep
fit_unmatched_sens <- coxph(Surv(dthtt-slpexam1, death) ~ cluster_f + age5c + race + income_tertile + 
                         degree_attain + married + aehi_10 + smoke + modvig_pa + 
                           bmi + prev_cvd + prev_copd + prev_cancer + cesd_nosleep, data = shs_df)
summary(fit_unmatched_sens)

fit_unmatched_mpsd_sens  <- coxph(Surv(dthtt-slpexam1, death) ~ mpsd_di + age5c + race + income_tertile + 
                              degree_attain + married + aehi_10 + smoke + modvig_pa+ 
                                bmi + prev_cvd + prev_copd + prev_cancer + cesd_nosleep, data = shs_df)
summary(fit_unmatched_mpsd_sens )

fit_unmatched_tstsd_sens  <- coxph(Surv(dthtt-slpexam1, death) ~ sdtst_di + age5c + race + income_tertile + 
                               degree_attain + married + aehi_10 + smoke + modvig_pa+ 
                                 bmi + prev_cvd + prev_copd + prev_cancer + cesd_nosleep, data = shs_df)
summary(fit_unmatched_tstsd_sens )

fit_unmatched_tst_sens  <- coxph(Surv(dthtt-slpexam1, death) ~ tst_di + age5c + race + income_tertile + 
                             degree_attain + married + aehi_10 + smoke + modvig_pa+ 
                               bmi + prev_cvd + prev_copd + prev_cancer + cesd_nosleep, data = shs_df)
summary(fit_unmatched_tst_sens )

















  # Model on matched data
fit_matched_withcov <- coxph(Surv(dthtt-slpexam1, death) ~ cluster_f, data = m.data1,
                             weights = weights, cluster = subclass)
summary(fit_matched_withcov)
ShowRegTable(fit_matched_withcov)

fit_matched_withcov_ks <- coxph(Surv(dthtt-slpexam1, death) ~ cluster_f, data = m.data1_ks,
                             weights = weights, cluster = subclass)
summary(fit_matched_withcov_ks)
ShowRegTable(fit_matched_withcov_ks)

# Adjusting for PScore
fit_matched_withPScore <- coxph(Surv(dthtt-slpexam1, death) ~ cluster_f + distance, data = m.data1,
                                weights = weights, cluster = subclass)
summary(fit_matched_withPScore)

# Model on matched data not including covariates used in PScore
fit_matched_withoutcov <- coxph(Surv(dthtt-slpexam1, death) ~ cluster_f, data = m.data1, robust = TRUE,
                                weights = weights, cluster = subclass)
summary(fit_matched_withoutcov)

# Model on matched data including PScore as covariate
fit_matched_with_pscore <- coxph(Surv(dthtt-slpexam1, death) ~ cluster_f + distance, data = m.data1, robust = TRUE,
                                 weights = weights, cluster = subclass)
summary(fit_matched_with_pscore)

# Model on iptw
fit_iptw <- coxph(Surv(dthtt-slpexam1, death) ~ cluster_f, data = m.data1,
                       weights = iptw, model = T)
summary(fit_iptw)

# Model on bmi/ahi matched data including covariates used in PScore
fit_matched_withcov <- coxph(Surv(dthtt-slpexam1, death) ~ cluster_f + age5c + race + income_tertile + 
                               degree_attain + married + aehi_10 + smoke + modvig_pa, data = m.data1_ahi_bmi,
                             weights = weights, cluster = subclass)
summary(fit_matched_withcov)

# kitchen sink pscore model
fit_matched_ks <- coxph(Surv(dthtt-slpexam1, death) ~ cluster_f, 
                                data = m.data1_ks,
                                weights = weights, cluster = subclass)
summary(fit_matched_ks)


# Generate IPTW weights
m.data1_ks$iptw <- ifelse(m.data1_ks$cluster_f == "Favorable sleep",
                       1/m.data1_ks$distance,
                       1/(1-m.data1_ks$distance))
favstats(m.data1_ks$iptw)

# kitchen sink pscore model
fit_matched_ks_iptw <- coxph(Surv(dthtt-slpexam1, death) ~ cluster_f, 
                        data = m.data1_ks,
                        weights = iptw)
summary(fit_matched_ks_iptw)


 
## Test bootstrap CIs
library(boot)
boot_est <- boot(pair_ids, est_fun, R = 499)
boot_est
boot.ci(boot_est, type = "bca")

## Bootstrap function
boot_fun <- function(data, formula, distr){
  m_boot <- survreg(formula, data = data, dist=distr)
  return(exp(coef(m_boot)))
}

boot_res <- censboot(data = m.data1_ks,
                     boot_fun, R = 999,
                     formula = Surv(dthtt-slpexam1, death) ~ cluster_f + age5c + race + income_tertile + 
                       degree_attain + married + aehi_10 + smoke + modvig_pa,
                     distr = "loglogistic"
)

summary(boot_res)
