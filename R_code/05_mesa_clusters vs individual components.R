## Regularity vs clusters for mortality

sueno_df$mpsd_di_rc <- 1 - sueno_df$mpsd_di
sueno_df$sdtst_di_rc <- 1 - sueno_df$sdtst_di
sueno_df$tst_di_rc <- 1 - sueno_df$tst_di

## Midpoint irregularity: <30 min sd
mpsd_model <- coxph(Surv(dthtt-slpexam1, death) ~ mpsd_di + age5c + female + race + 
                       modvig_pa + smoke + income_tertile + 
                       degree_attain + married + aehi_10, data = sueno_df)

mpsd_rc_model <- coxph(Surv(dthtt-slpexam1, death) ~ mpsd_di_rc + age5c + female + race + 
                      modvig_pa + smoke + income_tertile + 
                      degree_attain + married + aehi_10, data = sueno_df)

summary(mpsd_model)
ShowRegTable(mpsd_model)
ShowRegTable(mpsd_rc_model)

## Duration irregularity: <60 min sd
sdtst_model <- coxph(Surv(dthtt-slpexam1, death) ~ sdtst_di + age5c + female + race + 
                     modvig_pa + smoke + income_tertile + 
                     degree_attain + married + aehi_10, data = sueno_df)

sdtst_rc_model <- coxph(Surv(dthtt-slpexam1, death) ~ sdtst_di_rc + age5c + female + race + 
                       modvig_pa + smoke + income_tertile + 
                       degree_attain + married + aehi_10, data = sueno_df)


summary(sdtst_model)
ShowRegTable(sdtst_model)
ShowRegTable(sdtst_rc_model)

## Total sleep time: 6-8 hours
tst_model <- coxph(Surv(dthtt-slpexam1, death) ~ tst_di + age5c + female + race + 
                     modvig_pa + smoke + income_tertile + 
                     degree_attain + married + aehi_10, data = sueno_df)

tst_di_model <- coxph(Surv(dthtt-slpexam1, death) ~ tst_di_rc + age5c + female + race + 
                     modvig_pa + smoke + income_tertile + 
                     degree_attain + married + aehi_10, data = sueno_df)


summary(tst_model)
ShowRegTable(tst_model)
ShowRegTable(tst_di_model)

# cluster
sueno_df$cluster_num_rc <- 1 - sueno_df$cluster_num
cluster_mod <- coxph(Surv(dthtt-slpexam1, death) ~ cluster_f + age5c + female + race + 
                       modvig_pa + smoke + income_tertile + 
                       degree_attain + married + aehi_10, data = sueno_df)

cluster_rc_mod <- coxph(Surv(dthtt-slpexam1, death) ~ cluster_num_rc + age5c + female + race + 
                       modvig_pa + smoke + income_tertile + 
                       degree_attain + married + aehi_10, data = sueno_df)


summary(cluster_mod)
ShowRegTable(cluster_mod)
ShowRegTable(cluster_rc_mod)

cluster_model <- cluster_mod

## Comparison #### 
  
# Midpoint SD <30min          HR: 0.64 [0.44, 0.94] 
# Duration SD <60 min         HR: 0.61 [0.44, 0.84]
# Total sleep time 6-8 hours  HR: 0.73 [0.54, 0.99]
# Cluster un/favorable        HR: 0.52 [0.38, 0.72] *lowest HR

## Clusters vs individual metrics

# Total sleep time by cluster
tst_plot <- ggplot(sueno_df, aes(x = tst_hr, fill = as.factor(cluster_f))) + 
  geom_density(alpha = 0.3) +
  theme_bw(base_size = 25) +
  xlab("Total sleep time (hours)") +
  labs(fill = "Sleep cluster")

# Midpoint variability by cluster
mpsd_plot <- ggplot(sueno_df, aes(x = mpsd, fill = as.factor(cluster_f))) + 
  geom_density(alpha = 0.3) +
  theme_bw(base_size = 25) +
  xlab("Midpoint SD (min)") +
  labs(fill = "Sleep cluster")

# Duration consistency by cluster
sdtst_plot <- ggplot(sueno_df, aes(x = sdtst, fill = as.factor(cluster_f))) + 
  geom_density(alpha = 0.3) +
  theme_bw(base_size = 25) +
  xlab("Duration SD (min)") +
  labs(fill = "Sleep cluster")


ggpubr::ggexport(tst_plot, 
                 filename = "tst x cluster plot.png",
                 width = 1000,
                 height = 1000,
                 pointsize = 24) 

ggpubr::ggexport(mpsd_plot, 
                 filename = "mpsd x cluster plot.png",
                 width = 1000,
                 height = 1000,
                 pointsize = 24) 

ggpubr::ggexport(sdtst_plot, 
                 filename = "sdtst x cluster plot.png",
                 width = 1000,
                 height = 500,
                 pointsize = 24) 

## Plot:
library(survival)
library(survminer)

mortality_cluster_plot   

## Loop code
sleep_cluster_vars_di <- c("tst_di", "mpsd_di", "sdtst_di",
                           "cluster_f")

cluster_report_table_di <- c()

for(i in 1:length(sleep_cluster_vars_di)){
  
  adjusted_sleep_formula_di <- paste0("Surv(dthtt-slpexam1, death) ~ ", 
                                      sleep_cluster_vars_di[i], 
                                      " + age5c + female + race + 
                       modvig_pa + smoke + income_tertile + 
                       degree_attain + married + aehi_10")
  
  
  ## UNADJUSTED REGRESSIONS
  
  death_mod_di <- coxph(formula = as.formula(adjusted_sleep_formula_di),
                        data    = sueno_df)
  
  summary(death_mod_di)
  
  # For coefficients
  HR_coefs <- exp(death_mod_di$coefficients)
  
  # For confidence interval
  cc <- as.data.frame(coef(summary(death_mod_di)))
  names(cc) <- c("coef", "hazard_ratio", "se_coef", "z", "p")
  
  cc$LB <- summary(death_mod_di)$conf.int[1,3]
  cc$UB <- summary(death_mod_di)$conf.int[1,4]
  cc$var <- sleep_cluster_vars_di[i]
  
  print(i)
  
  # For p-values
  # pvals <- coef(summary(death_mod_di))[, 5]
  # stars <- c()
  # for(i in 1:length(pvals)){
  #   if(pvals[i] > 0.05) stars[i] <- ""
  #   if(pvals[i] < 0.05 & pvals[i] >=0.01) stars[i] <- "*"
  #   if(pvals[i] <= 0.01 & pvals[i] >=0.001) stars[i] <- "**" 
  #   if(pvals[i] < 0.001) stars[i] <- "***"
  # }
  # 
  # Unite coefs with ci_vector, ci_vector on new line
  
  
  cluster_report_table_di <- rbind(cluster_report_table_di, cc[1,])
  
  # write.csv(report_df, file = paste0(sleep_vars[j], "-RR.csv"))
  
}

cluster_report_table_di


cluster_report_table_di$group <- factor(cluster_report_table_di$var,
                                        levels = c("tst_di", "mpsd_di",
                                                   "sdtst_di", "cluster_f"))

cluster_report_table_di$group <- factor(cluster_report_table_di$group,
                                        labels = c("Duration 6-8 hours", 
                                                   "Midpoint irregularity <30 minutes",
                                                   "Duration irregularity <60 minutes",
                                                   "Un/favorable sleep clusters"))

mortality_cluster_plot <- ggplot(cluster_report_table_di, 
                                 aes(x = reorder(group, -hazard_ratio), 
                                     y = hazard_ratio)) +
  geom_point() + 
  geom_errorbar(aes(ymin = LB, ymax = UB), width = 0.2, size = 0.8) + 
  coord_flip() + 
  theme_classic(base_size = 12) +
  geom_hline(yintercept = 1, linetype = "dashed") + 
  ylab("Hazard Ratios") +
  xlab("Favorable sleep (categorical)")+
  theme(legend.position = "none") +
  # ggtitle(expression(underline(Hazard~Ratio~'(95% CI)'))) +
  theme(plot.title = element_text(angle = 0, hjust=0.5, vjust=-1, size = 10))

mortality_cluster_plot                                        

## End script