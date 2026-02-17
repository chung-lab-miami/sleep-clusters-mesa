## Cluster plot
library(survival)
library(survminer)

clusfit <- survfit(Surv((dthtt/365- slpexam1/365), death) ~ cluster_f, data = cluster_df)
clusfit


cluster_survplot <- ggsurvplot(clusfit, data = cluster_df,
                               legend.title = "Sleep cluster",
                               legend.labs = c("Irregular-insufficient", "Regular-optimal"),
                               xlab = "Time (years)",
                               xlim = c(0, 7),
                               ylim = c(0.8, 1),
                               conf.int = T,
                               risk.table= T,
                               ggtheme = theme_minimal(base_size = 25))

ggpubr::ggexport(cluster_survplot, 
                 filename = "cluster_survplot.png",
                 width = 1000,
                 height = 1400,
                 pointsize = 24) 

## mpsd plot

mpsdfit <- survfit(Surv((dthtt/365- slpexam1/365), death) ~ mpsd_di, data = cluster_df)
mpsdfit


mpsd_survplot <- ggsurvplot(mpsdfit, data = cluster_df,
                               legend.title = "Regular timing",
                               legend.labs = c("SD of Midpoint >=30", "SD of Midpoint <30"),
                               xlab = "Time (years)",
                               ylim = c(0.8, 1),
                               conf.int = T,
                            risk.table= T,
                            ggtheme = theme_minimal(base_size = 25))

ggpubr::ggexport(mpsd_survplot, 
                 filename = "mpsd_survplot.png",
                 width = 1000,
                 height = 1000,
                 pointsize = 24) 


## SD duration plot

sdtstfit <- survfit(Surv((dthtt/365- slpexam1/365), death) ~ sdtst_di, data = cluster_df)
sdtstfit


sdtst_survplot <- ggsurvplot(sdtstfit, data = cluster_df,
                               legend.title = "Consistent duration",
                               legend.labs = c("SD of duration >= 60 min", "SD of duration <60 min"),
                               xlab = "Time (years)",
                               ylim = c(0.8, 1),
                               conf.int = T,
                             risk.table= T,
                             ggtheme = theme_minimal(base_size = 25))

ggpubr::ggexport(sdtst_survplot, 
                 filename = "sdtst_survplot.png",
                 width = 1000,
                 height = 1000,
                 pointsize = 24) 


## TST plot

tstfit <- survfit(Surv((dthtt/365- slpexam1/365), death) ~ tst_di, data = cluster_df)
tstfit


tst_survplot <- ggsurvplot(tstfit, data = cluster_df,
                               legend.title = "Optimal sleep duration (6-8 hours)",
                               legend.labs = c("Unfavorable duration", "Favorable duration (6-8 hrs)"),
                               xlab = "Time (years)",
                               ylim = c(0.8, 1),
                               conf.int = T,
                           risk.table= T,
                           ggtheme = theme_minimal(base_size = 25))

ggpubr::ggexport(tst_survplot, 
                 filename = "tst_survplot.png",
                 width = 1000,
                 height = 1000,
                 pointsize = 24) 

###################################################################
## 5/3/2023 - adjusted curves


model_4_yr <- coxph(Surv(dthtt/365 -slpexam1/365, death) ~ cluster_f +  
                   age5c + female + race + 
                   modvig_pa + smoke +  married + 
                   work_sched + totmed5 + 
                   bmi + cesd_nosleep + prev_cvd + 
                   ahi + whiirs5c + rstlesslgs5, data = cluster_df)

ShowRegTable(model_4_yr)

cluster_df$cluster_f <- factor(cluster_df$cluster_f, labels = c("Irregular-insufficient", "Regular-optimal"))

adjust_curves <- ggadjustedcurves(model_4_yr, data = cluster_df,
                               legend.title = "Regular and sufficient sleep",
                               legend.labs = c("Irregular-insufficient", "Regular-optimal"),
                               xlab = "Time (years)",
                               ylim = c(0.85, 1),
                               conf.int = T,
                               risk.table= T,
                               ggtheme = theme_minimal(base_size = 25),
                 method = "conditional", variable = "cluster_f")


ggpubr::ggexport(adjust_curves, 
                 filename = "adjusted_cluster_conditional_KMsurvplot.png",
                 width = 1000,
                 height = 1000,
                 pointsize = 24) 
