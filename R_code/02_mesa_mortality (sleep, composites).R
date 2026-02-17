##################################################################################
##################################################################################
# Sleep variables
#############################################################################################
#############################################################################################
library(hms)
library(factoextra)
library(FactoMineR)

set.seed(8675309)

# From actigraphy
mesa_df$tst <- mesa_df$avgmainsleep5
mesa_df$sme <- mesa_df$avgmainteff5
mesa_df$mpsd_nonhms <- mesa_df$sdsleepmidpoint5
mesa_df$frag <- mesa_df$avgfragmentation5
mesa_df$sdtst <- mesa_df$sdmainsleep5

# From questionnaire
mesa_df$ess <- mesa_df$epslpscl5c
# mesa_df$whi <- mesa_df$whiirs5c # Higher is worse

# From PSG
mesa_df$sws_per <- mesa_df$times34p5
mesa_df$rem_per <- mesa_df$timeremp5
mesa_df$ahi <- mesa_df$oahi3pa5

#############################################################################################
#############################################################################################
# TST: 6-8 hours == favorable
mesa_df$tst_di <- ifelse(mesa_df$tst >= 360 & mesa_df$tst <= 480, 1, 0)

ggplot(mesa_df, aes(x = tst)) + 
  geom_histogram() + 
  theme_bw() +
  geom_vline(xintercept = c(360, 480), linetype = "dashed")

table(mesa_df$tst_di)

# Sleep maintenance efficiency: > 90% == favorable
mesa_df$sme_di <- ifelse(mesa_df$sme > 90, 1, 0)

# Epworth Sleepiness Scale: 5 or 10
mesa_df$ess_di <- ifelse(mesa_df$ess <= 10, 1, 0)

# WHI insomnia scale for quality (RC - higher is better)
# favstats(mesa_df$whi)
# mesa_df$whi_di <- ifelse(mesa_df$whiirs5c < 9, 1, 0)

# Midpoint sd (higher is worse - put in hours)
mesa_df$mpsd_hms <- as.hms(mesa_df$mpsd_nonhms)

# Subtract midnight and calculate in minutes
mesa_df$mpsd_reference <- as.hms(00:00:00)

head(mesa_df$mpsd_hms)
head(mesa_df$mpsd_reference)

head(mesa_df$mpsd_hms - mesa_df$mpsd_reference) # Correct

mesa_df$mpsd <- mesa_df$mpsd_hms - mesa_df$mpsd_reference

head(mesa_df$mpsd)
head(as.numeric(mesa_df$mpsd))

mesa_df$mpsd <- as.numeric(mesa_df$mpsd)
mesa_df$mpsd[mesa_df$mpsd >= 20000] <- NA # Exclude > 20k per Tianyi

mesa_df$mpsd <- mesa_df$mpsd / 60 # Convert to minutes for consistency
mesa_df$mpsd_di <- ifelse(mesa_df$mpsd < 30, 1, 0)

qplot(mesa_df$mpsd) + geom_vline(xintercept = 30)
sum(mesa_df$mpsd < mpsd_q1, na.rm = T)
sum(mesa_df$mpsd < 30, na.rm = T)

sum((mesa_df$mpsd < 30) != (mesa_df$mpsd <= 30), na.rm=T)
which(mesa_df$mpsd == 30)

qplot(log(mesa_df$mpsd))

mesa_df$log_mpsd <- log(mesa_df$mpsd)

## NOTE: MPSD is now in MINUTES

## NEW: 5/19/2020
# TST irregularity: dichotomize at 60, per Tianyi's paper
mesa_df$sdtst_di <- ifelse(mesa_df$sdmainsleep5 < 60, 1, 0)
qplot(mesa_df$sdtst)

# Fragmentation
frag_q1 <- favstats(mesa_df$frag)$Q1
mesa_df$frag_di <- ifelse(mesa_df$frag <= 15, 1, 0)

# SWS percentage (use NSF guidelines)
# sws_q3 <- favstats(mesa_df$sws_per)$Q3
mesa_df$swsper_di <- ifelse(mesa_df$times34p5 >= 16 & mesa_df$times34p5 <= 20, 1, 0)

# REM percentage (use NSF guidelines)
# remper_q3 <- favstats(mesa_df$rem_per)$Q3
mesa_df$remper_di <- ifelse(mesa_df$timeremp5 >= 21 & mesa_df$timeremp5 <= 30, 1, 0)

# AHI
mesa_df$ahi_di <- ifelse(mesa_df$ahi <= 15, 1, 0)

# NSF age ranges
# adult (26-64 years), and older adult (???65 years).
# to 65: 0-20
# 65+  : 0-30

# WASO waso5: PSG WASO
mesa_df$waso <- mesa_df$waso5
mesa_df$waso_di <- 0
mesa_df$waso_di <- ifelse(mesa_df$waso<60, 1, 0)

# Sleep onset latency avgonsetlatency5
mesa_df$sol <- mesa_df$avgonsetlatency5
mesa_df$sol_act_di <- ifelse(mesa_df$sol < 30, 1, 0)
table(mesa_df$sol_act_di)

## Consider difficulties initiating sleep as SOL
mesa_df$sol_sub <- as.numeric(mesa_df$trbleslpng5)
qplot(mesa_df$sol_sub)

mesa_df$sol_di <- ifelse(mesa_df$trbleslpng5 == "1: NO, NOT IN THE PAST 4 WEEKS" |
                           mesa_df$trbleslpng5 == "2: YES, LESS THAN ONCE A WEEK", 1, 0)

# typicalslp5	PAST 4 WEEKS: OVERALL TYPICAL NIGHT SLEEP	0: VERY SOUND OR RESTFUL
# 1: SOUND AND RESTFUL
# 2: AVERAGE QUALITY
# 3: RESTLESS
# 4: VERY RESTLESS
mesa_df$quality <- as.numeric(mesa_df$typicalslp5)
mesa_df$quality_di <- ifelse(mesa_df$typicalslp5 == "0: VERY SOUND OR RESTFUL" |
                               mesa_df$typicalslp5 == "1: SOUND AND RESTFUL", 1, 0)
qplot(mesa_df$quality)
qplot(mesa_df$quality_di)


##  1) code timing again or find code.
##  2) rerun PCA. send to Chandra.
##  3) Run loop. The following all goes within the loop:

## Within the loop:
# 3a) sample with replacement
# 3b) run pca
# 3c) nested models
# 3d) predicted values (empirical CI)

## PCA: bring code in from previous code (cnp). Estimate on ALL of MESA with timing. Subset to 1,736.
## nested models: run each model 0-6 (cnp). 
##                store BETAS for race in a df (colnames = iteration, black, asian, hispanic, r2, p). 
##                        ROWS: each iteration (rownames = i)
## RETURN DATAFRAME

#############################################################################################
#############################################################################################
## TIMING CODE
##  1) Make into a "circular" variable: values > 1440 minutes are recoded to start again from 0
##  2) dichotomize at 2am-4am = 1

## avgsleepmidpoint5
qplot(mesa_df$avgsleepmidpoint5)
head(as.hms(mesa_df$avgsleepmidpoint5))

mesa_df$midpoint_hm_s <- as.hms(mesa_df$avgsleepmidpoint5)
head(mesa_df$midpoint_hm_s)

# Subtract midnight and calculate in minutes
mesa_df$timing_reference <- as.hms(00:00:00)

head(mesa_df$midpoint_hm_s)
head(mesa_df$timing_reference)

head(mesa_df$midpoint_hm_s - mesa_df$timing_reference) # Correct

mesa_df$timing <- mesa_df$midpoint_hm_s - mesa_df$timing_reference

head(mesa_df$timing)
head(as.numeric(mesa_df$timing))

mesa_df$timing <- as.numeric(mesa_df$timing) / 60

qplot(mesa_df$timing) + geom_vline(xintercept = 1440, linetype = "dashed")
sum(mesa_df$timing >1440, na.rm = T)
favstats(mesa_df$timing)


mesa_df$timing_m <- ifelse(mesa_df$timing < 720, mesa_df$timing, mesa_df$timing-1440)
mesa_df$timing_m_c <- abs(mesa_df$timing_m - median(mesa_df$timing_m, na.rm=T))

qplot(mesa_df$timing_m) + geom_vline(xintercept = c(120, 240), linetype = "dashed")
sum(is.na(mesa_df$timing_m))

mesa_df$log_timing <- log(mesa_df$timing_m_c+1)

## PCA
#############################################################################################
#############################################################################################
## SHS-PC1 
pr_df <- mesa_df %>% dplyr::select(tst, sme, log_mpsd, ess, 
                                   quality, frag, sws_per, rem_per, ahi, waso, 
                                   sol, sdtst, log_timing, idno) %>%
  mutate(quality = as.numeric(quality)) %>% na.omit()
dim(pr_df)

# idno must be last in vector
pca_res <- prcomp(scale(pr_df[,1:13]*-1)) # *-1 so that higher = better
summary(pca_res)
plot(pca_res)



fviz_eig(pca_res)

fviz_pca_var(pca_res,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

str(pca_res)

# Eigenvalues
eig.val <- get_eigenvalue(pca_res)
eig.val

# Results for Variables
res.var <- get_pca_var(pca_res)
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation 

res.ind <- get_pca_ind(pca_res)
res.ind$coord          # Coordinates
res.ind$contrib        # Contributions to the PCs
res.ind$cos2           # Quality of representation 

pca_df <- as.data.frame(res.ind$coord)

pca_df <- cbind(pca_df, pr_df)

# var_df <- as.data.frame(res.var$coord)
# head(var_df)
# write.csv(var_df$Dim.1, file = "Dim1.csv")


pc1_df <- pca_df %>% dplyr::mutate(pc1_scaled = Dim.1) %>%
  dplyr::select(pc1_scaled, idno)

mesa_df <- merge(mesa_df, pc1_df, by = "idno")

mesa_df$pc1_scaled <- scale(mesa_df$pc1_scaled, scale = T, center = T)*-1
favstats(mesa_df$pc1_scaled ~ mesa_df$race)
boxplot(mesa_df$pc1_scaled ~ mesa_df$race)

## SHS
# timing_di <- 2 am to 4 am = 1
mesa_df$timing_di <- ifelse(mesa_df$timing_m >= 120 & mesa_df$timing_m <= 240, 1, 0)

mesa_df$shs <- with(mesa_df, tst_di + sme_di + ess_di + quality_di + mpsd_di + frag_di + swsper_di + remper_di + ahi_di +
                      waso_di + sol_di + timing_di + sdtst_di)
favstats(mesa_df$shs)
shapiro.test(mesa_df$shs)
qplot(mesa_df$shs, binwidth = 1)

summary(aov_mod_shs <- aov(mesa_df$shs ~ mesa_df$race))
TukeyHSD(aov_mod_shs)

# Regularity and duration clusters

mesa_cluster_df <- mesa_df %>% dplyr::select(tst, log_mpsd, sdtst, idno) %>%
  na.omit() # idno must be last parameter

no_params <- dim(mesa_cluster_df)[2] - 1 # idno must be last parameter

## Cluster validation
# For 1 to 15 cluster centers
# wss <- c()
# for (i in 1:15) {
#   km.out <- kmeans(scale(mesa_cluster_df[,1:no_params]), centers = i, nstart=20, iter.max = 25)
#   # Save total within sum of squares to wss variable
#   wss[i] <- km.out$tot.withinss
# }
# 
# # Plot total within sum of squares vs. number of clusters
# plot(1:15, wss, type = "b",
#      xlab = "Number of Clusters",
#      ylab = "Within groups sum of squares", main = "Sleep Health Cluster")
# 
# x <- 1:15
# y = wss
# data <- as.data.frame(cbind(x, y))
# wss_plot <- ggplot(data = data, aes(x = x, y = wss)) + geom_line(size = 1.5) + geom_point(size = 4) #+ geom_vline(xintercept = 3,
# #linetype = "dashed")
# wss_sparcl <- wss_plot +
#   xlab("Number of clusters") +
#   ylab("Within groups sum of squares") +
#   theme_minimal(base_size = 25) +
#   scale_x_continuous(breaks = c(1:15)) + 
#   geom_vline(xintercept = 2, linetype="dashed")
# 
# ragg::agg_tiff("MESA mortality within-sum-of-squares.tiff", width = 10, height = 10, units = "in", res = 300)
# wss_sparcl
# dev.off()
# 
# 
library(cluster)
km_gaps_1 <- clusGap(scale(mesa_cluster_df[, 1:no_params], center = T, scale = T),
                     FUN = kmeans,
                     nstart = 20,
                     K.max = 15,
                     B = 60,
                     iter.max = 100)

km_gaps_1_plot <- plot(km_gaps_1, main = "Gap statistic (d.power = 2)")

# ragg::agg_tiff("MESA mortality GAP statistic.tiff", width = 10, height = 10, units = "in", res = 300)
# plot(km_gaps_1, main = "Gap statistic (d.power = 2)")
# dev.off()
# 
# with(km_gaps_1, maxSE(Tab[,"gap"], Tab[,"SE.sim"], method = "Tibs2001SEmax"))
# 
# NBClust
library(NbClust)
SHI_nbclust <- NbClust(data = scale(mesa_cluster_df[,1:no_params]), method = "kmeans",
                       index = "all")
str(SHI_nbclust)
nbclust_numclust <- as.data.frame(t(SHI_nbclust$Best.nc))
options("scipen" = 100, digits = 4)

nbclust_numclust <- nbclust_numclust %>% select(-Value_Index)
nbclust_table_plot <- grid.table(nbclust_numclust)

nbclust_mat <- print(nbclust_numclust, quote = FALSE)

ggplot(nbclust_numclust, aes(x = Number_clusters, y = ..count..)) +
  theme_classic(base_size = 15) +
  geom_dotplot() +
  xlab("k") +
  ylab("Index count for best k") +
  scale_x_continuous(breaks = c(0:15)) +
  scale_y_continuous()
#
#
# library(cstab)
# ## Stability
stability <- cStability(scale(mesa_cluster_df[,1:no_params]), kseq = 2:10, nB = 1000, method = "kmeans",
                        predict = 1,
                        norm = T)

plot(stability, main = "Stability")

k <- 2:10
stability_plot <- ggplot(,aes(k, stability$instab_path_norm)) + geom_line() +
  xlab("k")+
  ylab("Normalized instability") + theme_minimal(base_size = 10) +
  geom_vline(xintercept = 2, linetype = "dashed") +
  scale_x_continuous(breaks = c(0:10)) +
  #ggtitle("Stability by k. N = 385") +
  geom_point(size = 2) +
  theme_bw(base_size = 40)

ggpubr::ggexport(stability_plot,
                 filename = "stability_plot.png",
                 width = 1000,
                 height = 1000,
                 pointsize = 24)


# ragg::agg_tiff("MESA mortality STABILITY statistic.tiff", width = 10, height = 10, units = "in", res = 300)
# stability_plot
# dev.off()
# 
# # Ideal k = 2
# 
# library(fpc)
# scale(mesa_cluster_df[, 1:no_params], center = T, scale = T)

# clusboot_res2 <- clusterboot(data = scale(mesa_cluster_df[, 1:no_params], center = T, scale = T),
#                              bootmethod = c("boot", "subset"),
#                              clustermethod = kmeansCBI,
#                              krange = 2)
# clusboot_res3 <- clusterboot(data = scale(mesa_cluster_df[, 1:no_params], center = T, scale = T),
#                              bootmethod = c("boot", "subset"),
#                              clustermethod = kmeansCBI,
#                              krange = 3)
#
# clusboot_res2
# clusboot_res3
# 
# df_jaccard <- data.frame(k = c(2, 2, 3, 3, 3),
#                          cluster = c(1, 2, 1, 2, 3))
# 
# df_jaccard$bootres <- c(clusboot_res2$bootmean, clusboot_res3$bootmean)
# 
# df_jaccard$subsetres <- c(clusboot_res2$subsetmean, clusboot_res3$subsetmean)
# 
# df_jaccard$bootrecover <- c(clusboot_res2$bootrecover, clusboot_res3$bootrecover)
# 
# df_jaccard$subsetrecover <- c(clusboot_res2$subsetrecover, clusboot_res3$subsetrecover)
# 
# df_jaccard$bootdissolved <- c(clusboot_res2$bootbrd, clusboot_res3$bootbrd)
# 
# df_jaccard$subsetdissolved <- c(clusboot_res2$subsetbrd, clusboot_res3$subsetbrd)
# 
# write.csv(df_jaccard, file = "df_jaccard.csv")
# 
# ggplot(df_jaccard, aes(x = as.factor(k), y = bootrecover)) +
#   geom_bar(stat = "identity", position = position_dodge()) +
#   theme_bw() +
#   ylab("Mean Jaccard, bootstrap") +
#   xlab("K")

## Sparse K-means
library(sparcl)

sparse_km.perm <- KMeansSparseCluster.permute(scale(mesa_cluster_df[,1:no_params]), K = 2,
                                              wbounds = seq(2, 7, len = 15),
                                              nperms = 250)

km.out <- KMeansSparseCluster(scale(mesa_cluster_df[,1:no_params]), K = 2, wbounds = sparse_km.perm$bestw)

## FIX THIS CODE
cluster_vector <- km.out[[1]]$Cs
length(cluster_vector)

mesa_df$cluster_rand <- cluster_vector
table(mesa_df$cluster_rand)

favstats(mesa_df$tst ~ mesa_df$cluster_rand) # 1 = better, 2 = worse
favstats(mesa_df$mpsd ~ mesa_df$cluster_rand)# 1 = better, 2 = worse

mesa_df <- mesa_df %>% 
  dplyr::mutate(cluster_num = 2 - cluster_rand) %>%
  dplyr::mutate(cluster_f = factor(cluster_num, labels = c("Irregular-insufficient",
                                                           "Regular-optimal"))) 
table(mesa_df$cluster_f)
# Unfavorable sleep   Favorable sleep 
# 783                 1046 


## Events
## Events code

## Joon Chung
## jchung26@bwh.harvard.edu

# From Tianyi:
# slpexam5=stdyady5c; /*days of actigraphy relative to exam 5*/
#   if slpexam5=. then slpexam5=stdypdy5c; /*days of psg relative to exam 5*/
#     slpexam1=slpexam5+e15dyc; /*days of sleep study relative to exam 1*/
#       

library(haven)
mesa_events <- read_dta("C:/Users/jj261/Dropbox (Partners HealthCare)/MESAEvThru2018_20210518/MESAEvThru2018_20210518.dta")

mesa_df$slpexam5 <- mesa_df$stdyady5c
which(is.na(mesa_df$slpexam5))

mesa_df$slpexam5[which(is.na(mesa_df$slpexam5))] <- mesa_df$stdypdy5c[which(is.na(mesa_df$slpexam5))]
mesa_df$slpexam1 <- mesa_df$slpexam5 + mesa_df$e15dyc # Days of sleep study relative to Exam 1
# 
# ## Merge mesa_df with mesa_events
# mortality_df <- merge(mesa_df, mesa_events, by = "idno")

## Non cvd events (cancer, copd)
MESANonCVDEvThru2018_20210517 <- read_dta("C:/Users/jj261/Dropbox (Partners HealthCare)/MESANonCVDEvThru2018_20210517/MESANonCVDEvThru2018_20210517.dta")
mesa_noncvd_df <- MESANonCVDEvThru2018_20210517

mesa_cvd_events <- mesa_events %>% dplyr::select(cvda, cvdatt, idno, dth, dthtt)
mesa_noncvd_events <- mesa_noncvd_df %>% dplyr::select(cancer, cancertt, copd, copdtt, idno)
mesa_all_events <- merge(mesa_cvd_events, mesa_noncvd_events, by = "idno")

## Back to cluster dropbox
setwd("C:/Users/jj261/Dropbox (Partners HealthCare)/2023 sleep_clusters_MESA")
## End script
