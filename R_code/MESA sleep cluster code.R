## Joon Chung
## Contact: see README
## 01/19/2022

# Regularity and duration clusters

mesa_cluster_df <- mesa_df %>% dplyr::select(tst, log_mpsd, sdtst, idno) %>%
  na.omit() # idno must be last parameter

no_params <- dim(mesa_cluster_df)[2] - 1 # idno must be last parameter

## Cluster validation
# For 1 to 15 cluster centers
wss <- c()
for (i in 1:15) {
  km.out <- kmeans(scale(mesa_cluster_df[,1:no_params]), centers = i, nstart=20, iter.max = 25)
  # Save total within sum of squares to wss variable
  wss[i] <- km.out$tot.withinss
}

# Plot total within sum of squares vs. number of clusters
plot(1:15, wss, type = "b",
     xlab = "Number of Clusters",
     ylab = "Within groups sum of squares", main = "Sleep Health Cluster")

x <- 1:15
y = wss
data <- as.data.frame(cbind(x, y))
wss_plot <- ggplot(data = data, aes(x = x, y = wss)) + geom_line(size = 1.5) + geom_point(size = 4) #+ geom_vline(xintercept = 3,
#linetype = "dashed")
wss_sparcl <- wss_plot +
  xlab("Number of clusters") +
  ylab("Within groups sum of squares") +
  theme_minimal(base_size = 25) +
  scale_x_continuous(breaks = c(1:15)) + 
  geom_vline(xintercept = 2, linetype="dashed")

ragg::agg_tiff("MESA mortality within-sum-of-squares.tiff", width = 10, height = 10, units = "in", res = 300)
wss_sparcl
dev.off()


library(cluster)
km_gaps_1 <- clusGap(scale(mesa_cluster_df[, 1:no_params], center = T, scale = T),
                     FUN = kmeans,
                     nstart = 20,
                     K.max = 15,
                     B = 60,
                     iter.max = 100)

km_gaps_1_plot <- plot(km_gaps_1, main = "Gap statistic (d.power = 2)")

ragg::agg_tiff("MESA mortality GAP statistic.tiff", width = 10, height = 10, units = "in", res = 300)
km_gaps_1_plot
dev.off()

with(km_gaps_1, maxSE(Tab[,"gap"], Tab[,"SE.sim"], method = "Tibs2001SEmax"))

# NBClust
library(NbClust)
SHI_nbclust <- NbClust(data = scale(mesa_cluster_df[,1:no_params]), method = "kmeans")
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


library(cstab)
## Stability
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
  geom_point(size = 2)

ragg::agg_tiff("MESA mortality STABILITY statistic.tiff", width = 10, height = 10, units = "in", res = 300)
stability_plot
dev.off()

# Ideal k = 2

library(fpc)
scale(mesa_cluster_df[, 1:no_params], center = T, scale = T)

clusboot_res2 <- clusterboot(data = scale(mesa_cluster_df[, 1:no_params], center = T, scale = T),
                             bootmethod = c("boot", "subset"),
                             clustermethod = kmeansCBI,
                             krange = 2)
clusboot_res3 <- clusterboot(data = scale(mesa_cluster_df[, 1:no_params], center = T, scale = T),
                             bootmethod = c("boot", "subset"),
                             clustermethod = kmeansCBI,
                             krange = 3)

clusboot_res2
clusboot_res3

df_jaccard <- data.frame(k = c(2, 2, 3, 3, 3),
                         cluster = c(1, 2, 1, 2, 3))

df_jaccard$bootres <- c(clusboot_res2$bootmean, clusboot_res3$bootmean)

df_jaccard$subsetres <- c(clusboot_res2$subsetmean, clusboot_res3$subsetmean)

df_jaccard$bootrecover <- c(clusboot_res2$bootrecover, clusboot_res3$bootrecover)

df_jaccard$subsetrecover <- c(clusboot_res2$subsetrecover, clusboot_res3$subsetrecover)

df_jaccard$bootdissolved <- c(clusboot_res2$bootbrd, clusboot_res3$bootbrd)

df_jaccard$subsetdissolved <- c(clusboot_res2$subsetbrd, clusboot_res3$subsetbrd)

write.csv(df_jaccard, file = "df_jaccard.csv")

ggplot(df_jaccard, aes(x = as.factor(k), y = bootrecover)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_bw() +
  ylab("Mean Jaccard, bootstrap") +
  xlab("K")

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
  dplyr::mutate(cluster_f = factor(cluster_num, labels = c("Unfavorable sleep",
                                   "Favorable sleep"))) 
table(mesa_df$cluster_f)
# Unfavorable sleep   Favorable sleep 
# 783                 1046 

cluster_vector <- mesa_df %>% dplyr::select(cluster_f, cluster_num, idno)
# shs_df <- merge(shs_df, cluster_vector, by = "idno")
