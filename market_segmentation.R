library(tidyverse)
library(dplyr)
library(readxl)
library(gplots)
library(factoextra)
library(cluster)
library(clusterSim)
library(dbscan)
library(rpart)

#Import data
bsData <- read_excel("bathSoapData.xls")

#Analysis 1 - Purchase Behavior

names(bsData) <- gsub("[[:punct:]]|\\s", "_", names(bsData))
colnames(bsData)
str(bsData)
bsd<- bsData
bsd <- bsd %>% rowwise() %>%  mutate(maxBr=max(Br__Cd__57__144, Br__Cd__55, Br__Cd__272, Br__Cd__286, Br__Cd__24, Br__Cd__481, Br__Cd__352, Br__Cd__5))
bsd <- bsd %>% select(-c(Br__Cd__57__144, Br__Cd__55, Br__Cd__272, Br__Cd__286, Br__Cd__24, Br__Cd__481, Br__Cd__352, Br__Cd__5))
bsd$Member_id <- as.factor(bsd$Member_id)
bsd$FEH <- as.factor(bsd$FEH)
bsd$SEX <- as.factor(bsd$SEX)
bsd$CHILD <- as.factor(bsd$CHILD)
bsd$CS <- as.factor(bsd$CS)
bsd <-bsd %>% mutate(fehDummy=1) %>% pivot_wider(names_from = FEH, values_from = fehDummy, names_prefix = "FEH_", values_fill = list(fehDummy=0)) %>% dplyr::select(-FEH_0)

summary(bsd$MT)

bsd <- bsd %>% mutate(MT=if_else(MT %in% c(0, 4, 5, 10, 17),MT,-1))
bsd$MT <- as.factor(bsd$MT)
bsd <-bsd %>% mutate(mtDummy=1) %>% pivot_wider(names_from = MT, values_from = mtDummy, names_prefix = "MT_", values_fill = list(mtDummy=0)) 
bsd<- bsd %>% dplyr::select(- `MT_-1`)
bsd <- bsd %>% mutate(mtChild=1) %>% pivot_wider(names_from = CHILD, values_from = mtChild, names_prefix = "CHILD_", values_fill = list(mtChild=0)) %>% dplyr::select(- CHILD_5) 
bsd <- bsd %>% mutate(sexdummy=1) %>% pivot_wider(names_from = SEX, values_from = sexdummy, names_prefix = "SEX_", values_fill = list(sexdummy=0)) %>% dplyr::select(- SEX_0) 
bsd <- bsd %>% mutate(csdummy=1) %>% pivot_wider(names_from = CS, values_from = csdummy, names_prefix = "CS_", values_fill = list(csdummy=0)) %>% dplyr::select(- CS_2)

PURCHASE_BEHAVIOR <- c('No__of_Brands', 'Brand_Runs', 'Total_Volume', 'No__of__Trans','Vol_Tran','Value', 'Trans___Brand_Runs', 'Avg__Price', 'maxBr', 'Others_999')

x_purchase_behavior <- bsd
xpb <- x_purchase_behavior %>% dplyr::select(PURCHASE_BEHAVIOR) %>% scale() 
xpb <- as.data.frame(xpb)
xpb$brnd_lylty <- xpb$maxBr-xpb$Others_999+xpb$Trans___Brand_Runs-xpb$No__of_Brands
xpb <- xpb %>% select(-c(maxBr,Others_999,Trans___Brand_Runs,No__of_Brands))
xpb <- xpb %>% select(-c(Vol_Tran,Value))
xpb <- xpb %>% select(-c(Brand_Runs))

x_purchase_behavior <- x_purchase_behavior %>% select(-c(maxBr,Others_999,Trans___Brand_Runs,No__of_Brands,Vol_Tran,Value,Brand_Runs))
x_purchase_behavior$brnd_lylty <- xpb$brnd_lylty

#K-Means 
kmClus_pb<- xpb%>%kmeans(centers=3, nstart=20)
kmClus_pb$tot.withinss

#Ideal clusters
kmClus_pb$tot.withinss/kmClus_pb$betweenss
fviz_nbclust(xpb, kmeans, method = "wss")
fviz_nbclust(xpb, kmeans, method = "silhouette")
dissimilar <- daisy(xpb)         
sil <- silhouette(kmClus_pb$cluster, dissimilar)
fviz_silhouette(sil)

#Visualize clusters
fviz_cluster(kmClus_pb, data=xpb)

centres_km <- t(kmClus_pb$centers)
centres_km <- (centres_km - min(centres_km))/(max(centres_km)-min(centres_km))
matplot(y = centres_km, type = 'l', lty = 1, col = 1:5, cex = 1, xlab = "Purchase behavior", ylab = "mean", xaxt="n")
legend(legend = colnames(centres_km), x = "topright", y = "topright", lty = 1, lwd = 2, col = 1:5)
angleAxis(1, labels = c('Total_Volume', 'No__of__Trans', 'Avg__Price', 'brnd_lylty'), at = 1:9, srt = 90, xpd = TRUE, cex = 0.7)

#Analysis w/ demographics
x_purchase_behavior <- x_purchase_behavior %>% mutate(clusKM=kmClus_pb$cluster)
x_purchase_behavior %>% group_by(clusKM) %>% summarise_at(c('SEC', 'HS', 'SEX_1','SEX_2', 'EDU', 'Affluence_Index','AGE', 'CHILD_1', 'CHILD_2', 'CHILD_3', 'CHILD_4', 'Total_Volume', 'No__of__Trans', 'Avg__Price','brnd_lylty'), mean) %>% view()

#Hierarchical 
xdist <- dist(xpb ,method = "euclidean")
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

ac <- function(x) {
  agnes(xdist, method = x)$ac
}
map_dbl(m, ac)

xdist <- get_dist(xpb ,method = "pearson")
map_dbl(m, ac)

hierC_pb_ag_w <- agnes(xdist, method = "ward" )

fviz_nbclust(xpb, FUN = hcut, method = "wss", 
             k.max = 10) +
  ggtitle("Elbow method")
fviz_nbclust(xpb, FUN = hcut, method = "silhouette", 
             k.max = 10) +
  ggtitle("Silhouette method")

dissimilar <- daisy(xpb)         
sil <- silhouette(cut_hierC_pb_ag_w, dissimilar)
fviz_silhouette(sil)

#Ideal clusters
cut_hierC_pb_ag_w <- cutree(hierC_pb_ag_w, k = 3)#3
table(cut_hierC_pb_ag_w)
print(index.DB(xpb,cut_hierC_pb_ag_w, centrotypes="centroids"))

x_purchase_behavior <- x_purchase_behavior %>% mutate(clus_aeg=cut_hierC_pb_ag_w)
x_purchase_behavior %>% group_by(clus_aeg) %>% summarise_at(c('EDU', 'Affluence_Index', 'Total_Volume', 'No__of__Trans', 'Avg__Price', 'brnd_lylty'), mean) %>% view()

temp <- x_purchase_behavior  %>% group_by(clus_aeg) %>% summarise_at(c('Total_Volume', 'No__of__Trans', 'Avg__Price', 'brnd_lylty'), mean)
temp_ag <- temp[,-1] %>% scale()
centres_ag <- t(temp_ag)
colnames(centres_ag) <- t(temp[,1])
centres_ag <- (centres_ag - min(centres_ag))/(max(centres_ag)-min(centres_ag)) #scaling the centers to make them between 0 and 1
matplot(y = centres_ag, type = 'l', lty = 1, col = 1:4, cex = 1, xlab = "Purchase behavior", ylab = "mean", xaxt="n")
legend(legend = colnames(centres_ag), x = "topright", y = "topright", lty = 1, lwd = 2, col = 1:5)
angleAxis(1, labels = c('Total_Volume', 'No__of__Trans', 'Avg__Price', 'brnd_lylty'), at = 1:9, srt = 90, xpd = TRUE, cex = 0.7)

#Visualize  clusters
fviz_cluster(list(data=xpb,cluster=cut_hierC_pb_ag_w), main="agnes-wards")
fviz_dend(hierC_pb_ag_w, k=3, color_labels_by_k = FALSE, rect=TRUE, main="agnes - Wards")

#DBSCAN 
msDbscan<- dbscan(xpb, eps = 0.75, MinPts = 6) #optimum k=3  eps= 0.75, minpts = 6
msDbscan
dbscan::kNNdistplot(xpb, k =  4)
abline(h = 0.75, lty = 2)

msDbscan$cluster <- msDbscan$cluster + 1   #changed cluster 0,1,2 to 1,2,3.

x_purchase_behavior <- x_purchase_behavior %>% mutate(clus_dbs=msDbscan$cluster)
x_purchase_behavior %>% group_by(clus_dbs) %>% summarise_at(c('EDU', 'Affluence_Index', 'Total_Volume', 'No__of__Trans', 'Avg__Price', 'brnd_lylty'), mean) %>% view()

temp <- x_purchase_behavior %>% group_by(clus_dbs) %>% summarise_at(c('Total_Volume', 'No__of__Trans', 'Avg__Price', 'brnd_lylty'), mean)
temp_dbs <- temp[,-1] %>% scale()
centres_dbs <- t(temp_dbs)
colnames(centres_dbs) <- t(temp[,1])
centres_dbs <- (centres_dbs - min(centres_dbs))/(max(centres_dbs)-min(centres_dbs)) #scaling the centers to make them between 0 and 1
matplot(y = centres_dbs, type = 'l', lty = 1, col = 1:4, cex = 1, xlab = "Purchase behavior", ylab = " ", xaxt="n")
legend(legend = colnames(centres_dbs), x = "topright", y = "topright", lty = 1, lwd = 2, col = 1:5)
angleAxis(1, labels = c('Total_Volume', 'No__of__Trans', 'Avg__Price', 'brnd_lylty'), at = 1:9, srt = 90, xpd = TRUE, cex = 0.7)

dissimilar <- daisy(xpb)         
sil <- silhouette(msDbscan$cluster, dissimilar)
fviz_silhouette(sil)

#Analysis 2 - Basis for Purchase

x_basis_pur <- bsd
x_basis_pur <- x_basis_pur %>% mutate(PropCat_beauty = PropCat_5+PropCat_9+PropCat_10+PropCat_11+PropCat_13+PropCat_8) %>% select(-c('PropCat_5','PropCat_9','PropCat_10','PropCat_11','PropCat_13','PropCat_8'))
x_basis_pur <- x_basis_pur %>% mutate(PropCat_health = PropCat_6+PropCat_7+PropCat_12+PropCat_14) %>% select(-c('PropCat_6','PropCat_7','PropCat_12','PropCat_14'))
x_basis_pur <- x_basis_pur %>% mutate(PropCat_other = PropCat_15) %>% select(-c('PropCat_15'))


BASIS_OF_PURCHASE <- c('Pur_Vol_Promo_6__','Pr_Cat_1', 'Pr_Cat_2',
                       'Pr_Cat_3', 'Pr_Cat_4', 'PropCat_beauty')


xbop <- x_basis_pur %>% dplyr::select(BASIS_OF_PURCHASE) %>% scale() 


kmClus_bop<- xbop%>%kmeans(centers=4, nstart=30)
kmClus_bop$tot.withinss

kmClus_pb$tot.withinss/kmClus_pb$betweenss
fviz_nbclust(xbop, kmeans, method = "wss")
fviz_nbclust(xbop, kmeans, method = "silhouette")

dissimilar <- daisy(xbop)         
sil <- silhouette(kmClus_bop$cluster, dissimilar)
fviz_silhouette(sil)

centre_km1 <- t(kmClus_bop$centers)
centre_km1 <- (centre_km1 - min(centre_km1))/(max(centre_km1)-min(centre_km1)) #normalized the centers to make them between 0 and 1
row_names_cluster <- rownames(centre_km1)
matplot(y = centre_km1, type = 'l',cex=0.3,xlab = "basis of purchase", ylab = "mean" , lty = 1, col = 1:6, xaxt="n")
legend(legend = colnames(centre_km1), x = "topright", y = "topright", lty = 1, lwd = 1, col = 1:6)
angleAxis(1, labels = c('Pur_Vol_Promo_6__','Pr_Cat_1', 'Pr_Cat_2',
                        'Pr_Cat_3', 'Pr_Cat_4', 'PropCat_beauty'), at = 1:10, srt = 90, xpd = TRUE, cex = 0.7)


x_basis_pur <- x_basis_pur %>% mutate(clusKM=kmClus_bop$cluster)
x_basis_pur %>% group_by(clusKM_bop) %>% summarise_at(c('SEC','HS', 'SEX_1','SEX_2', 'EDU', 'Affluence_Index','AGE', 'CHILD_1', 'CHILD_2', 'CHILD_3', 
                                                        'CHILD_4', 'Pur_Vol_No_Promo____', 'Pur_Vol_Promo_6__',
                                                        'Pur_Vol_Other_Promo__', 'Pr_Cat_1', 'Pr_Cat_2',
                                                        'Pr_Cat_3', 'Pr_Cat_4', 'PropCat_beauty', 'PropCat_health', 'PropCat_other'), mean) %>% view()

#Hierarchical 
xdist_bop <- dist(xbop ,method = "euclidean")

ac <- function(x) {
  agnes(xdist_bop, method = x)$ac
}
map_dbl(m, ac)

xdist_bop <- get_dist(xbop ,method = "pearson")
map_dbl(m, ac)

hierC_bop_ag_w <- agnes(xdist_bop, method = "ward" )


fviz_nbclust(xbop, FUN = hcut, method = "wss", 
             k.max = 10) +
  ggtitle("Elbow method")

fviz_nbclust(xbop, FUN = hcut, method = "silhouette", 
             k.max = 10) +
  ggtitle("Silhouette method")

cut_hierC_bop_ag_w <- cutree(hierC_bop_ag_w, k = 4)
table(cut_hierC_bop_ag_w)
library(clusterSim)
print(index.DB(xbop,cut_hierC_bop_ag_w, centrotypes="centroids"))

dissimilar <- daisy(xbop)         
sil <- silhouette(cut_hierC_bop_ag_w, dissimilar)
fviz_silhouette(sil)

x_basis_pur <- x_basis_pur %>% mutate(clus_aeg=cut_hierC_bop_ag_w)
x_basis_pur %>% group_by(clus_aeg) %>% summarise_at(c('EDU', 'Affluence_Index','AGE','Pur_Vol_Promo_6__',
                                                      'Pr_Cat_1', 'Pr_Cat_2',
                                                      'Pr_Cat_3', 'Pr_Cat_4', 'PropCat_beauty'), mean) %>% view()


temp_2 <- x_basis_pur  %>% group_by(clus_aeg) %>% summarise_at(c('Pur_Vol_Promo_6__',
                                                                 'Pr_Cat_1', 'Pr_Cat_2','Pr_Cat_3', 'Pr_Cat_4', 'PropCat_beauty'), mean)
temp_ag_2 <- temp_2[,-1] %>%scale()
centres_ag_2 <- t(temp_ag_2)
colnames(centres_ag_2) <- t(temp_2[,1])
centres_ag_2 <- (centres_ag_2 - min(centres_ag_2))/(max(centres_ag_2)-min(centres_ag_2)) #scaling the centers to make them between 0 and 1
matplot(y = centres_ag_2, type = 'l', lty = 1, col = 1:4, cex = 1, xlab = "Basis of Purchase", ylab = "mean", xaxt="n")
legend(legend = colnames(centres_ag_2), x = "topright", y = "topright", lty = 1, lwd = 2, col = 1:5)
angleAxis(1, labels = c('Pur_Vol_Promo_6__','Pr_Cat_1', 'Pr_Cat_2',
                        'Pr_Cat_3', 'Pr_Cat_4', 'PropCat_beauty'), at = 1:9, srt = 90, xpd = TRUE, cex = 0.7)


#Visualize clusters
fviz_cluster(list(data=xbop,cluster=cut_hierC_bop_ag_w), main="agnes-wards")
fviz_dend(hierC_bop_ag_w, k=4, color_labels_by_k = FALSE, rect=TRUE, main="agnes - Wards")

#DBSCAN 
msDbscan<- dbscan(xbop, eps = 0.69, MinPts = 8) #optimum k=4  eps= 0.69, minpts = 8
msDbscan

dbscan::kNNdistplot(xbop, k =  4)
abline(h = 0.69, lty = 2)

msDbscan$cluster <- msDbscan$cluster + 1   #changed cluster 0,1,2,3 to 1,2,3,4

x_basis_pur <- x_basis_pur %>% mutate(clus_dbs=msDbscan$cluster)
x_basis_pur %>% group_by(clus_dbs) %>% summarise_at(c('EDU', 'Affluence_Index','AGE','Pur_Vol_Promo_6__', 'Pr_Cat_1', 'Pr_Cat_2', 'Pr_Cat_3', 'Pr_Cat_4', 'PropCat_beauty'), mean) %>% view()


temp <- x_basis_pur %>% group_by(clus_dbs) %>% summarise_at(c('Pur_Vol_Promo_6__',
                                                              'Pr_Cat_1', 'Pr_Cat_2',
                                                              'Pr_Cat_3', 'Pr_Cat_4', 'PropCat_beauty'), mean)
temp_dbs1 <- temp[,-1] %>% scale()
centres_dbs1 <- t(temp_dbs1)
colnames(centres_dbs1) <- t(temp[,1])
centres_dbs1 <- (centres_dbs1 - min(centres_dbs1))/(max(centres_dbs1)-min(centres_dbs1)) #scaling the centers to make them between 0 and 1
matplot(y = centres_dbs1, type = 'l', lty = 1, col = 1:4, cex = 1, xlab = "Basis of purchase", ylab = "mean ", xaxt="n")
legend(legend = colnames(centres_dbs1), x = "topright", y = "topright", lty = 1, lwd = 2, col = 1:5)
angleAxis(1, labels = c('Pur_Vol_Promo_6__',
                        'Pr_Cat_1', 'Pr_Cat_2',
                        'Pr_Cat_3', 'Pr_Cat_4', 'PropCat_beauty'), at = 1:9, srt = 90, xpd = TRUE, cex = 0.7)

dissimilar <- daisy(xpb)         
sil <- silhouette(msDbscan$cluster, dissimilar)
fviz_silhouette(sil)
fviz_nbclust(xpb, dbscan, method = "wss")
fviz_nbclust(xbop, dbscan, method = "wss")

#Analysis 3 - Both 

x_both_segment <- x_purchase_behavior %>% select(-c(clusKM,clus_aeg))
x_both_segment <- x_both_segment %>% mutate(PropCat_beauty = PropCat_5+PropCat_9+PropCat_10+PropCat_11+PropCat_13+PropCat_8) %>% select(-c('PropCat_5','PropCat_9','PropCat_10','PropCat_11','PropCat_13','PropCat_8'))
x_both_segment <- x_both_segment %>% mutate(PropCat_health = PropCat_6+PropCat_7+PropCat_12+PropCat_14) %>% select(-c('PropCat_6','PropCat_7','PropCat_12','PropCat_14'))
x_both_segment <- x_both_segment %>% mutate(PropCat_other = PropCat_15) %>% select(-c('PropCat_15'))


PURCHASE_BEHAVIOR_AND_BASIS <- c('Total_Volume', 'No__of__Trans','Avg__Price','brnd_lylty',
                                 'Pur_Vol_Promo_6__','Pr_Cat_1', 'Pr_Cat_2',
                                 'Pr_Cat_3', 'Pr_Cat_4', 'PropCat_beauty')


xboth <- x_both_segment %>% dplyr::select(PURCHASE_BEHAVIOR_AND_BASIS) %>% scale() 
fviz_nbclust(xboth, kmeans, method = "wss")
fviz_nbclust(xboth, kmeans, method = "silhouette")


#K-Means
kmClus_both<- xboth%>%kmeans(centers=4, nstart=20)
kmClus_both$tot.withinss

dissimilar <- daisy(xboth)         
sil <- silhouette(kmClus_both$cluster, dissimilar)
fviz_silhouette(sil)

fviz_cluster(kmClus_both, data=xboth)
x_both_segment <- x_both_segment %>% mutate(clusKM=kmClus_both$cluster)
x_both_segment %>% group_by(clusKM) %>% summarise_at(c('SEC', 'HS', 'SEX_1','SEX_2', 'EDU', 'Affluence_Index','AGE', 'CHILD_1', 'CHILD_2', 'CHILD_3', 'CHILD_4', PURCHASE_BEHAVIOR_AND_BASIS), mean) %>% view()

centres_km3<- t(kmClus_both$centers)
centres_km3 <- (centres_km3 - min(centres_km3))/(max(centres_km3)-min(centres_km3))
matplot(y = centres_km3, type = 'l', lty = 1, col = 1:6, cex = 1, xlab = "Purchase behavior and basis", ylab = " ", xaxt="n")
legend(legend = colnames(centres_km3), x = "topright", y = "topright", lty = 1, lwd = 2, col = 1:5)
angleAxis(1, labels = c(colnames(xboth)), at = 1:10, srt = 90, xpd = TRUE, cex = 0.7)

#Hierarchical
xdist_b <- get_dist(xboth ,method = "euclidean")
hierC_both_ag_w <- agnes(xdist_b, method = "ward" )

m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

ac <- function(x) {
  agnes(xdist_b, method = x)$ac
}
map_dbl(m, ac)


fviz_nbclust(xboth, FUN = hcut, method = "wss", 
             k.max = 10) +
  ggtitle("Elbow method")
fviz_nbclust(xboth, FUN = hcut, method = "silhouette", 
             k.max = 10) +
  ggtitle("Silhouette method")

cut_hierC_both_ag_w <- cutree(hierC_both_ag_w, k = 5)
table(cut_hierC_both_ag_w)

print(index.DB(xboth,cut_hierC_both_ag_w, centrotypes="centroids"))

x_both_segment <- x_both_segment %>% mutate(clus_aeg=cut_hierC_both_ag_w)
x_both_segment %>% group_by(clus_aeg) %>% summarise_at(c('EDU', 'Affluence_Index', PURCHASE_BEHAVIOR_AND_BASIS), mean) %>% view()

temp_3 <- x_both_segment  %>% group_by(clus_aeg) %>% summarise_at(c(PURCHASE_BEHAVIOR_AND_BASIS), mean)
temp_ag_3 <- temp_3[,-1] %>% scale()
centres_ag_3 <- t(temp_ag_3)
colnames(centres_ag_3) <- t(temp_3[,1])
centres_ag_3 <- (centres_ag_3 - min(centres_ag_3))/(max(centres_ag_3)-min(centres_ag_3)) #scaling the centers to make them between 0 and 1
matplot(y = centres_ag_3, type = 'l', lty = 1, col = 1:5, cex = 1, xlab = "Purchase behavior and basis", ylab = "mean", xaxt="n")
legend(legend = colnames(centres_ag_3), x = "topright", y = "topright", lty = 1, lwd = 1, col = 1:5)
angleAxis(1, labels = c(PURCHASE_BEHAVIOR_AND_BASIS), at = 1:10, srt = 90, xpd = TRUE, cex = 0.7)

#Visualize clusters
fviz_cluster(list(data=xboth,cluster=cut_hierC_both_ag_w), main="agnes-wards")
fviz_dend(hierC_both_ag_w, k=3, color_labels_by_k = FALSE, rect=TRUE, main="agnes - Wards")

#DBSCAN
msDbscan<- dbscan(xboth, eps = 2, MinPts = 11) 
msDbscan

dbscan::kNNdistplot(xboth, k =  3)
abline(h = 1.5, lty = 2)
msDbscan$cluster <- msDbscan$cluster + 1
x_both_segment <- x_both_segment %>% mutate(clus_dbs=msDbscan$cluster)
x_both_segment %>% group_by(clus_dbs) %>% summarise_at(c('EDU', 'Affluence_Index', PURCHASE_BEHAVIOR_AND_BASIS), mean) %>% view()

temp <- x_both_segment %>% group_by(clus_dbs) %>% summarise_at(c(PURCHASE_BEHAVIOR_AND_BASIS), mean)
temp_dbs2 <- temp[,-1] %>% scale()
centres_dbs2 <- t(temp_dbs2)
colnames(centres_dbs2) <- t(temp[,1])
centres_dbs2 <- (centres_dbs2 - min(centres_dbs2))/(max(centres_dbs2)-min(centres_dbs2)) #scaling the centers to make them between 0 and 1
matplot(y = centres_dbs2, type = 'l', lty = 1, col = 1:4, cex = 1, xlab = "Purchase behavior and basis", ylab = "mean", xaxt="n")
legend(legend = colnames(centres_dbs2), x = "topright", y = "topright", lty = 1, lwd = 2, col = 1:5)
angleAxis(1, labels = c(PURCHASE_BEHAVIOR_AND_BASIS), at = 1:10, srt = 90, xpd = TRUE, cex = 0.7)

dissimilar <- daisy(xpb)         
sil <- silhouette(msDbscan$cluster, dissimilar)
fviz_silhouette(sil)

x <- x3 %>% mutate(clusKM_both = x3$clusKM_both,clus_aeg_b = x3$clus_aeg_b,clus_kkm_b=x3$clus_kkm_b)
x1 <- x3 %>% mutate(clusKM_both = x3$clusKM_both,clus_aeg_b = x3$clus_aeg_b,clus_kkm_b=x3$clus_kkm_b)

#Decision Trees
nr<-nrow(x_basis_pur)

set.seed(22)
trnIndex<- sample(1:nr, size = round(0.70*nr), replace=FALSE)
x_trn <- x_basis_pur[trnIndex, ]
x_tst <- x_basis_pur[-trnIndex, ]
dim(x_trn)
dim(x_tst)

km_dt <- rpart(clusKM ~., data=x_trn %>% select(-c(Member_id,clus_aeg)), method="class", parms = list(split = "gini"), control = rpart.control(cp = 0.0, minsplit = 50))

printcp(km_dt)
plotcp(km_dt)
kmp_r<- prune.rpart(km_dt, cp=0.0004)

#Variable importance
kmp_r$variable.importance

#Evaluate performance
predTrn=predict(kmp_r,x_trn, type='class')
table(pred = predTrn, true=x_trn$clusKM)
mean(predTrn == x_trn$clusKM)

table(pred = predict(kmp_r,x_tst, type='class'), true=x_tst$clusKM)
mean(predict(kmp_r,x_tst, type='class') ==x_tst$clusKM)

aeg_dt <- rpart(clus_aeg ~., data=x_trn %>% select(-c(Member_id,clusKM)), method="class", parms = list(split = "gini"), control = rpart.control(cp = 0.0, minsplit = 50))
printcp(aeg_dt)
plotcp(aeg_dt)
aegp_r<- prune.rpart(aeg_dt, cp=0.14)

#Variable importance
aegp_r$variable.importance

#Evaluate performance
predTrn=predict(aegp_r,x_trn, type='class')
table(pred = predTrn, true=x_trn$clus_aeg)
mean(predTrn == x_trn$clus_aeg)

table(pred = predict(aegp_r,x_tst, type='class'), true=x_tst$clus_aeg)
mean(predict(aegp_r,x_tst, type='class') ==x_tst$clus_aeg)
