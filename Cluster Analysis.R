# Cluster Analysis using Sales Product
# Hierarchical Clustering 

install.packages(c("factoextra","cluster","NbClust"))

library(factoextra)
library(cluster)
library(NbClust)

df = read.csv(choose.files())
View(df)
str(df)

# Omitting na values
df = na.omit(df)

#Covert all character variables or int variable to numeric
df$Product_Description = as.numeric(as.factor(df$Product_Description))
df$Product_Category = as.numeric(as.factor(df$Product_Category))
df$Product_Line = as.numeric(as.factor(df$Product_Line))
df$Raw_Material = as.numeric(as.factor(df$Raw_Material))
df$Region = as.numeric(as.factor(df$Region))
df = scale(df)
df

#Hierarchical Clustering
library(factoextra)
fviz_nbclust(df, FUN = hcut , method = "silhouette")

# Average Linkage Method
metode_al = hclust(dist(df), method = "average")
hc_ave = cophenetic(metode_al)
cor.ave = cor(as.dist(dist(df)), hc_ave)

# Single Linkage Method
metode_sl = hclust(dist(df), method = "single")
hc_single = cophenetic(metode_sl)
cor.single = cor(as.dist(dist(df)), hc_single)

#Complete Linkage Method
metode_cl = hclust(dist(df), method = "complete")
hc_complete = cophenetic(metode_cl)
cor.complete = cor(as.dist(dist(df)), hc_complete)

# Ward Method
metode_w = hclust(dist(df), method = "ward.D")
hc_ward = cophenetic(metode_w)
cor.Ward = cor(as.dist(dist(df)), hc_ward)

# Centroid Method
metode_cd = hclust(dist(df), method = "centroid")
hc_centroid = cophenetic(metode_cd)
cor.centroid = cor(as.dist(dist(df)), hc_centroid)

# Displays the cophenetic correlation value to 1 line
cbind.data.frame(cor.ave, cor.complete, cor.single, cor.Ward, cor.centroid)

# Plotting
plot(metode_al)
rect.hclust(metode_al, 3)

# Visualizing
km = kmeans(df, centers = 3, nstart = 25)
fviz_cluster(km, data = df)

# Cluster Profiling is done by finding the average value
group3 = cutree(metode_al, 3)
table(group3)
table(group3/nrow(df))
aggregate(df, list(group3), mean)






# Cluster Analysis USing USAressts Dataset
data("USArrests")

install.packages("MVN")
library(MVN)
outlier = mvn(USArrests, multivariateOutlierMethod = "quan", showNewData = TRUE)
# Here we Found that there are 8 outlier
# so we are discarding the Alaska data
df = USArrests[-2,]
df = scale(USArrests)

# Hierarchical Clustering
library(factoextra)
fviz_nbclust(df, FUN=hcut, method = "silhouette")

# Average Linkage Method
metode_al = hclust(dist(df), method = "average")
hc_ave = cophenetic(metode_al)
cor.ave = cor(as.dist(dist(df)), hc_ave)

# Single Linkage Method
metode_sl = hclust(dist(df), method = "single")
hc_single = cophenetic(metode_sl)
cor.single = cor(as.dist(dist(df)), hc_single)

#Complete Linkage Method
metode_cl = hclust(dist(df), method = "complete")
hc_complete = cophenetic(metode_cl)
cor.complete = cor(as.dist(dist(df)), hc_complete)

# Ward Method
metode_w = hclust(dist(df), method = "ward.D")
hc_ward = cophenetic(metode_w)
cor.Ward = cor(as.dist(dist(df)), hc_ward)

# Centroid Method
metode_cd = hclust(dist(df), method = "centroid")
hc_centroid = cophenetic(metode_cd)
cor.centroid = cor(as.dist(dist(df)), hc_centroid)

# Displays the cophenetic correlation value to 1 line
cbind.data.frame(cor.ave, cor.complete, cor.single, cor.Ward, cor.centroid)

# Plotting
plot(metode_al)
rect.hclust(metode_al, 2)

# Visualizing
km = kmeans(df, centers = 2, nstart = 25)
fviz_cluster(km, data = df)

# Cluster Profiling
group2 = cutree(metode_al, 2)
table(group2)
table(group2/nrow(df))
aggregate(df, list(group2), mean)
