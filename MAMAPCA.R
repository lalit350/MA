#PCA using Apple quality dataset
install.packages("corrr")
library('corrr')
install.packages("ggcorrplot")
library(ggcorrplot)
install.packages("FactoMineR")
library("FactoMineR")
install.packages("factoextra") #for visualization (fviz)
library(factoextra)

apple_quality = read.csv(choose.files())
str(apple_quality)

#Checking null values
colSums(is.na(apple_quality))
apple = na.omit(apple_quality)
colSums(is.na(apple))

#Normalizing the data
numerical_data = apple[,2:8]
head(numerical_data)
numerical_data$Acidity = as.numeric(numerical_data$Acidity)
str((numerical_data))
data_normalized = scale(numerical_data)
head(data_normalized)

#Computing the correlation matrix
corr_matrix <- cor(data_normalized)
ggcorrplot(corr_matrix)


#Applying PCA
data.pca <- princomp(corr_matrix)
summary(data.pca)
data.pca$loadings[, 1:4]

#Visualizing the Principal Components
#1) Scree Plot
fviz_eig(data.pca, addlabels = TRUE)

#2) Biplot
fviz_pca_var(data.pca, col.var = "black")

#3) Contribution of each variable 
fviz_cos2(data.pca, choice = "var", axes = 1:2)

#4) Biplot combined with cos2 
fviz_pca_var(data.pca, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)









#PCA using Factor Analysis dataset
install.packages("corrr")
library('corrr')
install.packages("ggcorrplot")
library(ggcorrplot)
install.packages("FactoMineR")
library("FactoMineR")
install.packages("factoextra") #for visualization (fviz)
library(factoextra)

factor_analysis = read.csv(choose.files())
str(factor_analysis)

#Checking null values
colSums(is.na(factor_analysis))

#Normalizing the data
#As data is already normalized, we will go ahead without selecting the rows
data_normalized = scale(factor_analysis)
head(data_normalized)

#Computing the correlation matrix
corr_matrix <- cor(data_normalized)
corr_matrix
ggcorrplot(corr_matrix)

#Applying PCA
data.pca <- princomp(corr_matrix)
summary(data.pca)
data.pca$loadings[, 1:3]

#Visualizing the Principal Components
#1) Scree Plot
fviz_eig(data.pca, addlabels = TRUE)

#2) Biplot
fviz_pca_var(data.pca, col.var = "black")

#3) Contribution of each variable 
fviz_cos2(data.pca, choice = "var", axes = 1:2)

#4) Biplot combined with cos2 
fviz_pca_var(data.pca, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)

