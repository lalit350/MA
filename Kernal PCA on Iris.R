library(kernlab)
data(iris)
features <- iris[, 1:4]
kernel <- rbfdot(sigma = 0.1)
features_matrix <- as.matrix(features)
kpca_result <- kpca(features_matrix, kernel = kernel, features = 2)
projected_data <- as.data.frame(predict(kpca_result, features_matrix))
plot(projected_data, col = iris$Species, pch = 20, main = "Kernel PCA on Iris Dataset")
legend("topleft", legend = unique(iris$Species), col = 1:length(unique(iris$Species)), pch = 20)

