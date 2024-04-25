# Load required libraries
library(fda)
library(readr)

# Load the dataset
data <- read_csv(choose.files())

# Extract relevant columns (time and values)
times <- as.numeric(seq_along(data$date))  # Assuming sequential time points
values <- data$total_vaccinations

# Create a basis for functional data objects
basis <- create.bspline.basis(rangeval = range(times), nbasis = 6)

# Smooth the data using functional data smoothing
smoothed_data <- smooth.basis(times, values, basis)

# Perform functional PCA
fpca_result <- pca.fd(smoothed_data)

# Plot the functional PCA results
par(mfrow = c(1, 2))
plot(fpca_result$values, type = "b", xlab = "Component", ylab = "Eigenvalue", main = "Scree Plot")
plot(fpca_result$harmonics, col = 1:3, main = "Functional PCA Harmonics")
