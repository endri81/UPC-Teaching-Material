# Laboratory 1: t-SNE Implementation in R
# Module 1: Setup and Data Exploration
# Time: 20 minutes

# === SECTION 1.1: Environment Setup (5 min) ===

# Install required packages (run once)
install.packages(c("Rtsne", "tidyverse", "ggplot2", "plotly", 
                   "umap", "factoextra", "corrplot"))

# Load libraries
library(Rtsne)
library(tidyverse)
library(ggplot2)
library(plotly)
library(factoextra)
library(corrplot)

# Set seed for reproducibility
set.seed(42)

# Check installation
cat("R version:", R.version.string, "\n")
cat("Rtsne version:", as.character(packageVersion("Rtsne")), "\n") 

# === SECTION 1.2: Understanding High-Dimensional Data (7 min) ===

# Load iris dataset - our starting point
data(iris)
cat("Dataset dimensions:", dim(iris), "\n")
cat("Variables:", names(iris), "\n")

# Instructor Note: Explain that we have 4D data we want to visualize in 2D

# Visualize pairwise relationships
pairs(iris[,1:4], 
      col = as.numeric(iris$Species),
      pch = 19,
      main = "Iris Dataset: All Pairwise Projections")

# Key insight: Some separation visible, but we see only 2D slices

# Create distance matrix
iris_data <- iris[,1:4]
dist_matrix <- dist(iris_data)

# Visualize distance matrix
# Visualize distance matrix using a heatmap
dist_mat <- as.matrix(dist_matrix)
heatmap(dist_mat[1:50, 1:50], 
        Rowv = NA, # Turn off row reordering
        Colv = NA, # Turn off column reordering
        labRow = "", # Remove row labels
        labCol = "", # Remove column labels
        main = "Distance Matrix Heatmap (first 50 samples)")

# === SECTION 1.3: The Curse of Dimensionality Demo (8 min) ===

# Generate high-dimensional random data
generate_sphere_points <- function(n, d) {
  # Generate points on unit sphere in d dimensions
  X <- matrix(rnorm(n * d), nrow = n)
  X <- X / sqrt(rowSums(X^2))
  return(X)
}

# Compare distance distributions in different dimensions
dimensions <- c(2, 10, 100)
n_points <- 500

distance_distributions <- map_df(dimensions, function(d) {
  X <- generate_sphere_points(n_points, d)
  distances <- as.vector(dist(X))
  data.frame(
    dimension = d,
    distance = distances
  )
})

# Instructor: Explain concentration of distances
ggplot(distance_distributions, aes(x = distance, fill = factor(dimension))) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 50) +
  facet_wrap(~dimension, scales = "free_y", 
             labeller = labeller(dimension = function(x) paste0("D = ", x))) +
  theme_minimal() +
  labs(title = "Distance Concentration in High Dimensions",
       x = "Pairwise Distance", 
       y = "Count",
       fill = "Dimension") +
  theme(legend.position = "none")

# Compute statistics
distance_stats <- distance_distributions %>%
  group_by(dimension) %>%
  summarise(
    mean_dist = mean(distance),
    sd_dist = sd(distance),
    cv = sd_dist / mean_dist  # coefficient of variation
  )

print("Distance statistics by dimension:")
print(distance_stats)

# EXERCISE 1: Student Task
# Calculate what percentage of volume is in the outer shell
# for a sphere in d=10 dimensions, with inner radius 0.9
d <- 10
inner_radius <- 0.9
outer_radius <- 1.0

# Your code here:
volume_ratio <- ____________  # Hint: (outer^d - inner^d) / outer^d

cat("Percentage in shell:", round(volume_ratio * 100, 2), "%\n")