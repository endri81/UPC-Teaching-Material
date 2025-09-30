# === MODULE 4: PCA Preprocessing and Advanced Options ===
# Time: 20 minutes

# === SECTION 4.1: Why PCA Preprocessing? (7 min) ===

# Generate a high-dimensional dataset
n_samples <- 200
n_features <- 100

set.seed(123)
high_dim_data <- matrix(rnorm(n_samples * n_features), 
                        nrow = n_samples)

# Add structure: 3 clusters
high_dim_data[1:66, 1:30] <- high_dim_data[1:66, 1:30] + 3
high_dim_data[67:133, 31:60] <- high_dim_data[67:133, 31:60] + 3  
high_dim_data[134:200, 61:90] <- high_dim_data[134:200, 61:90] + 3

labels <- factor(rep(c("A", "B", "C"), c(66, 67, 67)))

# Compare: Direct t-SNE vs PCA + t-SNE
# Direct t-SNE
start_time_direct <- Sys.time()
tsne_direct <- Rtsne(high_dim_data,
                     dims = 2,
                     perplexity = 30,
                     max_iter = 500,
                     verbose = FALSE,
                     check_duplicates = FALSE,
                     pca = FALSE)
time_direct <- Sys.time() - start_time_direct

# PCA preprocessing then t-SNE
start_time_pca <- Sys.time()
pca_result <- prcomp(high_dim_data, center = TRUE, scale. = TRUE)
pca_data <- pca_result$x[, 1:30]  # Keep first 30 PCs

tsne_pca <- Rtsne(pca_data,
                  dims = 2,
                  perplexity = 30,
                  max_iter = 500,
                  verbose = FALSE,
                  check_duplicates = FALSE,
                  pca = FALSE)
time_pca <- Sys.time() - start_time_pca

# Compare results
comparison_df <- data.frame(
  X_direct = tsne_direct$Y[,1],
  Y_direct = tsne_direct$Y[,2],
  X_pca = tsne_pca$Y[,1],
  Y_pca = tsne_pca$Y[,2],
  Cluster = labels
)

p_direct <- ggplot(comparison_df, aes(x = X_direct, y = Y_direct, color = Cluster)) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(title = "Direct t-SNE",
       subtitle = paste("Time:", round(time_direct, 2), "sec"))

p_pca <- ggplot(comparison_df, aes(x = X_pca, y = Y_pca, color = Cluster)) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(title = "PCA + t-SNE",
       subtitle = paste("Time:", round(time_pca, 2), "sec"))

library(gridExtra)
grid.arrange(p_direct, p_pca, ncol = 2)

# === SECTION 4.2: Initialization Strategies (7 min) ===

# Compare random vs PCA initialization
run_tsne_with_init <- function(data, init_method) {
  if (init_method == "pca") {
    pca <- prcomp(data, scale. = TRUE)
    init <- pca$x[, 1:2] * 0.0001
  } else {
    init <- matrix(rnorm(nrow(data) * 2, sd = 0.0001), 
                   nrow = nrow(data))
  }
  
  Rtsne(data,
        dims = 2,
        Y_init = init,
        perplexity = 30,
        max_iter = 500,
        verbose = FALSE,
        check_duplicates = FALSE,
        pca = FALSE)
}

# Run both
tsne_random_init <- run_tsne_with_init(iris_scaled, "random")
tsne_pca_init <- run_tsne_with_init(iris_scaled, "pca")

# Visualize
init_df <- data.frame(
  X_random = tsne_random_init$Y[,1],
  Y_random = tsne_random_init$Y[,2],
  X_pca = tsne_pca_init$Y[,1],
  Y_pca = tsne_pca_init$Y[,2],
  Species = iris$Species
)

p_random <- ggplot(init_df, aes(x = X_random, y = Y_random, color = Species)) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(title = "Random Initialization")

p_pca_init <- ggplot(init_df, aes(x = X_pca, y = Y_pca, color = Species)) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(title = "PCA Initialization")

grid.arrange(p_random, p_pca_init, ncol = 2)

# === SECTION 4.3: Early Exaggeration (6 min) ===

# Instructor: Explain early exaggeration concept
# Note: Standard Rtsne doesn't expose early exaggeration directly
# We'll demonstrate the concept

# EXERCISE 4: Implement simple exaggeration
# Multiply P matrix by factor of 4 for first iterations
# This is conceptual - actual implementation requires modifying algorithm

exaggeration_demo <- function(P, exaggeration_factor = 4) {
  # Your code here:
  P_exaggerated <- ____________
  return(P_exaggerated)
}

# Test with toy example
toy_P <- matrix(c(0, 0.5, 0.5,
                  0.5, 0, 0.5,
                  0.5, 0.5, 0), nrow = 3)
P_exag <- exaggeration_demo(toy_P)
print("Original P:")
print(toy_P)
print("Exaggerated P:")
print(P_exag)