# === MODULE 3: First t-SNE Implementation ===
# Time: 20 minutes

# === SECTION 3.1: Basic t-SNE on Iris (8 min) ===

# Prepare data - ALWAYS scale!
iris_scaled <- scale(iris_data)

# Instructor: Explain why scaling matters
cat("Before scaling - variance by feature:\n")
print(apply(iris_data, 2, var))
cat("\nAfter scaling - variance by feature:\n")
print(apply(iris_scaled, 2, var))

# Run t-SNE with default parameters
tsne_result <- Rtsne(iris_scaled, 
                     dims = 2,
                     perplexity = 30,
                     verbose = TRUE,
                     max_iter = 500,
                     check_duplicates = FALSE)

# Create visualization dataframe
tsne_df <- data.frame(
  X = tsne_result$Y[,1],
  Y = tsne_result$Y[,2],
  Species = iris$Species
)

# Basic visualization
p1 <- ggplot(tsne_df, aes(x = X, y = Y, color = Species)) +
  geom_point(size = 2, alpha = 0.8) +
  theme_minimal() +
  labs(title = "t-SNE of Iris Dataset (Default Parameters)",
       subtitle = paste("Perplexity =", 30, "| Iterations =", 500))

print(p1)

# === SECTION 3.2: Effect of Perplexity (7 min) ===

# Corrected perplexity values
perplexity_values <- c(5, 15, 30, 45)
tsne_perplexity_results <- list()

for (perp in perplexity_values) {
  result <- Rtsne(iris_scaled,
                  dims = 2,
                  perplexity = perp,
                  max_iter = 500,
                  verbose = FALSE,
                  check_duplicates = FALSE)
  
  tsne_perplexity_results[[as.character(perp)]] <- data.frame(
    X = result$Y[,1],
    Y = result$Y[,2],
    Species = iris$Species,
    Perplexity = paste("Perplexity:", perp)
  )
}

# Combine results
perplexity_df <- bind_rows(tsne_perplexity_results)

# Visualize effect of perplexity
ggplot(perplexity_df, aes(x = X, y = Y, color = Species)) +
  geom_point(size = 1.5, alpha = 0.7) +
  facet_wrap(~Perplexity, scales = "free") +
  theme_minimal() +
  labs(title = "Effect of Perplexity on t-SNE Results",
       subtitle = "Same data, different perplexity values") +
  theme(legend.position = "bottom")

# === SECTION 3.3: Monitoring Convergence (5 min) ===

# Run with cost tracking
tsne_cost <- Rtsne(iris_scaled,
                   dims = 2,
                   perplexity = 30,
                   max_iter = 1000,
                   verbose = FALSE,
                   check_duplicates = FALSE,
                   pca = FALSE,
                   partial_pca = FALSE,
                   theta = 0.5,
                   num_threads = 1)

# Extract costs (if available in your Rtsne version)
# Note: Some versions don't return iteration costs
costs <- tsne_cost$costs
if (!is.null(costs)) {
  cost_df <- data.frame(
    iteration = seq_along(costs),
    cost = costs
  )
  
  ggplot(cost_df, aes(x = iteration, y = cost)) +
    geom_line() +
    theme_minimal() +
    labs(title = "t-SNE Convergence",
         x = "Iteration",
         y = "KL Divergence")
}

# EXERCISE 3: Multiple runs for stability
# Run t-SNE 5 times with different seeds
n_runs <- 5
stability_results <- list()

for (i in 1:n_runs) {
  set.seed(i * 100)
  # Your code here:
  result <- ____________
  
  stability_results[[i]] <- data.frame(
    X = result$Y[,1],
    Y = result$Y[,2],
    Species = iris$Species,
    Run = paste("Run", i)
  )
}

# Visualize stability
stability_df <- bind_rows(stability_results)
# Create plot to show variation across runs