# === MODULE 6: Interpretation and Pitfalls ===
# Time: 20 minutes

# === SECTION 6.1: What NOT to Trust (7 min) ===

# Generate data to demonstrate pitfalls
set.seed(456)

# Create three clusters with different densities
cluster1 <- matrix(rnorm(100 * 2, mean = 0, sd = 0.3), ncol = 2)
cluster2 <- matrix(rnorm(100 * 2, mean = 3, sd = 0.3), ncol = 2)
cluster3 <- matrix(rnorm(300 * 2, mean = 1.5, sd = 1.5), ncol = 2)  # Sparse

synthetic_data <- rbind(cluster1, cluster2, cluster3)
synthetic_labels <- factor(rep(c("Dense1", "Dense2", "Sparse"), c(100, 100, 300)))

# Run t-SNE
tsne_synthetic <- Rtsne(synthetic_data,
                        dims = 2,
                        perplexity = 30,
                        max_iter = 500,
                        verbose = FALSE,
                        check_duplicates = FALSE)

# Visualize original and t-SNE
p_original <- ggplot(data.frame(X = synthetic_data[,1], 
                                Y = synthetic_data[,2],
                                Cluster = synthetic_labels),
                     aes(x = X, y = Y, color = Cluster)) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(title = "Original Data",
       subtitle = "Note: 'Sparse' has 3x more points but spread out") +
  coord_fixed()

p_tsne <- ggplot(data.frame(X = tsne_synthetic$Y[,1],
                            Y = tsne_synthetic$Y[,2],
                            Cluster = synthetic_labels),
                 aes(x = X, y = Y, color = Cluster)) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(title = "t-SNE Embedding",
       subtitle = "Cluster sizes NOT preserved!")

grid.arrange(p_original, p_tsne, ncol = 2)

# Instructor: Emphasize that cluster SIZE means nothing in t-SNE!

# === SECTION 6.2: Distance Interpretation (6 min) ===

# Measure distances in original vs t-SNE space
sample_indices <- c(1, 101, 201)  # One from each cluster
original_distances <- dist(synthetic_data[sample_indices, ])
tsne_distances <- dist(tsne_synthetic$Y[sample_indices, ])

cat("Original distances between cluster centers:\n")
print(as.matrix(original_distances))
cat("\nt-SNE distances:\n")
print(as.matrix(tsne_distances))

# Create misleading example: uniform random data
uniform_data <- matrix(runif(500 * 10, min = 0, max = 1), ncol = 10)
tsne_uniform <- Rtsne(uniform_data,
                      dims = 2,
                      perplexity = 30,
                      max_iter = 500,
                      verbose = FALSE,
                      check_duplicates = FALSE)

# Visualize - appears to have structure!
ggplot(data.frame(X = tsne_uniform$Y[,1], Y = tsne_uniform$Y[,2]),
       aes(x = X, y = Y)) +
  geom_point(alpha = 0.6, color = "darkblue") +
  theme_minimal() +
  labs(title = "t-SNE of Uniform Random Data",
       subtitle = "WARNING: Shows false structure! No real clusters exist")

# === SECTION 6.3: Validation Best Practices (7 min) ===

# Function to run stability analysis
stability_analysis <- function(data, n_runs = 5, perplexity = 30) {
  results <- list()
  
  for (i in 1:n_runs) {
    set.seed(i * 1000)
    tsne <- Rtsne(data,
                  dims = 2,
                  perplexity = perplexity,
                  max_iter = 500,
                  verbose = FALSE,
                  check_duplicates = FALSE)
    results[[i]] <- tsne$Y
  }
  
  return(results)
}

# Run stability analysis on iris
iris_stability <- stability_analysis(iris_scaled, n_runs = 5)

# Visualize all runs
stability_plots <- list()
for (i in 1:5) {
  df <- data.frame(X = iris_stability[[i]][,1],
                   Y = iris_stability[[i]][,2],
                   Species = iris$Species)
  
  stability_plots[[i]] <- ggplot(df, aes(x = X, y = Y, color = Species)) +
    geom_point(size = 1) +
    theme_minimal() +
    labs(title = paste("Run", i)) +
    theme(legend.position = "none")
}

grid.arrange(grobs = stability_plots, ncol = 3,
             top = "t-SNE Stability: 5 Independent Runs")

# FINAL EXERCISE: Complete analysis pipeline
# Given a new dataset, perform complete t-SNE analysis

# Generate complex dataset
set.seed(789)
complex_data <- matrix(rnorm(300 * 20), ncol = 20)
complex_data[1:100, 1:5] <- complex_data[1:100, 1:5] + 2
complex_data[101:200, 6:10] <- complex_data[101:200, 6:10] + 2
complex_labels <- factor(rep(c("A", "B", "C"), each = 100))

# Your complete pipeline:
# 1. Preprocess
# 2. Run t-SNE with parameter sweep
# 3. Validate results
# 4. Create final visualization

# Your code here: