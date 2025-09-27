# Example tsne_plots.R script structure
library(Rtsne)
library(ggplot2)

# MNIST example (using a subset)
# Load MNIST data (you'd need to download it first)
# mnist_data <- read.csv("mnist_subset.csv")

# For demonstration, create synthetic data
set.seed(42)
n_points <- 1000
n_features <- 50

# Simulate data with 10 clusters (digits 0-9)
data <- matrix(rnorm(n_points * n_features), nrow = n_points)
labels <- rep(0:9, each = n_points/10)

# Add cluster structure
for(i in 0:9) {
  idx <- which(labels == i)
  data[idx, ] <- data[idx, ] + rnorm(n_features, mean = i, sd = 0.5)
}

# Run t-SNE
tsne_result <- Rtsne(data, perplexity = 30, verbose = TRUE)

# Create visualization
df <- data.frame(
  x = tsne_result$Y[,1],
  y = tsne_result$Y[,2],
  digit = as.factor(labels)
)

p <- ggplot(df, aes(x = x, y = y, color = digit)) +
  geom_point(size = 1.5, alpha = 0.8) +
  theme_minimal() +
  labs(title = "t-SNE Visualization of Digits",
       x = "t-SNE 1", y = "t-SNE 2") +
  scale_color_brewer(palette = "Set3")

ggsave("./Figures/mnist_tsne.png", p, width = 8, height = 6, dpi = 150)