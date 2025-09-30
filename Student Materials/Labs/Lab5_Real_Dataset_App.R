# === MODULE 5: Real Dataset Application ===
# Time: 20 minutes

# === SECTION 5.1: Load and Explore Wine Dataset (5 min) ===

# Load wine dataset (built into R)
library(datasets)
library(rattle)
data(wine, package = "rattle")  # If not available, use UCI wine dataset

# Alternative: Load from UCI
if (!exists("wine")) {
  wine_url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data"
  wine <- read.csv(wine_url, header = FALSE)
  colnames(wine) <- c("Type", "Alcohol", "Malic_acid", "Ash", "Alcalinity_of_ash",
                      "Magnesium", "Total_phenols", "Flavanoids", "Nonflavanoid_phenols",
                      "Proanthocyanins", "Color_intensity", "Hue", "OD280_OD315", "Proline")
}

# Explore dataset
cat("Wine dataset dimensions:", dim(wine), "\n")
cat("Number of wine types:", length(unique(wine$Type)), "\n")

# Check for missing values
cat("Missing values per column:\n")
print(colSums(is.na(wine)))

# Visualize feature distributions
wine_long <- wine %>%
  pivot_longer(cols = -Type, names_to = "Feature", values_to = "Value")

ggplot(wine_long, aes(x = Value, fill = factor(Type))) +
  geom_histogram(bins = 20, alpha = 0.6, position = "identity") +
  facet_wrap(~Feature, scales = "free") +
  theme_minimal() +
  labs(title = "Wine Dataset: Feature Distributions by Type",
       fill = "Wine Type") +
  theme(legend.position = "bottom")

# === SECTION 5.2: Preprocessing Pipeline (7 min) ===

# Separate features and labels
wine_features <- wine[, -1]
wine_labels <- factor(wine$Type)

# Check scale differences
scale_check <- data.frame(
  Feature = names(wine_features),
  Mean = apply(wine_features, 2, mean),
  SD = apply(wine_features, 2, sd),
  Min = apply(wine_features, 2, min),
  Max = apply(wine_features, 2, max)
)
print("Feature scales before normalization:")
print(scale_check)

# Preprocessing pipeline
preprocess_for_tsne <- function(data) {
  # 1. Handle missing values (if any)
  data <- na.omit(data)
  
  # 2. Scale features
  data_scaled <- scale(data)
  
  # 3. Check for zero variance features
  zero_var <- which(apply(data_scaled, 2, var) == 0)
  if (length(zero_var) > 0) {
    cat("Removing zero variance features:", names(data)[zero_var], "\n")
    data_scaled <- data_scaled[, -zero_var]
  }
  
  # 4. Optional: PCA for initial dimensionality reduction
  if (ncol(data_scaled) > 50) {
    pca <- prcomp(data_scaled)
    cumvar <- cumsum(pca$sdev^2) / sum(pca$sdev^2)
    n_comp <- which(cumvar >= 0.95)[1]
    data_scaled <- pca$x[, 1:n_comp]
    cat("Reduced to", n_comp, "components (95% variance)\n")
  }
  
  return(data_scaled)
}

wine_processed <- preprocess_for_tsne(wine_features)

# === SECTION 5.3: t-SNE Analysis with Validation (8 min) ===

# Run t-SNE with multiple perplexities
perplexities <- c(10, 20, 30, 40)
wine_tsne_results <- list()

for (perp in perplexities) {
  set.seed(123)  # For reproducibility
  result <- Rtsne(wine_processed,
                  dims = 2,
                  perplexity = perp,
                  max_iter = 1000,
                  verbose = FALSE,
                  check_duplicates = FALSE)
  
  wine_tsne_results[[paste0("perp_", perp)]] <- data.frame(
    X = result$Y[,1],
    Y = result$Y[,2],
    Type = wine_labels,
    Perplexity = perp
  )
}

# Combine and visualize
wine_tsne_df <- bind_rows(wine_tsne_results)

ggplot(wine_tsne_df, aes(x = X, y = Y, color = Type)) +
  geom_point(size = 2, alpha = 0.7) +
  facet_wrap(~Perplexity, scales = "free", 
             labeller = labeller(Perplexity = function(x) paste("Perplexity:", x))) +
  theme_minimal() +
  labs(title = "Wine Dataset: t-SNE with Different Perplexities",
       subtitle = "178 wines, 13 chemical features, 3 types") +
  scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A"),
                     labels = c("Type 1", "Type 2", "Type 3"))

# Validate clustering quality
library(cluster)
best_result <- wine_tsne_results[["perp_30"]]

# Silhouette analysis
sil <- silhouette(as.numeric(wine_labels), 
                  dist(best_result[, c("X", "Y")]))
avg_sil <- mean(sil[,3])

cat("Average silhouette score:", round(avg_sil, 3), "\n")

# EXERCISE 5: Compare with PCA
# Implement PCA visualization and compare
pca_wine <- prcomp(wine_processed, scale. = FALSE)  # Already scaled

pca_df <- data.frame(
  PC1 = pca_wine$x[,1],
  PC2 = pca_wine$x[,2],
  Type = wine_labels
)

# Your visualization here:
p_pca_wine <- ggplot(____________) +
  ____________

# Compare side by side
p_tsne_wine <- ggplot(best_result, aes(x = X, y = Y, color = Type)) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(title = "t-SNE")

grid.arrange(p_pca_wine, p_tsne_wine, ncol = 2,
             top = "PCA vs t-SNE on Wine Dataset")