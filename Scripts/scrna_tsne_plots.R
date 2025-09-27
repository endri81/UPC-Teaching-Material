# Generate example single-cell RNA-seq t-SNE visualization
library(Rtsne)
library(ggplot2)

# Simulate single-cell data with distinct cell types
set.seed(123)
n_cells <- 2000  # Simplified for example
n_genes <- 50    # After PCA reduction

# Create 5 major cell types with some subclusters
cell_types <- c("T cells", "B cells", "Monocytes", "NK cells", "Dendritic")
n_per_type <- n_cells / length(cell_types)

# Generate data with distinct clusters
data <- matrix(rnorm(n_cells * n_genes), nrow = n_cells)
labels <- rep(cell_types, each = n_per_type)

# Add cluster structure
for(i in 1:length(cell_types)) {
  idx <- which(labels == cell_types[i])
  # Shift each cell type in feature space
  data[idx, ] <- data[idx, ] + rnorm(n_genes, mean = i * 2, sd = 0.5)
  
  # Add some subclusters within cell types
  if(i == 1) {  # T cells have subtypes
    subtype1 <- idx[1:(length(idx)/2)]
    data[subtype1, 1:10] <- data[subtype1, 1:10] + 2
  }
}

# Add a rare cell population (0.1%)
n_rare <- round(n_cells * 0.001)
rare_idx <- sample(1:n_cells, n_rare)
data[rare_idx, ] <- data[rare_idx, ] + rnorm(n_genes, mean = 10, sd = 1)
labels[rare_idx] <- "Rare cells"

# Run t-SNE
tsne_result <- Rtsne(data, perplexity = 30, verbose = TRUE)

# Create visualization
df <- data.frame(
  tSNE1 = tsne_result$Y[,1],
  tSNE2 = tsne_result$Y[,2],
  CellType = as.factor(labels)
)

p <- ggplot(df, aes(x = tSNE1, y = tSNE2, color = CellType)) +
  geom_point(size = 0.8, alpha = 0.7) +
  theme_minimal() +
  labs(title = "t-SNE of Single-Cell RNA-seq Data",
       subtitle = "10,000 cells, colored by cell type") +
  theme(legend.position = "right") +
  scale_color_brewer(palette = "Set2")

ggsave("./Figures/scrna_tsne.png", p, width = 8, height = 6, dpi = 150)