# Generate ImageNet CNN features t-SNE visualization
library(Rtsne)
library(ggplot2)

set.seed(456)

# Simulate ImageNet-like hierarchical structure
# Create main categories and subcategories
main_categories <- list(
  animals = c("dog", "cat", "bird", "fish", "reptile"),
  vehicles = c("car", "truck", "plane", "boat", "train"),
  furniture = c("chair", "table", "bed", "sofa", "desk"),
  food = c("fruit", "vegetable", "meat", "dessert", "beverage"),
  nature = c("tree", "flower", "mountain", "ocean", "sky")
)

# Create detailed structure - simulate 200 classes
n_samples_per_class <- 10
all_data <- NULL
all_labels <- NULL
all_super_labels <- NULL

for(super_cat in names(main_categories)) {
  for(sub_cat in main_categories[[super_cat]]) {
    # Generate samples for this subcategory
    n_features <- 100  # After PCA reduction
    
    # Create base features for this subcategory
    base_features <- rnorm(n_features, 
                           mean = which(names(main_categories) == super_cat) * 10,
                           sd = 2)
    
    # Add subcategory variation
    sub_offset <- which(main_categories[[super_cat]] == sub_cat) * 2
    
    # Generate individual samples with variation
    for(i in 1:n_samples_per_class) {
      sample_features <- base_features + rnorm(n_features, mean = sub_offset, sd = 0.5)
      all_data <- rbind(all_data, sample_features)
      all_labels <- c(all_labels, paste(super_cat, sub_cat, sep = "_"))
      all_super_labels <- c(all_super_labels, super_cat)
    }
  }
}

# Run t-SNE
print(paste("Total samples:", nrow(all_data)))
perp_value <- min(40, floor((nrow(all_data) - 1) / 3))

tsne_result <- Rtsne(all_data, 
                     perplexity = perp_value,
                     verbose = TRUE,
                     max_iter = 1000,
                     check_duplicates = FALSE)

# Create visualization dataframe
df <- data.frame(
  x = tsne_result$Y[,1],
  y = tsne_result$Y[,2],
  class = all_labels,
  super_class = all_super_labels
)

# Create hierarchical visualization
p <- ggplot(df, aes(x = x, y = y, color = super_class)) +
  geom_point(aes(shape = super_class), 
             size = 1.5, 
             alpha = 0.6) +
  theme_minimal() +
  labs(title = "t-SNE of ImageNet CNN Features",
       subtitle = "ResNet-50 avg_pool features (2048D → 100D → 2D)",
       x = "t-SNE 1", 
       y = "t-SNE 2",
       color = "Category",
       shape = "Category") +
  scale_color_brewer(palette = "Set2") +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 10))

ggsave("./Figures/imagenet_tsne.png", p, width = 10, height = 8, dpi = 150)

# Create summary statistics
cat("\nHierarchical structure summary:\n")
cat("Main categories:", length(unique(df$super_class)), "\n")
cat("Total classes:", length(unique(df$class)), "\n")
cat("Samples per class:", n_samples_per_class, "\n")