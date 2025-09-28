# tsne_visualization_generator.R
# Generate all images for t-SNE lecture

# Load required libraries
library(Rtsne)
library(ggplot2)
library(gridExtra)
library(scales)
library(MASS)
library(plotly)
library(viridis)
library(reshape2)

# Create output directory
dir.create("Figures", showWarnings = FALSE)

# Set consistent theme
theme_set(theme_minimal(base_size = 14))

# Set seed for reproducibility
set.seed(42)

# ===============================================
# 1. CROWDING PROBLEM VISUALIZATIONS
# ===============================================

# Completely revised crowding visualization
generate_crowding_viz <- function() {
  set.seed(42)
  n_points <- 150
  
  # LEFT PANEL: High-D space simulation
  # In high dimensions, points naturally live in a shell
  angles <- seq(0, 2*pi, length.out = n_points)
  
  # Create a clear ring/shell structure
  df_highd <- data.frame(
    x = 0.9 * cos(angles) + rnorm(n_points, 0, 0.02),
    y = 0.9 * sin(angles) + rnorm(n_points, 0, 0.02),
    space = "10D"
  )
  
  # RIGHT PANEL: 2D projection simulation
  # All those shell points must compress into available 2D space
  # This creates severe overlap in the center
  df_lowd <- data.frame(
    x = rnorm(n_points, 0, 0.15),  # Compressed to center
    y = rnorm(n_points, 0, 0.15),  # Much smaller spread
    space = "2D"
  )
  
  # Create the comparison plot
  library(patchwork)
  
  p1 <- ggplot(df_highd, aes(x, y)) +
    geom_point(color = "#2E86AB", size = 2.5, alpha = 0.8) +
    coord_fixed() +
    xlim(c(-1.5, 1.5)) + ylim(c(-1.5, 1.5)) +
    labs(title = "High-D Space (10D)",
         subtitle = "Points distributed in shell\nEveryone has room") +
    theme_minimal(base_size = 14) +
    theme(plot.title = element_text(face = "bold"),
          panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank())
  
  p2 <- ggplot(df_lowd, aes(x, y)) +
    geom_point(color = "#A23B72", size = 2.5, alpha = 0.8) +
    coord_fixed() +
    xlim(c(-1.5, 1.5)) + ylim(c(-1.5, 1.5)) +
    labs(title = "Projected to 2D",
         subtitle = "Catastrophic overlap\nInformation destroyed") +
    theme_minimal(base_size = 14) +
    theme(plot.title = element_text(face = "bold"),
          panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank())
  
  # Combine plots
  p_combined <- p1 + p2 + 
    plot_annotation(title = "Crowding Problem: High-D vs 2D",
                    theme = theme(plot.title = element_text(size = 16, face = "bold")))
  
  ggsave("Figures/crowding_interactive_demo.png", p_combined, 
         width = 12, height = 6, dpi = 100)
  
  # Also save individual frames
  ggsave("Figures/crowding_animation_frame1.png", p1, 
         width = 6, height = 6, dpi = 100)
  ggsave("Figures/crowding_interactive_demo_2d.png", p2, 
         width = 6, height = 6, dpi = 100)
}

# Alternative: More dramatic visualization
generate_crowding_viz_dramatic <- function() {
  set.seed(42)
  
  # Create data that really shows the problem
  n_points <- 200
  
  # High-D: Create three distinct shells (representing different distances)
  group1 <- data.frame(
    x = 0.5 * cos(seq(0, 2*pi, length.out = n_points/3)) + rnorm(n_points/3, 0, 0.01),
    y = 0.5 * sin(seq(0, 2*pi, length.out = n_points/3)) + rnorm(n_points/3, 0, 0.01),
    group = "Near"
  )
  
  group2 <- data.frame(
    x = 0.8 * cos(seq(0, 2*pi, length.out = n_points/3)) + rnorm(n_points/3, 0, 0.01),
    y = 0.8 * sin(seq(0, 2*pi, length.out = n_points/3)) + rnorm(n_points/3, 0, 0.01),
    group = "Medium"
  )
  
  group3 <- data.frame(
    x = 1.1 * cos(seq(0, 2*pi, length.out = n_points/3)) + rnorm(n_points/3, 0, 0.01),
    y = 1.1 * sin(seq(0, 2*pi, length.out = n_points/3)) + rnorm(n_points/3, 0, 0.01),
    group = "Far"
  )
  
  df_highd <- rbind(group1, group2, group3)
  df_highd$space <- "10D: Three distinct distance shells"
  
  # 2D: Everything collapses to center
  df_lowd <- data.frame(
    x = c(rnorm(n_points/3, 0, 0.08),
          rnorm(n_points/3, 0, 0.10),
          rnorm(n_points/3, 0, 0.12)),
    y = c(rnorm(n_points/3, 0, 0.08),
          rnorm(n_points/3, 0, 0.10),
          rnorm(n_points/3, 0, 0.12)),
    group = rep(c("Near", "Medium", "Far"), each = n_points/3),
    space = "2D: All distances collapse"
  )
  
  df_combined <- rbind(df_highd, df_lowd)
  
  p <- ggplot(df_combined, aes(x, y, color = group)) +
    geom_point(size = 1.5, alpha = 0.7) +
    facet_wrap(~space, ncol = 2) +
    coord_fixed() +
    xlim(c(-1.5, 1.5)) + ylim(c(-1.5, 1.5)) +
    scale_color_manual(values = c("Near" = "#E63946", 
                                  "Medium" = "#F77F00", 
                                  "Far" = "#06AED5")) +
    labs(title = "The Crowding Catastrophe",
         color = "Distance Group") +
    theme_minimal(base_size = 14) +
    theme(strip.text = element_text(face = "bold", size = 12),
          legend.position = "bottom")
  
  ggsave("Figures/crowding_interactive_demo.png", p, 
         width = 14, height = 7, dpi = 100)
}

# Run both versions
generate_crowding_viz()
generate_crowding_viz_dramatic()

# ===============================================
# 2. SWISS ROLL DATASET
# ===============================================

generate_swiss_roll <- function(n = 1000) {
  t <- seq(0, 4*pi, length.out = n)
  x <- t * cos(t) + rnorm(n, 0, 0.1)
  y <- runif(n, 0, 20)
  z <- t * sin(t) + rnorm(n, 0, 0.1)
  
  data.frame(x = x, y = y, z = z, color = t)
}

swiss_data <- generate_swiss_roll()

# 3D Swiss Roll
p_swiss_3d <- ggplot(swiss_data, aes(x, z, color = color)) +
  geom_point(alpha = 0.6) +
  scale_color_viridis() +
  ggtitle("Swiss Roll: Original 3D Manifold") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("Figures/swiss_roll_3d.png", p_swiss_3d, width = 8, height = 6, dpi = 100)

# PCA projection
pca_result <- prcomp(swiss_data[,1:3])
swiss_pca <- data.frame(
  PC1 = pca_result$x[,1],
  PC2 = pca_result$x[,2],
  color = swiss_data$color
)

p_swiss_pca <- ggplot(swiss_pca, aes(PC1, PC2, color = color)) +
  geom_point(alpha = 0.6) +
  scale_color_viridis() +
  ggtitle("Swiss Roll: PCA Projection (Tears Structure)") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("Figures/swiss_roll_pca.png", p_swiss_pca, width = 8, height = 6, dpi = 100)

# t-SNE projection
tsne_swiss <- Rtsne(as.matrix(swiss_data[,1:3]), perplexity = 30, verbose = FALSE)
swiss_tsne <- data.frame(
  tSNE1 = tsne_swiss$Y[,1],
  tSNE2 = tsne_swiss$Y[,2],
  color = swiss_data$color
)

p_swiss_tsne <- ggplot(swiss_tsne, aes(tSNE1, tSNE2, color = color)) +
  geom_point(alpha = 0.6) +
  scale_color_viridis() +
  ggtitle("Swiss Roll: t-SNE (Preserves Local Structure)") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("Figures/swiss_roll_tsne.png", p_swiss_tsne, width = 8, height = 6, dpi = 100)

# ===============================================
# 3. CURSE OF DIMENSIONALITY
# ===============================================

generate_curse_viz <- function() {
  dimensions <- c(2, 5, 10, 20, 50, 100)
  shell_volumes <- sapply(dimensions, function(d) {
    # Volume of shell from 0.9 to 1.0
    1 - 0.9^d
  })
  
  df <- data.frame(
    Dimension = dimensions,
    Shell_Volume = shell_volumes * 100  # Convert to percentage
  )
  
  p <- ggplot(df, aes(Dimension, Shell_Volume)) +
    geom_line(size = 2, color = "darkblue") +
    geom_point(size = 4, color = "red") +
    labs(x = "Number of Dimensions",
         y = "% of Volume in Outer Shell (0.9-1.0)",
         title = "Curse of Dimensionality: Volume Concentration") +
    scale_y_continuous(limits = c(0, 100)) +
    theme_minimal()
  
  ggsave("Figures/curse_dimensionality_animation.png", p, width = 10, height = 6, dpi = 100)
}

generate_curse_viz()

# ===============================================
# 4. DISTANCE TO PROBABILITY TRANSFORMATION
# ===============================================

generate_dist_to_prob <- function() {
  distances <- seq(0, 5, 0.1)
  
  # Different sigma values (adaptive bandwidth)
  sigma_values <- c(0.5, 1.0, 2.0)
  
  df <- expand.grid(
    distance = distances,
    sigma = sigma_values
  )
  
  df$probability <- exp(-df$distance^2 / (2 * df$sigma^2))
  df$sigma_label <- paste("σ =", df$sigma)
  
  p <- ggplot(df, aes(distance, probability, color = sigma_label)) +
    geom_line(size = 1.5) +
    labs(x = "Distance", y = "Similarity",
         title = "Gaussian Kernel: Distance to Probability",
         color = "Bandwidth") +
    theme_minimal()
  
  ggsave("Figures/distance_to_probability_animation.png", p, width = 10, height = 6, dpi = 100)
}

generate_dist_to_prob()

# ===============================================
# 5. ADAPTIVE BANDWIDTH VISUALIZATION
# ===============================================

generate_adaptive_bandwidth <- function() {
  # Dense region
  set.seed(123)
  dense_points <- mvrnorm(50, mu = c(0, 0), Sigma = matrix(c(0.1, 0, 0, 0.1), 2, 2))
  
  p_dense <- ggplot(data.frame(dense_points), aes(X1, X2)) +
    geom_point(size = 3, alpha = 0.7, color = "blue") +
    geom_point(aes(x = 0, y = 0), color = "red", size = 5) +
    annotate("path",
             x = 0.3 * cos(seq(0, 2*pi, length.out = 100)),
             y = 0.3 * sin(seq(0, 2*pi, length.out = 100)),
             color = "red", size = 1, linetype = "dashed") +
    xlim(c(-2, 2)) + ylim(c(-2, 2)) +
    ggtitle("Dense Region: Small σ") +
    coord_fixed() +
    theme_minimal()
  
  ggsave("Figures/adaptive_bandwidth_dense.png", p_dense, width = 6, height = 6, dpi = 100)
  
  # Sparse region
  sparse_points <- mvrnorm(10, mu = c(0, 0), Sigma = matrix(c(1, 0, 0, 1), 2, 2))
  
  p_sparse <- ggplot(data.frame(sparse_points), aes(X1, X2)) +
    geom_point(size = 3, alpha = 0.7, color = "blue") +
    geom_point(aes(x = 0, y = 0), color = "red", size = 5) +
    annotate("path",
             x = 1.5 * cos(seq(0, 2*pi, length.out = 100)),
             y = 1.5 * sin(seq(0, 2*pi, length.out = 100)),
             color = "red", size = 1, linetype = "dashed") +
    xlim(c(-2, 2)) + ylim(c(-2, 2)) +
    ggtitle("Sparse Region: Large σ") +
    coord_fixed() +
    theme_minimal()
  
  ggsave("Figures/adaptive_bandwidth_sparse.png", p_sparse, width = 6, height = 6, dpi = 100)
}

generate_adaptive_bandwidth()

# ===============================================
# 6. PERPLEXITY VISUALIZATION
# ===============================================

generate_perplexity_viz <- function() {
  # Show effective number of neighbors
  perplexity_values <- c(5, 15, 30, 50)
  
  plots <- list()
  for(i in seq_along(perplexity_values)) {
    perp <- perplexity_values[i]
    
    # Simulate neighborhood with varying probabilities
    n_neighbors <- 100
    probs <- exp(-seq(0, 10, length.out = n_neighbors))
    probs <- probs / sum(probs)
    
    # Adjust to match perplexity
    target_entropy <- log2(perp)
    current_entropy <- -sum(probs * log2(probs + 1e-10))
    
    df <- data.frame(
      neighbor = 1:n_neighbors,
      probability = probs * (target_entropy / current_entropy),
      significant = 1:n_neighbors <= perp
    )
    
    plots[[i]] <- ggplot(df[1:min(50, n_neighbors),], aes(neighbor, probability, fill = significant)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("TRUE" = "blue", "FALSE" = "gray")) +
      ggtitle(paste("Perplexity =", perp)) +
      theme_minimal() +
      theme(legend.position = "none")
  }
  
  p_combined <- grid.arrange(grobs = plots, ncol = 2)
  ggsave("Figures/perplexity_visual.png", p_combined, width = 12, height = 10, dpi = 100)
}

generate_perplexity_viz()

# ===============================================
# 7. KL DIVERGENCE VISUALIZATION
# ===============================================

generate_kl_divergence <- function() {
  # Asymmetry visualization
  neighbors <- 1:20
  p_true <- exp(-0.5 * (neighbors - 5)^2)
  p_true <- p_true / sum(p_true)
  
  q_good <- exp(-0.5 * (neighbors - 5.5)^2)
  q_good <- q_good / sum(q_good)
  
  q_bad <- exp(-0.5 * (neighbors - 10)^2)
  q_bad <- q_bad / sum(q_bad)
  
  df <- data.frame(
    neighbor = rep(neighbors, 3),
    probability = c(p_true, q_good, q_bad),
    distribution = rep(c("P (Truth)", "Q (Good)", "Q (Bad)"), each = 20)
  )
  
  p <- ggplot(df, aes(neighbor, probability, fill = distribution)) +
    geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
    scale_fill_manual(values = c("P (Truth)" = "blue", 
                                 "Q (Good)" = "green", 
                                 "Q (Bad)" = "red")) +
    labs(title = "KL Divergence: Measuring Information Loss",
         x = "Neighbor Index", y = "Probability") +
    theme_minimal()
  
  ggsave("Figures/kl_divergence_asymmetry.png", p, width = 10, height = 6, dpi = 100)
  ggsave("Figures/kl_divergence_visualization.png", p, width = 10, height = 6, dpi = 100)
}

generate_kl_divergence()

# ===============================================
# 8. GRADIENT FORCES VISUALIZATION
# ===============================================

generate_gradient_forces <- function() {
  # Create a grid of points
  grid_size <- 20
  x <- seq(-2, 2, length.out = grid_size)
  y <- seq(-2, 2, length.out = grid_size)
  grid <- expand.grid(x = x, y = y)
  
  # Central point
  center <- c(0, 0)
  
  # Calculate forces (simplified)
  grid$dist <- sqrt((grid$x - center[1])^2 + (grid$y - center[2])^2)
  grid$force_x <- -(grid$x - center[1]) / (1 + grid$dist^2)
  grid$force_y <- -(grid$y - center[2]) / (1 + grid$dist^2)
  
  p <- ggplot(grid, aes(x, y)) +
    geom_segment(aes(xend = x + force_x * 0.1, 
                     yend = y + force_y * 0.1),
                 arrow = arrow(length = unit(0.1, "cm")),
                 alpha = 0.5) +
    geom_point(aes(x = 0, y = 0), color = "red", size = 5) +
    ggtitle("Gradient Forces in t-SNE") +
    coord_fixed() +
    theme_minimal()
  
  ggsave("Figures/gradient_force_field.png", p, width = 8, height = 8, dpi = 100)
  ggsave("Figures/sne_gradient_forces.png", p, width = 8, height = 8, dpi = 100)
}

generate_gradient_forces()

# ===============================================
# 9. GAUSSIAN VS T-DISTRIBUTION
# ===============================================

generate_gaussian_vs_t <- function() {
  x <- seq(-5, 5, 0.01)
  
  df <- data.frame(
    distance = rep(x, 2),
    similarity = c(exp(-x^2), 1/(1 + x^2)),
    distribution = rep(c("Gaussian", "Student's t"), each = length(x))
  )
  
  p <- ggplot(df, aes(distance, similarity, color = distribution)) +
    geom_line(size = 1.5) +
    scale_color_manual(values = c("Gaussian" = "blue", "Student's t" = "red")) +
    labs(title = "Heavy Tails: Gaussian vs Student's t",
         x = "Distance", y = "Similarity") +
    theme_minimal()
  
  ggsave("Figures/gaussian_vs_t_comparison.png", p, width = 10, height = 6, dpi = 100)
  
  # Distance ratio comparison
  distances <- seq(0.5, 5, 0.1)
  ratio_gaussian <- exp(-1) / exp(-distances^2)
  ratio_t <- (1/(1+1)) / (1/(1+distances^2))
  
  df_ratio <- data.frame(
    distance = rep(distances, 2),
    ratio = c(ratio_gaussian, ratio_t),
    distribution = rep(c("Gaussian", "Student's t"), each = length(distances))
  )
  
  p_ratio <- ggplot(df_ratio, aes(distance, log10(ratio), color = distribution)) +
    geom_line(size = 1.5) +
    scale_color_manual(values = c("Gaussian" = "blue", "Student's t" = "red")) +
    labs(title = "Similarity Ratio: Reference Distance = 1",
         x = "Distance", y = "Log10(Ratio)") +
    theme_minimal()
  
  ggsave("Figures/distance_ratio_plot.png", p_ratio, width = 10, height = 6, dpi = 100)
}

generate_gaussian_vs_t()

# ===============================================
# 10. OPTIMIZATION TRICKS
# ===============================================

generate_optimization_viz <- function() {
  # Early exaggeration
  iterations <- 1:1000
  exaggeration <- ifelse(iterations <= 250, 4, 1)
  
  df_exag <- data.frame(
    iteration = iterations,
    exaggeration = exaggeration
  )
  
  p_exag <- ggplot(df_exag, aes(iteration, exaggeration)) +
    geom_line(size = 1.5, color = "red") +
    geom_vline(xintercept = 250, linetype = "dashed") +
    labs(title = "Early Exaggeration Schedule",
         x = "Iteration", y = "Exaggeration Factor") +
    theme_minimal()
  
  ggsave("Figures/early_exaggeration.png", p_exag, width = 8, height = 5, dpi = 100)
  
  # Momentum schedule
  momentum <- ifelse(iterations <= 250, 0.5, 0.8)
  
  df_mom <- data.frame(
    iteration = iterations,
    momentum = momentum
  )
  
  p_mom <- ggplot(df_mom, aes(iteration, momentum)) +
    geom_line(size = 1.5, color = "blue") +
    geom_vline(xintercept = 250, linetype = "dashed") +
    labs(title = "Momentum Schedule",
         x = "Iteration", y = "Momentum") +
    theme_minimal()
  
  ggsave("Figures/momentum_schedule.png", p_mom, width = 8, height = 5, dpi = 100)
}

generate_optimization_viz()

# ===============================================
# 11. BARNES-HUT TREE
# ===============================================

generate_barnes_hut <- function() {
  # Simple quadtree visualization
  set.seed(456)
  n_points <- 100
  points <- data.frame(
    x = runif(n_points),
    y = runif(n_points)
  )
  
  p <- ggplot(points, aes(x, y)) +
    geom_point(size = 2, alpha = 0.6) +
    # Add quadtree divisions
    geom_vline(xintercept = 0.5, linetype = "dashed", alpha = 0.5) +
    geom_hline(yintercept = 0.5, linetype = "dashed", alpha = 0.5) +
    geom_vline(xintercept = c(0.25, 0.75), linetype = "dotted", alpha = 0.3) +
    geom_hline(yintercept = c(0.25, 0.75), linetype = "dotted", alpha = 0.3) +
    annotate("rect", xmin = 0.5, xmax = 0.75, ymin = 0.5, ymax = 0.75,
             fill = "red", alpha = 0.2) +
    annotate("text", x = 0.625, y = 0.625, label = "Treat as\nsingle mass",
             size = 3) +
    ggtitle("Barnes-Hut Approximation: Quadtree") +
    theme_minimal()
  
  ggsave("Figures/barnes_hut_tree.png", p, width = 8, height = 8, dpi = 100)
}

generate_barnes_hut()

# ===============================================
# 12. DEBUGGING VISUAL GUIDE
# ===============================================

generate_debugging_guide <- function() {
  # Create examples of failure modes
  set.seed(789)
  
  # Ball of points
  ball_data <- data.frame(
    x = rnorm(200, 0, 0.1),
    y = rnorm(200, 0, 0.1),
    type = "Ball (LR too low)"
  )
  
  # Scattered points
  scattered_data <- data.frame(
    x = runif(200, -2, 2),
    y = runif(200, -2, 2),
    type = "Scattered (LR too high)"
  )
  
  # Fragmented clusters
  frag_data <- rbind(
    data.frame(x = rnorm(50, -1, 0.1), y = rnorm(50, -1, 0.1)),
    data.frame(x = rnorm(50, 1, 0.1), y = rnorm(50, 1, 0.1)),
    data.frame(x = rnorm(50, -1, 0.1), y = rnorm(50, 1, 0.1)),
    data.frame(x = rnorm(50, 1, 0.1), y = rnorm(50, -1, 0.1))
  )
  frag_data$type <- "Fragmented (Perp too low)"
  
  # Good result
  good_data <- rbind(
    data.frame(x = rnorm(67, -0.8, 0.3), y = rnorm(67, 0, 0.3)),
    data.frame(x = rnorm(67, 0.8, 0.3), y = rnorm(67, 0.8, 0.3)),
    data.frame(x = rnorm(66, 0.8, 0.3), y = rnorm(66, -0.8, 0.3))
  )
  good_data$type <- "Good Result"
  
  all_data <- rbind(ball_data, scattered_data, frag_data, good_data)
  
  p <- ggplot(all_data, aes(x, y)) +
    geom_point(size = 1, alpha = 0.6) +
    facet_wrap(~type, scales = "free") +
    theme_minimal() +
    ggtitle("t-SNE Debugging: Common Failure Modes")
  
  ggsave("Figures/debugging_visual_guide.png", p, width = 12, height = 10, dpi = 100)
  ggsave("Figures/failure_modes_gallery.png", p, width = 12, height = 10, dpi = 100)
}

generate_debugging_guide()

# ===============================================
# 13. PERPLEXITY COMPARISON
# ===============================================

generate_perplexity_comparison <- function() {
  # Generate clustered data
  set.seed(321)
  n <- 600
  
  # Three clear clusters
  cluster1 <- mvrnorm(n/3, mu = c(-5, 0), Sigma = diag(2))
  cluster2 <- mvrnorm(n/3, mu = c(5, 0), Sigma = diag(2))
  cluster3 <- mvrnorm(n/3, mu = c(0, 5), Sigma = diag(2))
  
  data <- rbind(cluster1, cluster2, cluster3)
  labels <- rep(1:3, each = n/3)
  
  # Run t-SNE with different perplexities
  perplexities <- c(5, 30, 100)
  plots <- list()
  
  for(i in seq_along(perplexities)) {
    perp <- perplexities[i]
    tsne_result <- Rtsne(data, perplexity = perp, verbose = FALSE)
    
    df <- data.frame(
      X = tsne_result$Y[,1],
      Y = tsne_result$Y[,2],
      Cluster = as.factor(labels)
    )
    
    plots[[i]] <- ggplot(df, aes(X, Y, color = Cluster)) +
      geom_point(size = 1.5, alpha = 0.7) +
      scale_color_viridis_d() +
      ggtitle(paste("Perplexity =", perp)) +
      theme_minimal() +
      theme(legend.position = "none")
  }
  
  p_combined <- grid.arrange(grobs = plots, ncol = 3)
  ggsave("Figures/perplexity_comparison.png", p_combined, width = 15, height = 5, dpi = 100)
}

generate_perplexity_comparison()

# ===============================================
# 14. INTERPRETATION PITFALLS
# ===============================================

generate_interpretation_sins <- function() {
  set.seed(654)
  
  # Sin 1: Cluster sizes
  cluster1 <- mvrnorm(1000, mu = c(-2, 0), Sigma = diag(2) * 0.5)
  cluster2 <- mvrnorm(100, mu = c(2, 0), Sigma = diag(2) * 0.5)
  
  tsne_data <- rbind(cluster1, cluster2)
  tsne_result <- Rtsne(tsne_data, perplexity = 30, verbose = FALSE)
  
  df_sin1 <- data.frame(
    X = tsne_result$Y[,1],
    Y = tsne_result$Y[,2],
    Size = c(rep("1000 points", 1000), rep("100 points", 100))
  )
  
  p_sin1 <- ggplot(df_sin1, aes(X, Y, color = Size)) +
    geom_point(size = 1, alpha = 0.6) +
    scale_color_manual(values = c("1000 points" = "blue", "100 points" = "red")) +
    ggtitle("Sin #1: Visual Size ≠ True Size") +
    theme_minimal()
  
  ggsave("Figures/sin1_cluster_size.png", p_sin1, width = 8, height = 6, dpi = 100)
  
  # Sin 2: Inter-cluster distances
  set.seed(987)
  cluster_a <- mvrnorm(200, mu = c(-5, 5), Sigma = diag(2) * 0.3)
  cluster_b <- mvrnorm(200, mu = c(5, 5), Sigma = diag(2) * 0.3)
  cluster_c <- mvrnorm(200, mu = c(0, -5), Sigma = diag(2) * 0.3)
  
  all_clusters <- rbind(cluster_a, cluster_b, cluster_c)
  tsne_result2 <- Rtsne(all_clusters, perplexity = 30, verbose = FALSE)
  
  df_sin2 <- data.frame(
    X = tsne_result2$Y[,1],
    Y = tsne_result2$Y[,2],
    Cluster = rep(c("A", "B", "C"), each = 200)
  )
  
  p_sin2 <- ggplot(df_sin2, aes(X, Y, color = Cluster)) +
    geom_point(size = 1, alpha = 0.6) +
    scale_color_viridis_d() +
    ggtitle("Sin #2: Inter-cluster Distance Meaningless") +
    annotate("segment", x = -10, y = -10, xend = 0, yend = -10,
             arrow = arrow(ends = "both"), size = 1) +
    annotate("text", x = -5, y = -12, label = "Distance?", size = 4) +
    theme_minimal()
  
  ggsave("Figures/sin2_distances.png", p_sin2, width = 8, height = 6, dpi = 100)
  
  # Sin 3: Absolute position
  df_sin3 <- df_sin2
  df_sin3$X <- -df_sin3$X  # Flip horizontally
  
  p_sin3 <- ggplot(df_sin3, aes(X, Y, color = Cluster)) +
    geom_point(size = 1, alpha = 0.6) +
    scale_color_viridis_d() +
    ggtitle("Sin #3: Position is Arbitrary") +
    annotate("text", x = 0, y = 10, label = "Top means nothing", size = 4) +
    annotate("text", x = 0, y = -10, label = "Bottom means nothing", size = 4) +
    theme_minimal()
  
  ggsave("Figures/sin3_position.png", p_sin3, width = 8, height = 6, dpi = 100)
}

generate_interpretation_sins()

# ===============================================
# 15. MNIST-LIKE RESULT
# ===============================================

generate_mnist_simulation <- function() {
  set.seed(2024)
  n_per_digit <- 100
  n_digits <- 10
  
  # Simulate MNIST-like clusters
  data_list <- list()
  for(i in 0:9) {
    center_x <- 3 * cos(2 * pi * i / 10)
    center_y <- 3 * sin(2 * pi * i / 10)
    
    data_list[[i+1]] <- data.frame(
      x = rnorm(n_per_digit, center_x, 0.5),
      y = rnorm(n_per_digit, center_y, 0.5),
      digit = as.factor(i)
    )
  }
  
  mnist_data <- do.call(rbind, data_list)
  
  # Add some structure within clusters
  mnist_data$x <- mnist_data$x + rnorm(nrow(mnist_data), 0, 0.1)
  mnist_data$y <- mnist_data$y + rnorm(nrow(mnist_data), 0, 0.1)
  
  p <- ggplot(mnist_data, aes(x, y, color = digit)) +
    geom_point(size = 1.5, alpha = 0.7) +
    scale_color_viridis_d() +
    ggtitle("t-SNE of MNIST Digits (Simulated)") +
    labs(x = "t-SNE 1", y = "t-SNE 2") +
    theme_minimal()
  
  ggsave("Figures/mnist_tsne_result.png", p, width = 10, height = 8, dpi = 100)
}

generate_mnist_simulation()

# ===============================================
# 16. ADDITIONAL VISUALIZATIONS
# ===============================================

# Stability analysis
generate_stability_analysis <- function() {
  set.seed(111)
  
  # Run t-SNE multiple times
  n_runs <- 10
  correlations <- matrix(0, n_runs, n_runs)
  
  # Generate data
  data <- mvrnorm(200, mu = rep(0, 10), Sigma = diag(10))
  
  # Store results
  results <- list()
  for(i in 1:n_runs) {
    set.seed(i * 100)
    results[[i]] <- Rtsne(data, perplexity = 30, verbose = FALSE)$Y
  }
  
  # Calculate pairwise correlations
  for(i in 1:n_runs) {
    for(j in 1:n_runs) {
      correlations[i,j] <- cor(c(results[[i]]), c(results[[j]]))
    }
  }
  
  # Visualize
  df_corr <- melt(correlations)
  names(df_corr) <- c("Run1", "Run2", "Correlation")
  
  p <- ggplot(df_corr, aes(Run1, Run2, fill = Correlation)) +
    geom_tile() +
    scale_fill_viridis(limits = c(-1, 1)) +
    ggtitle("Stability Analysis: Run-to-Run Correlation") +
    theme_minimal()
  
  ggsave("Figures/stability_analysis.png", p, width = 8, height = 7, dpi = 100)
}

generate_stability_analysis()

# Create remaining placeholder images
create_placeholder <- function(filename, title) {
  p <- ggplot(data.frame(x = 0, y = 0), aes(x, y)) +
    annotate("text", x = 0, y = 0, label = title, size = 8) +
    xlim(-1, 1) + ylim(-1, 1) +
    theme_void() +
    theme(plot.background = element_rect(fill = "lightgray"))
  
  ggsave(paste0("Figures/", filename), p, width = 8, height = 6, dpi = 100)
}

# Generate remaining placeholders
remaining_files <- c(
  "heavy_tails_solution_animation.png",
  "interactive_tsne_demo.png",
  "preprocessing_pipeline.png",
  "information_visual.png",
  "information_loss_diagram.png",
  "force_simulation.png",
  "hyperparameter_grid.png",
  "feature_attribution.png",
  "confidence_regions.png",
  "memory_optimization.png",
  "interactive_playground.png",
  "exam_problem.png",
  "complete_journey.png",
  "single_cell_tsne.png",
  "word_embeddings_tsne.png",
  "imagenet_tsne.png",
  "parametric_tsne_architecture.png",
  "multiscale_tsne.png",
  "dynamic_tsne.png",
  "different_df_comparison.png",
  "failed_embedding.png",
  "fixed_embedding.png",
  "fraud_detection_tsne.png",
  "acceleration_comparison.png",
  "streaming_tsne.png",
  "cross_validation_tsne.png",
  "ensemble_methods.png",
  "genomics_icon.png",
  "nlp_icon.png",
  "vision_icon.png"
)

for(file in remaining_files) {
  if(!file.exists(paste0("Figures/", file))) {
    create_placeholder(file, paste("Placeholder:", file))
  }
}

cat("\nAll images generated successfully in 'Figures' directory!\n")
cat("Total images created:", length(list.files("Figures", pattern = "*.png")), "\n")