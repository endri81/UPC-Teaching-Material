# === MODULE 2: Understanding t-SNE Components ===
# Time: 20 minutes

# === SECTION 2.1: From Distances to Probabilities (7 min) ===

# Create a tiny dataset for manual calculation
tiny_data <- iris_data[c(1, 51, 101), ]  # One from each species
rownames(tiny_data) <- c("Setosa", "Versicolor", "Virginica")

# Compute distances
tiny_dist <- as.matrix(dist(tiny_data))
print("Distance matrix:")
print(round(tiny_dist, 2))

# Convert to similarities (Gaussian kernel)
sigma <- 1.0  # Fixed for demonstration
compute_similarities <- function(dist_matrix, sigma) {
  exp(-dist_matrix^2 / (2 * sigma^2))
}

similarities <- compute_similarities(tiny_dist, sigma)
diag(similarities) <- 0  # No self-similarity
print("Similarity matrix:")
print(round(similarities, 3))

# Normalize to probabilities (row-wise)
compute_probabilities <- function(similarities) {
  P <- similarities / rowSums(similarities)
  return(P)
}

P_conditional <- compute_probabilities(similarities)
print("Conditional probability matrix P(j|i):")
print(round(P_conditional, 3))

# Symmetrize
P_joint <- (P_conditional + t(P_conditional)) / (2 * nrow(P_conditional))
print("Joint probability matrix P_ij:")
print(round(P_joint, 3))

# === SECTION 2.2: Perplexity and Bandwidth Selection (7 min) ===

# Function to compute perplexity
compute_perplexity <- function(P) {
  H <- -sum(P * log2(P + 1e-10))  # Shannon entropy
  2^H  # Perplexity
}

# Demonstrate perplexity for different sigma values
sigmas <- seq(0.1, 5, length.out = 20)
perplexities <- numeric(length(sigmas))

for (i in seq_along(sigmas)) {
  sim <- compute_similarities(tiny_dist[1, -1], sigmas[i])
  prob <- sim / sum(sim)
  perplexities[i] <- compute_perplexity(prob)
}

perp_df <- data.frame(sigma = sigmas, perplexity = perplexities)

ggplot(perp_df, aes(x = sigma, y = perplexity)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 1.5, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Perplexity vs Bandwidth (σ)",
       x = "Bandwidth σ", 
       y = "Perplexity",
       subtitle = "Red line shows target perplexity = 1.5")

# === SECTION 2.3: Student-t Distribution in Low-D (6 min) ===

# Compare Gaussian and Student-t kernels
distances <- seq(0, 5, length.out = 100)
gaussian_kernel <- exp(-distances^2)
student_t_kernel <- (1 + distances^2)^(-1)

kernel_df <- data.frame(
  distance = rep(distances, 2),
  similarity = c(gaussian_kernel, student_t_kernel),
  kernel = rep(c("Gaussian", "Student-t"), each = length(distances))
)

ggplot(kernel_df, aes(x = distance, y = similarity, color = kernel)) +
  geom_line(size = 1.2) +
  theme_minimal() +
  labs(title = "Gaussian vs Student-t Kernels",
       subtitle = "Student-t has heavier tails, allowing more space for moderate distances",
       x = "Distance in 2D embedding",
       y = "Similarity") +
  annotate("text", x = 3, y = 0.3, label = "More space\nfor moderate\ndistances", 
           color = "red", size = 3)

# EXERCISE 2: Compute KL divergence
# Given P and Q, compute KL(P||Q)
P_example <- c(0.7, 0.2, 0.1)
Q_example <- c(0.5, 0.3, 0.2)

kl_divergence <- function(P, Q) {
  # Your code here:
  # Hint: sum(P * log(P/Q))
  _____________
}

kl_value <- kl_divergence(P_example, Q_example)
cat("KL divergence:", round(kl_value, 4), "\n")