# Generate word2vec t-SNE visualization - CORRECTED
library(Rtsne)
library(ggplot2)
library(ggrepel)  # for better label placement

# Simulate word embeddings
set.seed(42)
n_dims <- 50  # After dimensionality reduction

# Create semantic categories with more words for proper perplexity
categories <- list(
  countries = c("USA", "France", "Germany", "Japan", "Brazil", "Canada", "Italy", "Spain",
                "China", "India", "Russia", "Mexico", "Australia", "Egypt", "Nigeria"),
  animals = c("dog", "cat", "elephant", "lion", "tiger", "bear", "mouse", "rabbit",
              "horse", "cow", "sheep", "pig", "bird", "fish", "snake"),
  verbs = c("run", "walk", "jump", "swim", "fly", "eat", "sleep", "think",
            "write", "read", "speak", "listen", "watch", "play", "work"),
  colors = c("red", "blue", "green", "yellow", "orange", "purple", "black", "white",
             "brown", "gray", "pink", "cyan", "magenta", "beige", "navy"),
  food = c("pizza", "burger", "salad", "pasta", "rice", "bread", "cheese", "apple",
           "banana", "chicken", "beef", "fish", "soup", "cake", "coffee"),
  tech = c("computer", "phone", "internet", "software", "data", "algorithm", "code", "app",
           "website", "database", "network", "server", "cloud", "AI", "robot"),
  profession = c("doctor", "teacher", "engineer", "lawyer", "artist", "scientist", "nurse",
                 "pilot", "chef", "writer", "musician", "athlete", "farmer", "driver", "manager")
)

# Generate embedding data with semantic clustering
all_words <- unlist(categories)
n_words_actual <- length(all_words)
print(paste("Total words:", n_words_actual))  # Should be > 100 for perplexity=30

data <- matrix(rnorm(n_words_actual * n_dims), nrow = n_words_actual)

# Add structure based on categories
word_categories <- rep(names(categories), sapply(categories, length))
for(i in 1:length(categories)) {
  cat_name <- names(categories)[i]
  idx <- which(word_categories == cat_name)
  # Shift each category in embedding space
  data[idx, ] <- data[idx, ] + rnorm(n_dims, mean = i*3, sd = 0.5)
}

# Calculate appropriate perplexity
max_perplexity <- floor((n_words_actual - 1) / 3)
perplexity_value <- min(30, max_perplexity)
print(paste("Using perplexity:", perplexity_value))

# Run t-SNE with appropriate perplexity
tsne_result <- Rtsne(data, 
                     perplexity = perplexity_value, 
                     verbose = TRUE, 
                     max_iter = 2000,
                     check_duplicates = FALSE)

# Create dataframe for plotting
df <- data.frame(
  x = tsne_result$Y[,1],
  y = tsne_result$Y[,2],
  word = all_words,
  category = word_categories
)

# Create visualization with selective labeling
# Only label a few representative words from each category to avoid overcrowding
representative_words <- c("USA", "France", "China",
                          "dog", "elephant", "lion",
                          "run", "think", "write",
                          "computer", "algorithm", "AI",
                          "pizza", "apple", "coffee",
                          "doctor", "engineer", "artist",
                          "red", "blue", "green")

df$label <- ifelse(df$word %in% representative_words, df$word, "")

p <- ggplot(df, aes(x = x, y = y, color = category)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_text_repel(aes(label = label), 
                  size = 3, 
                  max.overlaps = 20,
                  segment.size = 0.2,
                  point.padding = 0.5) +
  theme_minimal() +
  labs(title = "t-SNE Visualization of Word Embeddings",
       subtitle = paste("Semantic clustering of", n_words_actual, "words"),
       x = "t-SNE 1", 
       y = "t-SNE 2") +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  scale_color_brewer(palette = "Set2")

ggsave("./Figures/word2vec_tsne.png", p, width = 10, height = 8, dpi = 150)

print("Plot saved as word2vec_tsne.png")