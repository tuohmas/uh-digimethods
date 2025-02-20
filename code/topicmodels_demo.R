# PREPARATIONS
# Clean the environment
rm(list = ls())

# Load packages
pacman::p_load(dplyr,
               tidyr,
               readr,
               purrr,
               tidytext,
               tm,
               ggplot2,
               gridExtra,
               stringr,
               topicmodels)

options(max.print = 999)

sessionInfo()
getwd()

# LOAD DATA ####################################################################

# Load Yle news corpus queried with the keyword "nato" from Jan 2024 - Jan 2025
# Document-Term matrix has been preprocessed by removing punctuation, numbers,
# stop words, and extremely common and rare terms (top 99.9 percent and bottom
# 0.1 percent)

dtm_trim <- read_csv2("data/tidydtm_prep_yle_nato.csv")
glimpse(dtm_trim)

# Do some extra cleaning for most common words
dtm_trim %>% count(term) %>%
  arrange(desc(n)) %>% glimpse()

dtm_trim <- dtm_trim %>%
  # Filter out the most common words
  filter(!term %in% c("–", "kuva", "nato", "kuuntel", "jutu", "aihe"))

# Use tidytext cast_dtm function to prepare tidy data frame for LDA
dtm_trim <- dtm_trim %>% cast_dtm(term = term, document = document,
                                  value = count)
inspect(dtm_trim)
tm::findFreqTerms(dtm_trim, nDocs(dtm_trim) * 0.50)

# Document metadata: id, url, title, date of publication
document_metadata <- read_csv2("data/yle-nato-clean.csv")
glimpse(document_metadata)

# TOPIC MODELING USING LDA #####################################################

# Create multiple LDA models with different topic numbers

k_range <- c(2, seq(from = 5, to = 25, by = 5))

# Iterate over topic numbers
for (k in k_range) {

  model <- LDA(dtm_trim, k, control = list(seed = 1234))

  write_rds(model, paste0("models/LDA_k", k, ".rds"))

}

# Inspect the properties of LDA model with 5 topics
LDA_k15 <- readRDS("models/LDA_k15.rds")

# Inspect word-topic (beta) and Document-topic (gamma) probabilities
topic_words <- tidy(LDA_k15, matrix = "beta")
topic_documents <- tidy(LDA_k15, matrix = "gamma")

# Visualize top per-topic-per-word probabilities to inspect topics
top_terms <- topic_words %>%
  group_by(topic) %>%
  # filter(topic %in% c(1,2,3,4,5)) %>%
  slice_max(beta, n = 15) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

# Visualize top per-topic-per-word probabilities to inspect topics
topic_documents <-
  topic_documents %>%
  # Add titles from the metadata file using left_join
  left_join(
    document_metadata %>% select(id, title), by = c("document" = "id")) %>%
  # Print the 80 characters of the title
  mutate(title = stringr::str_trunc(title, 80))

top_documents <- topic_documents %>%
  group_by(topic) %>%
  # filter(topic %in% c(1,3)) %>% # Optional
  slice_max(gamma, n = 15) %>%
  ungroup() %>%
  arrange(topic, -gamma)

View(top_documents)

top_documents %>%
  mutate(title = reorder_within(title, gamma, topic)) %>%
  ggplot(aes(gamma, title, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

# Topic proportions in five randomly chosen documents

set.seed(654321)
sample <- topic_documents %>% distinct(document) %>%
  slice_sample(n = 10) %>%
  pull()

topic_documents %>%
  filter(document %in% sample) %>%
  mutate(title = stringr::str_trunc(title, 50)) %>%
  ggplot(aes(y = gamma, x = factor(topic), fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ title, scales = "free")

# Filter documents based on topic-proportion threshold (exclusive)
gamma_threshold <- 0.5

topic_documents %>%
  filter(gamma > gamma_threshold) %>%
  filter(topic == 1)

topic_documents %>%
  filter(gamma > gamma_threshold, topic == 15) %>%
  arrange(desc(gamma)) %>%
  mutate(title = stringr::str_trunc(title, 30)) %>%
  ggplot(aes(gamma, title, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

# OPTIMAL TUNING FOR LDA MODEL PARAMETERS ######################################

# We can try to identify the optimal number of topics in a text corpus by
# using Log-likelihood and perplexity (see Nelimarkka, 2019)

# Apply a function that goes though k values from 2 through 75 in increments of 5

# Initialize an empty data frame
best_model_metrics <- data.frame(k = as.numeric(),
                                 LL = as.numeric(),
                                 perplexity = as.numeric())

best_model_metrics <- read_csv2("data/lda_tuning.csv")
best_model_metrics <- best_model_metrics[-c(1:2),]

k_range <- c(2, seq(from = 5, to = 75, by = 5))
k_range <- seq(from = 45, to = 90, by = 5)

# Iterate though k_range
for (k in k_range) {

  model <- LDA(dtm_trim, k, control = list(seed = 1234))

  write_rds(model, paste0("models/LDA_k", k, ".rds"))

  LL <- logLik(model) %>% as.numeric()
  perplexity <- perplexity(model, newdata = dtm_trim, estimate_theta = F)

  best_model_metrics <-
    rbind(best_model_metrics, data.frame(k, LL, perplexity))

  # Glimpse topic LL pairs
  glimpse(best_model_metrics)

  # Write model metrics as csv file
  write_csv2(best_model_metrics, "data/lda_tuning.csv")

  # Plot results as they roll in
  plot_LL <- ggplot(best_model_metrics, aes(x = k, y = LL)) +
    xlab("Number of topics") + ylab("Log-likelihood of the model (maximize)") +
    geom_line() +
    theme_bw()

  plot_perp <- ggplot(best_model_metrics, aes(x = k, y = perplexity)) +
    xlab("Number of topics") + ylab("Perplexity of the model (minimize)") +
    geom_line() +
    theme_bw()

  grid.arrange(plot_LL, plot_perp, ncol = 2)
}
