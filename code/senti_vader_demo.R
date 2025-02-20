# PREPARATIONS
# Clean the environment
rm(list = ls())

# Load packages
pacman::p_load(dplyr,
               tidyr,
               readr,
               purrr,
               tidytext,
               quanteda,
               quanteda.textmodels,
               tm,
               ggplot2,
               sotu,
               vader)

options(max.print = 999)

sessionInfo()
getwd()

# LOAD DATA ####################################################################

# Presidential inaugurals
inaugurals <- quanteda::data_corpus_inaugural
summary(inaugurals)
head(docvars(inaugurals), 10)

# Tokenize by sentence and convert to tidy format via Document-Feature Matrix
tidy_docs <- inaugurals %>%
  tokens(what = "sentence") %>%
  dfm() %>%
  tidy() %>%
  rename("sentence" = "term") %>%
  select(-count)

# Add meta data
tidy_docs <- tidy_docs %>%
  left_join(data_corpus_inaugural %>%
              summary() %>%
              tibble() %>%
              rename_with(tolower) %>%
              select(text, year, president, party),
            by = c("document" = "text")) %>%
  relocate(sentence, .after = party)

setwd("data/")
getwd()

# Add Trump's 2025 inaugural
trump_inaugrual2025 <- list.files(pattern = "trump_inaugrual_2025.txt")

trump_2025 <-
  map_df(trump_inaugrual2025,
         ~ tibble(
           document = "2025-Trump",
           text = paste(readLines(.x, warn = F), collapse = " ")))

trump_2025 <- trump_2025 %>%
  mutate(text = gsub("\\(.+?\\)", " ", text),
         year = 2025,
         president = "Trump",
         party = "Republican")

glimpse(trump_2025)

# Add Trump 2025 inaugural tokenized by sentences (using tidytext)
tidy_docs <- trump_2025 %>%
  unnest_tokens(output = sentence, input = text, token = "sentences") %>%
  bind_rows(tidy_docs, .)

#######

vader_senti <- vader_df(tidy_docs$sentence)
senti_docs <- tidy_docs %>% cbind(vader_senti %>% select(-text))

View(senti_docs)

senti_docs %>%
  arrange(compound) %>%
  select(document, sentence, compound, pos, neu, neg) %>%
  head(n = 50)

# Write as csv file
write_csv2(senti_docs, "inaugural_sentiments.csv")

senti_docs_aggregate <- senti_docs %>%
  group_by(year) %>%
  summarise(
    sum_compound = sum(compound),
    sum_pos = sum(pos),
    sum_neg = sum(neg)
    )

senti_docs_aggregate <- senti_docs_aggregate %>%
  gather(key = "senti_type", value = "score", sum_compound, sum_pos, sum_neg)

# Visualize aggregate sentiments over time
senti_docs_aggregate %>%
  ggplot(aes(x = year, y = score,
             color = factor(senti_type,
                            levels = c("sum_neg", "sum_pos", "sum_compound")))) +
  geom_line() +
  theme_minimal() +
  labs(colour = "Sentiment score types")
