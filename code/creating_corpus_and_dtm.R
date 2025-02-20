
# PREPARATIONS #################################################################
pacman::p_load(readr,
               dplyr,
               tidyr,
               purrr,
               tm,
               tidytext,
               SnowballC,
               stopwords,
               quanteda,
               textstem,
               tidytext)

options(max.print=999)

sessionInfo()

# Substitute with your text file directory, for example
setwd(r"(Z:\tkjheikk_research\yle-nato-output)")

# CREATE CORPUS FROM TEXT FILES  ###############################################

# List the text files in your current working directory
text_files <- list.files(pattern = ".txt$")  # Indicated by extension *.txt

# Read and clean text documents
documents <-

  # Load text files as data frame columns with tibble function
  map_df(text_files, ~ tibble(

    # Read text files with readLines, drop first and last lines (as redundant)
    # Collapse lines (vectors) into a single string
    # text = paste(head(readLines(.x, warn = F)[-1], -1), collapse = " ")) %>%
    text = paste(readLines(.x, warn = F), collapse = " ")) %>%

      # Name documents (doc_id) by their file name (identifiers in Yle News)
      mutate(doc_id = tools::file_path_sans_ext(.x), .before = everything()))

glimpse(documents)

# Write as a csv file
# write_csv2(documents, "documents_yle_nato.csv")

# Create a corpus with tm package
corpus <- Corpus(DataframeSource(as.data.frame(documents)), # Coerce as data frame first
          readerControl = list(language = "fi")) # Language: Finnish

# Inspect corpus (text content and meta data) by index number or a doc_id string
corpus[[2]]$content
corpus[[2]]$meta
corpus[["74-20070594"]]$content

# Create an initial Document-Term Matrix
dtm_dumb <- DocumentTermMatrix(corpus)

inspect(dtm_dumb)

### PREPROCESS CORPUS INTO DOCUMENT-TERM MATRIX ################################

# Create a custom function for stemming finnish words
fin_stem <- function(x) { stemDocument(x, "finnish") }

# If you want, you can add whichever additional stop words to your lists
fin_stopwords <- stopwords::stopwords("fi", source = "nltk") # NLTK stop words
add_stopwords <- c("asti", "lähtien", "näin", "myös", "eikä", "näin",
                   # Yle news specific
                   "yle", "kuva:", "tekijä", "lähteet: ", "•päivitetty")

fin_stopwords <- append(fin_stopwords, add_stopwords)
fin_stopwords

# Preprocess corpus: lowercase
corpus_prep <- corpus %>%
  tm_map(content_transformer(tolower)) %>%

  # Remove custom phrases (here Yle News specific)
  tm_map(removeWords, c("toteutus vaati toimiakseen javascriptin",
                        "avaa kuvien katselu")) %>%

  # Optional: if you want to remove stopwords
  tm_map(removeWords, fin_stopwords) %>%

  # Remove punctuation, numbers and repeated whitespaces
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%

  # Create equivalence classes: stem words with finnish word stemming algorithm
  # from SnowballC package
  tm_map(function(x) stemDocument(x, "finnish"))

# Inspect preprocessed results
corpus_prep[[2]]$content

documents_prep <- tibble(doc_id = NLP::meta(corpus_prep, "id"),
                         text = NLP::content(corpus_prep))

# Write as a csv file
# write_csv2(documents_prep, "documents_prep_yle_nato.csv")

# Create smarter Term-Document Matrix
dtm_prep <-
  DocumentTermMatrix(corpus_prep) # DTM with simple term frequencies

# DTM weighted by term frequency - inverse document frequency
dtm_prep_tfidf <- DocumentTermMatrix(
  corpus_prep, control = list(
    weighting = function(x) weightTfIdf(x, normalize = FALSE)))

inspect(dtm_prep)
inspect(dtm_prep_tfidf)

# Write as a file
# write_csv2(tidy(dtm_prep), "tidydtm_prep_yle_nato.csv")

# Rank top 10 tf-idf values per document
top_tf_idf <- dtm_prep_tfidf %>%
  tidy() %>%
  rename("tf_idf" = "count") %>%
  group_by(document) %>%
  slice_max(tf_idf, n = 10, with_ties = FALSE) %>%
  mutate(rank = rank(desc(tf_idf), ties.method = "first"))

# Write as a csv file
# write_csv2(documents_prep, "tf_idf_yle_nato.csv")

# List top values per document (descending order)
aggregate(term ~ document, top_tf_idf,
          function(x) paste(x, collapse = ", "), drop = FALSE)

# Compare to the most common words per document (based on term frequency alone)
findMostFreqTerms(dtm_prep, 10)

#### TRIM TERMS FROM THE MATRIX ################################################

# See which documents appear the most: e.g. 99.9% of all the documents in corpus
findFreqTerms(dtm_prep, 0.999 * nDocs(dtm_prep))

# Trim the most common and rare terms (top 99.9 perc and bottom 0.1 perc) with
# quanteda package trim function. We first need to turn the DTM into DFM
dfm_trim <-
  dfm_trim(quanteda::as.dfm(dtm_prep),
           min_docfreq = 0.001, max_docfreq = 0.999, docfreq_type = "prop",
           verbose = TRUE)

dfm_trim

# Alternatively: trim terms that appear only once or in every document:
findFreqTerms(dtm_prep, lowfreq = nDocs(dtm_prep)) # words in every document
findFreqTerms(dtm_prep, lowfreq = 1, highfreq = 1) # words that appear only once

dfm_trim <-
  dfm_trim(quanteda::as.dfm(dtm_prep),
          min_termfreq = 2, max_termfreq = tm::nDocs(dtm_smart), verbose = TRUE)

dfm_trim

# Then reverse conversion via tidytext tidy and cast_dtm functions
dtm_trim <- dfm_trim %>% tidy() %>% cast_dtm(document, term, count)
inspect(dtm_trim)

# Write as a csv file
# write_csv2(documents_prep, "tf_idf_yle_nato.csv")
