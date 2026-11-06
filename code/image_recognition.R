# Image recognition with Tesseract OCR

# PREPARATIONS ------------------------------------------------------------

# It's good to start scripts with a dedicated "prep work" section, where you
# - install and load all packages at once
# - specify your options
# - intialise function if needed
# - load external data, for example tesseract language packs

# We'll start off by cleaning the environment with rm
rm(list = ls())  # ls(): object names

# Options: show fractions as decimals, but restrict them to three decimal places
options(scipen = 999, digits = 3)

# Install and load packages with pacman library p_load
# First checks whether you alreadyhav
if(!require("pacman")) { install.packages("pacman") }

pacman::p_load(
  dplyr,         # For data manipulation
  tidyr,         # For data manipulation
  stringr,       # For data manipulation
  magick,        # For handling images
  tesseract,     # For optical character recognition in images
  hunspell       # For checking words
)

sessionInfo() # Prints out everything about our session

# Initialise Tesseract by downloading English and Finnish models
tesseract_download("eng", model = "best", progress = interactive())
tesseract_download("fin", model = "best", progress = interactive())

# Load our dataset of social media images

# Let's open one and see what's inside
magick::image_read()


# Add "good manners" to functions querying urls with polite::bow()
polite_image_read <- politely(magick::image_read)

ocr_img <- function(image = "http://jeroen.github.io/images/testocr.png",
                    grayscale = TRUE,
                    scale = "1500"
){

  # Iterate through a few different settings
  parameter = c(1, 10, 50, 75) # to control convolution kernel settings

  output_ocr <- vector(mode = "list", length(parameter))

  for(j in seq_along(parameter)) {

    image_ocr <- image %>%

      polite_image_read() %>%

      # Sharpen image along edges with Difference of Gaussians kernel convolution
      image_convolve('DoG:0,0,5', scaling = paste(parameter[j], "100%", sep = ", ")) %>%
      image_convert(type = 'Grayscale') %>%
      image_scale(scale) %>%

      image_write(format = 'png', density = '300x300') %>%

      tesseract::ocr_data() %>%

      # Strip punctuation to improve consistent spelling across iterations
      mutate(word = gsub("[[:punct:]]", "", word)) %>%

      select(word, confidence) %>%
      rename_with( ~ paste(.x, j, sep = "."), starts_with("confidence"))

    output_ocr[[j]] <- image_ocr

  }

  # FIXME TEMP! Crop image based on averaging the bounding boxes ("mid point")

  # image_ocr %>%
  #   select(bbox) %>%
  #   separate_wider_delim(bbox, delim  = ",", names = c("one", "two", "three", "four")) %>%
  #   sapply(as.numeric) %>%
  #   colMeans()

  # Merge list elements together by key word column; keep word order!
  image_ocr <-
    purrr::reduce(
      .x = output_ocr, merge, by = c("word"), all = TRUE, sort = FALSE
    ) %>%

    # Filter out words that do not appear across all model runs
    filter(complete.cases(.)) %>%

    # Summarise average confidence levels across different runs
    rowwise() %>%
    mutate(average = mean(c_across(where(is.numeric)))) %>%

    # Add two columns: use hunspell to check whether a word makes sense
    mutate(check = hunspell_check(c_across(where(is.character)))) %>%

    # And extract the first suggestion to remedy it
    mutate(
      suggest = hunspell_suggest(c_across(where(is.character))))

  # FIXME: I DON'T KNOW WHY IT DOES NOT WORK
  image_ocr$suggest_lengths <- lengths(image_ocr$suggest)
  image_ocr <- image_ocr %>% filter(suggest_lengths > 0)
  image_ocr$suggest <- sapply(image_ocr$suggest, `[[`, 1)


  # Correct bad suggestions
  ocr_suggest <- c("Al", "A1", "And", "and", "EU", "Europe", "The", "the",
                   "Meta", "Google", "Uber", "TikTok", "TIKTOK")
  correct     <- c("AI", "AI", "And", "and", "EU", "Europe", "The", "the",
                   "Meta", "Google", "Uber", "TikTok", "TIKTOK")

  correct_suggestion <- tibble(ocr_suggest, correct)

  image_ocr <- image_ocr %>%
    left_join(correct_suggestion, by = c("word" = "ocr_suggest")) %>%
    mutate(suggest = case_when(
      !is.na(correct) ~ correct,
      .default = suggest)
    ) %>%
    select(-correct)

  # Use suggestions when needed:
  if(all(image_ocr$check & image_ocr$average > 90)) {

    image_ocr <- image_ocr %>%
      # Discard single character words except for indefinite articles
      filter(!grepl("^[^aA]$", word)) %>%
      pull(word) %>%
      paste(collapse = " ")

  } else {

    image_ocr <- image_ocr %>%
      filter(!grepl("^[^aA]$", word)) %>%
      mutate(replacement = case_when(
        !check & average < 80 ~ suggest,
        .default = word)
      ) %>%
      pull(replacement) %>%
      paste(collapse = " ")

  }
}



