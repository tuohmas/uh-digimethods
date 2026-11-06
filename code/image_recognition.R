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
image_dir <- "data/images/YouTube thumbnails"

images <- list.files(image_dir, pattern = "*.png", full.names = TRUE)

lowres_images <- images[grepl("lowres", images)]
maxres_images <- images[grepl("maxres", images)]

# IMAGE RECOGNITION: First attempts ----------------------------------------

# The Tesseract OCR engine works by taking images and spitting out plain text.
# Here is a demo from the library developer
text <- ocr("https://jeroen.github.io/images/testocr.png")
cat(text)

# We can convince ourself that this works by validating the image by ourselves
demo_image <- "https://jeroen.github.io/images/testocr.png"
# Use magick library to read and display the images
magick::image_read(demo_image)

# Nice. Now we can try it with our onw dataset of YouTube thumbnails

# Let's open the first high-resolution with magick
magick::image_read(maxres_images[1])

# The image reads to us quite clearly. Let's store that separately
ground_truth <- "you can't own the minor scale!"

# Now let's see what tesseract can read from it
image <- magick::image_read(maxres_images[1])

tesseract::ocr(image)

# Not great :( And this is with a high resolution image
# Let's run just a few more examples to see how bad the situation is.
# For the sake of the exercise, let's sample 5 images and evaluate them in a
# for loop:

set.seed(5462)
image_sample <- maxres_images[sample(1:length(maxres_images), 5)]

seq_along(image_sample) # seq_along: a range from 1 to length of the vector

for(i in seq_along(image_sample)) { # Iterate through indexes 'i', start with {

  result <- ocr(image_sample[i])    # Nth, or 'ith' element of the range
  message(result)                   # Use message to print inside of the loop

} # Curly brackets ends the loop

# Only few OCRs are intelligble :((

# Let's see if we can manipulate the image to read better
image_bw <- image %>%
  # Convert to greyscale
  image_convert(type = 'Grayscale') %>%
  # Increase the image density
  image_write(format = 'png', density = '300x300')

# Finally run the OCR function again
ocr(image_bw)

# Still quite bad. Let's investigate where the problem further
image_ocr <- tesseract::ocr_data(image_bw)
print(image_ocr)

# Some stats about the model's confidence
summary(image_ocr$confidence)

# Potential fixes:
# - The text is tilted. Would rotating it help?
# - White text on black might also confuse the model. Could we invert colors?
# - Maybe the text is too blurry. Could we make it crisper?
# - Having a better contrast would also help. Can I brighten the background?

ocr_data <- image %>%

  # Convert to greyscale (already works)
  image_convert(type = 'Grayscale') %>%

  # Rotate clockwise by a few degrees: make the text level
  image_rotate(2) %>%

  # Negate the colors: black text on white background
  image_negate() %>%

  # Sharpen the image along the edges: crisper text
  # (See https://ropensci.org/blog/2017/11/02/image-convolve/)
  image_convolve('DoG:0,0,2', scaling = "100, 100%") %>%

  # Let's increase the contrast and brightness as well
  image_contrast(sharpen = 1) |>
  image_modulate(brightness = 150) |>

  # Increase the image density
  image_write(format = 'png', density = '300x300') %>%

  # Calculate OCR: return a dataframe with predicted words and confidence values
  ocr_data() |>

  # Change the word column: remove punctuations in the beginning or the end
  mutate(word = gsub(   # Replace using Regular Expressions (RegEx)
    "[^[:alnum:]']",    # Reads: "find everything else but alphabets, numbers, or character ' "
    "",                 # Replace with: an empty string of characters
    word)) |>           # Replace weha: all the values in the word column

  # Filter out odd, one-character words
  filter(nchar(word) > 1)

ocr_data

# NEXT STAGE: Average OCR over many iterations ----------------------------

# Much better! Playing around with the parameters, we also notice that different
# values give better values on different words. Let's try to turn this to our
# advantage.

# Define a range of rotation angles
rotations <- -3:3

# Let's create a for loop which tries all different rotations in the range
ocr_iterations <- data.frame()

for (i in seq_along(rotations)) { # i as in all indeces or elements in a range

  ocr_iteration <- image |>

    # instead of one angle, every iteration 'i' selects a new angle from the
    # rotations parameter vector: -3 (CCW), -2, -1, 0 (No angle), 1 (CW), 2, 3.
    image_rotate(rotations[i]) |>

    # The next steps are the same as before
    image_convert(type = 'Grayscale') |>
    image_negate() |>
    image_convolve('DoG:0,0,2', scaling = "100, 100%") |>
    image_contrast(sharpen = 1) |>
    image_modulate(brightness = 150) |>
    image_scale("1280") |>
    image_write(format = 'png', density = '300x300') |>

    ocr_data() %>%
    mutate(word = gsub("[^[:alnum:]]", "", word)) |>
    mutate(word = tolower(word))  |>
    filter(nchar(word) > 1) |>

    # For later we also need a sense of the word order
    mutate(order = row_number(), .after = word)

  # View the result
  glimpse(ocr_iteration)

  # Print out the average confidence score
  message(paste0("Tesseract is on average ",
                 round(mean(ocr_iteration$confidence), 2),
                 "% on this OCR."))

  # Bonus: let's compare the results to the ground truth value
  ocr_suggestion <- paste(ocr_iteration$word, collapse = " ")

  # adist function shows how far off are we
  distance <- adist(ocr_suggestion, ground_truth, partial = TRUE) |>
    as.numeric()

  message(paste0("We are still ", distance, " characters off."))

  # Add this iterations to the end of the dataframe
  ocr_iterations <- bind_rows(ocr_iterations, ocr_iteration)

} # Curly brackets close off the for loop

# Let's look at our iterations
ocr_iterations

# Count how many of each word did we get by sumarising
ocr_summary <- ocr_iterations |>
  group_by(word) |>
  summarise(counts = n(),
            counts_normalised = counts / length(rotations), # Advanced
            mean_confidence = mean(confidence),
            mean_order = mean(order)) |>
  arrange(desc(counts), mean_order)

print(ocr_summary, n = Inf)

closest_ocr <- ocr_summary |>
  filter(counts > 1) |>
  arrange(mean_order) |>
  pull(word) |>
  paste(collapse = " ")

closest_ocr

adist(closest_ocr, ground_truth) |>
  as.numeric() / nchar(ground_truth)

# Note that by we lose the bounding boxes when summarising

# NEXT STAGE: Spellchecker ------------------------------------------------

# Use hunspell library to check which words seem to be spelled correctly
hunspell(closest_ocr) # Our best match so far contains this many misspellings

ocr_summary$word
spell_check <- hunspell_check(ocr_summary$word)
ocr_summary$spell_check <- spell_check

ocr_summary |>
  filter(spell_check)

# We can also use hunspell to suggest us alternatives
word_suggestions <- hunspell_suggest(ocr_summary$word) |>
  sapply(`[[`, 1)
ocr_summary$word_suggestions <- word_suggestions

# Now let's change word column to suggestion if the spell_chek column is FALSE:
# so, replace words that aren't words with hunspell closest suggestions
ocr_summary <- ocr_summary |>
  mutate(word = ifelse(   # Change with a condition:
    spell_check == FALSE, # If: spell check says FALSE
    word_suggestions,     # Then: change the value to word suggestion
    word                  # Else: keep the word as it is
    ))

# Now let's summarise the words again
ocr_summary <- ocr_summary |>
  group_by(word) |>
  summarise(counts = sum(counts),
            counts_normalised = counts / length(rotations),
            mean_confidence = mean(mean_confidence),
            mean_order = mean(mean_order)) |>
  arrange(desc(counts), mean_order)

# Choose a cut-off point, counts or confidence, preferably after validating with different images
final_ocr <- ocr_summary |>
  filter(counts > 2) |>
  arrange(mean_order) |>
  pull(word) |>
  paste(collapse = " ")

final_ocr
ground_truth

adist(final_ocr, ground_truth)

# FINAL STAGE: Put it in a function ---------------------------------------

# Finally, let's put everything into one function to use it with the other
# images. This function needs at least two arguments: the image and the
# rotations from earlier. Let's add some other parameters as well.

better_ocr <- function(image, rotations = -3:3, brightness = 150, sharpen = 1) {

  rotations <- rotations

  # Instead of a empty data frame it is quicker to use a list
  ocr_iterations <- vector(mode = "list", length(rotations))

  for (i in seq_along(rotations)) {

    ocr_iteration <- image |>

      # Let's add a step where we read the image as well:
      image_read() |>

      image_rotate(rotations[i]) |>
      image_convert(type = 'Grayscale') |>
      image_negate() |>
      image_convolve('DoG:0,0,2', scaling = "100, 100%") |>
      image_contrast(sharpen = sharpen) |>
      image_modulate(brightness = brightness) |>
      image_scale("1280") |>
      image_write(format = 'png', density = '300x300') |>

      ocr_data() %>%
      mutate(word = gsub("[^[:alnum:]]", "", word)) |>
      mutate(word = tolower(word))  |>
      filter(nchar(word) > 1) |>

      mutate(order = row_number(), .after = word)

    # View the result
    glimpse(ocr_iteration)

    # Print out the average confidence score
    message(paste0("Tesseract is on average ",
                   round(mean(ocr_iteration$confidence), 2),
                   "% on this OCR."))

    # Add this iterations to the end of the dataframe
    ocr_iterations[[i]] <- ocr_iteration

  }

  ocr_iterations <- do.call("bind_rows", ocr_iterations)

  ocr_summary <- ocr_iterations |>
    group_by(word) |>
    summarise(counts = n(),
              counts_normalised = counts / length(rotations), # Advanced
              mean_confidence = mean(confidence),
              mean_order = mean(order)) |>
    arrange(desc(counts), mean_order)

  ocr_summary$spell_check <- hunspell_check(ocr_summary$word)

  word_suggestions <- hunspell_suggest(ocr_summary$word) |>
    sapply(`[[`, 1)

  ocr_summary$word_suggestions <- word_suggestions

  ocr_summary <- ocr_summary |>
    mutate(word = ifelse(   # Change with a condition:
      spell_check == FALSE, # If: spell check says FALSE
      word_suggestions,     # Then: change the value to word suggestion
      word                  # Else: keep the word as it is
    ))

  ocr_summary <- ocr_summary |>
    group_by(word) |>
    summarise(counts = sum(counts),
              counts_normalised = counts / length(rotations),
              mean_confidence = mean(mean_confidence),
              mean_order = mean(mean_order)) |>
    arrange(desc(counts), mean_order)

  final_ocr <- ocr_summary |>
    filter(counts > 2) |>
    arrange(mean_order) |>
    pull(word) |>
    paste(collapse = " ")

  return(final_ocr)

}

better_ocr(maxres_images[2], brightness = 120, sharpen = 0)
