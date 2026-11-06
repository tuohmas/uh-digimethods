# PREPARATIONS ------------------------------------------------------------

install.packages("dplyr")
install.packages("lubridate")

# DATA STRUCTURES ---------------------------------------------------------

# 0-dimensional
value <- nchar("Hi")
# 1-dimensional
vector <- c(1L, 3L, 5L:10L)
# 2-dimensional
df <- data.frame(vector, value)

## 0-dimensional

# Run nchar and store its output as value
value <- nchar("Hi")

# Evaluate by print returns an integer
print(value) == 2
typeof(value) == "integer"
length(value) == 1

# Variables can be transformed (casted) to other data types
as.character(value) == "2"
typeof(as.character(value)) == "character"

# Now we can do some arithmetic and store each result into a new variable
value2 <- nchar("Hello")

sum_values <- value + value2
sum_values == 7

mean_values <- mean(c(value, value2))
mean_values == 3.5

# Missing values (NA) have length and type, NULLs don't
missing_value <- NA
length(missing_value) == 1

empty_value <- NULL
length(empty_value) == 0

# is.* function checks the type and outputs TRUE or FALSE
logical_value <- is.na(empty_value)
if(logical_value == TRUE) {
  print("This should print out") }

as.integer(logical_value) == 1
logical_value != length(empty_value)

# Date object out of characters
date_value <- as.Date("2026-01-01")
as.integer(date_value) == 20454

Sys.Date() > date_value # Today's date is more recent than date_value

## 1-dimensional -----------------------------------------------------------

# Combine some integers and integer ranges to store a vector
vector <- c(1L, 3L, 5L:10L)   # L distinguishes integers from floats

vector <-                     # Alternatively...
  seq(from = 5, to = 10) %>%  # ...first create a custom range
  append(1, 3) %>%            # ...then add 1 and 3 to the end
  sort()                      # ...lastly sort in ascending order

# Evaluate by cat returns all the elements
cat(vector) # 1 3 5 6 7 8 9 10
is.vector(vector) == TRUE
length(vector) == 8

# 2-dimensional -----------------------------------------------------------

# Create a dataframe from many ordered vectors
sequence <- 1:12

df <- data.frame(
  month_number = sequence,
  month_name = month.name[sequence],
  year = 2026
  )

dim(df) # sames as nrow(df) and ncol(df)
sapply(df, typeof)

library(dplyr)
dplyr::glimpse(df) # or the same with dplyr library

View(df)

# Evaluate rows with index number or a condition
df[2, ]
df[df$month_name == "February", ]
filter(df, month_name == "February")

# Evaluate or replace cell values with both
df[2, "month_name"]
df[2, "month_name"]

# Evaluate column with dollar sign, brackets, or functions
df$year
months[, "year"]
pull(df, year)

typeof(df$year)

# Change columns values in place
df$month_number
df$month_number <- sprintf("%02d", df$month_number)

# Add new column: first of month in format "year-month-days"
df$first_of_month <-
  # paste values together with a given separator
  paste(df$year, df$month_number, "01", sep = "-") %>%
  as.Date()

df$first_of_month <- seq.Date(
  from = as.Date("2026-01-01"),
  by = "month",
  lenght.out = 12 # or, for exmple nrow(df)
  )

class(df$first_of_month)

# Change months names to Finnish
finnish_month_names <- format(df$first_of_month, "%B")

df <- df %>% mutate(month_name = finnish_month_names)

# Assign new columns by using data from existing values
df <- df %>%
  mutate(days = lubridate::days_in_month(first_of_month))

# Average and total of days
mean(df$days)
sum(df$days)
