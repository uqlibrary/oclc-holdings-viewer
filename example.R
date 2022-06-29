# load the custom functions and necessary packages
source("OCLC_libs/helpers.R")

#### Broken down process ####

# a. Use get_libs() to grab data related to a book's OCLC code
results <- get_libs(1061148251)
results <- get_libs(1061148251, verbose = TRUE)

# "results" is the raw API response

# b. Use flatten_libs() to extract a table of libraries from the response
df <- flatten_libs(results)

# The API limits to 50 results in one query.
# What if we try to grab more than 50?

# if there are more than 50 results, we can query again, starting at 51:
df2 <- get_libs(1061148251, start_lib = 51) %>%
  flatten_libs()

# But it is more convenient to use a wrapper function to get all the holdings!

#### Get all the holdings in one go ####

# For example, book with OCLC ID 1091066011 shows 517 libraries on website (at the time of testing)
# Use the "get_all()" function get all the data:
everything <- get_all(1091066011)
# (number of rows might be slightly different to website)

# The get_all() function can take the "limit" argument, which is set to 2000 by default.

#### Data processing examples ####

# how many unique institution names?
length(unique(everything$institutionName))

# what are the top countries?
everything %>%
  count(country, sort = TRUE)

#### Export ####

# To write the table to a CSV file:
write.csv(everything,
          file = "Bruce_Western_Homeward.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8") # to make sure it keeps special characters
