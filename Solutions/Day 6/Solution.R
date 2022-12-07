library(tidyverse)

input <- readLines("Solutions/Day 6/input.txt") %>%
  # Seperate each letter into its own element of a vector
  str_split(pattern = "") %>%
  unlist()

communication_system <- function(x, n) {
  # innitialize some variable
  last <- character(0)
  res <- integer(0)
  # Go through each element of the input
  for (i in seq_len(length(x))) {
    # get the last 4 elements
    last <- c(x[i], last)[1:n]
    # Check if the 4 elements are unique and there is 4 of them
    if (identical(last, unique(last)) && i > (n - 1)) {
      # Store i as the result.
      res <- i
      # Break from the loop
      break
    }
  }
  res
}
# Look for the first 4 unique characters
communication_system(input, 4)
# [1] 1965

# And 14 unique characters
communication_system(input, 14)
# [1] 2773

