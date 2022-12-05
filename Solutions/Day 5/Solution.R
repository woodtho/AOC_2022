library(tidyverse)

file <- readLines("Solutions/Day 5/input.txt")


boxes <-
  # Get everything before the line break
  file[1:match("", file) - 1] %>%
  # Fill in the empty spaces with placeholders (`[.]`)
  str_replace_all(., "    ", " [.]") %>%
  # remove the square brakets
  str_remove_all(., " |\\[|\\]") %>%
  # and split the strings into a matrix
  str_split(., pattern = "", simplify = TRUE) %>%
  as_tibble() %>%
  # Remove the rown with the numbers
  slice(-nrow(.)) %>%
  # and convert the tibble to a list
  as.list() %>%
  # Finally remove any place holders
  map(., ~ .[. != "."])


boxes
# List of 9
# $ V1: chr [1:5] "L" "C" "G" "M" ...
# $ V2: chr [1:8] "G" "H" "F" "T" ...
# $ V3: chr [1:8] "R" "W" "T" "M" ...
# $ V4: chr [1:6] "P" "Q" "V" "D" ...
# $ V5: chr [1:7] "T" "B" "L" "S" ...
# $ V6: chr [1:7] "P" "D" "C" "H" ...
# $ V7: chr [1:3] "T" "C" "H"
# $ V8: chr [1:8] "P" "H" "N" "Z" ...
# $ V9: chr [1:4] "G" "H" "F" "Z"


program <-
  # Take everything after the line break
  file[(match("", file) + 1):length(file)] %>%
  # Split it on the spaces
  str_split(., ' ', simplify = TRUE) %>%
  # And only keep the numbers
  .[, c(2, 4, 6)] %>%
  as_tibble() %>%
  set_names("n", "from", "to") %>%
  mutate(across(everything(), as.numeric))


# Puzzle 1 ----------------------------------------------------------------

CrateMover_9000 <- function(data, n, from, to) {
  # iterate through a sequence of length n
  for (j in seq_len(n)) {
    # if the stack has no boxes, do nothing
    if (length(data[[from]]) > 0) {
      # else get one box at a time and move it from the "from" stack to the
      # "to" stack
      data[[to]] <- c(data[[from]][1], data[[to]])
      # Then remove that box from the old stack
      data[[from]] <- data[[from]][-seq_len(1)]
    }
  }
  data
}

crates_1 <- boxes

for (i in seq_len(nrow(program))) {
  crates_1 <- CrateMover_9000(
    data = crates_1,
    n = program$n[i],
    from = program$from[i],
    to = program$to[i]
  )
}

map_chr(crates_1, first) %>% paste0(., collapse = "")
# [1] "VCTFTJQCG"





# Puzzle 2 ----------------------------------------------------------------


crates_2 <- boxes

CrateMover_9001 <- function(data, n, from, to) {
  # If there if boxes in the stack
  if (length(data[[from]]) > 0) {
    # Get all n boxes off the stack
    new <- c(data[[from]][seq_len(n)], data[[to]])
    # Remove any missing values from the new stack
    data[[to]] <- new[!is.na(new)]
    # Remove the boxes from the old stack
    data[[from]] <- data[[from]][-seq_len(n)]
  }
  data
}


for (i in seq_len(nrow(program))) {
  crates_2 <- CrateMover_9001(
    data = crates_2,
    n = program$n[i],
    from = program$from[i],
    to = program$to[i]
  )
  
}

map_chr(crates_2, first) %>% paste0(., collapse = "")
# [1] "GCFGLDNJZ"







