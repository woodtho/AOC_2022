library(tidyverse)

calories <-
  read.table("Solutions/Day 1/input.txt",
             # We want to keep the blank lines so help use identify new elves
             blank.lines.skip = FALSE,
             col.names = "calories") %>%
  # Add a blank row at the start
  bind_rows(data.frame(calories = NA_integer_), .) %>%
  # Add a new variable to track where each elf begins. 
  mutate(Elf = if_else(is.na(calories),
                       row_number(),
                       NA_integer_)) %>%
  # Fill the elf ID down until it meets the next elf
  fill(Elf, .direction = "down") %>%
  # Group the data 
  group_by(Elf) %>%
  # And summarise
  summarise(calories = sum(calories, na.rm = TRUE)) %>%
  arrange(-calories)
## A tibble: 251 × 2
#       Elf calories
#      <int>    <int>
#   1  1164    71934
#   2   231    69849
#   3  1454    69664
#   4  1663    69563
#   5  1914    68142
#   6  1265    67988
#   7   751    67869
#   8  1428    67294
#   9   357    66954
#  10   681    66368
## … with 241 more rows

# The elf with the most calories
top_n(calories, 1, calories) %>% pull()
# [1] 71934

# total calories carried by the top 3 elves
top_n(calories, 3, calories) %>% pull() %>% sum()
# [1] 211447
