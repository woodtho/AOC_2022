library(tidyverse)

calories <- read.table("Solutions/Day 1/input.txt", blank.lines.skip = FALSE, col.names = "calories") %>%
  bind_rows(data.frame(calories = NA_integer_), .) %>% 
  mutate(Elf = if_else(is.na(calories), row_number(), NA_integer_)) %>% 
  fill(Elf, .direction = "down") %>% 
  group_by(Elf) %>% 
  summarise(calories = sum(calories, na.rm = TRUE)) %>% 
  arrange(-calories)

# The elf with the most calories
top_n(calories, 1, calories) %>% pull()

# total calories carried by the top 3 elves
top_n(calories, 3, calories) %>% pull() %>% sum()
