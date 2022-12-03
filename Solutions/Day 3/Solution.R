library(tidyverse)


bags <-
  read.table("Solutions/Day 3/input.txt", col.names = "elf") %>%
  mutate(
    # Find the size of each elf's bag
    n = nchar(elf),
    # Divide each bag in half and split up the string into a vector of the items
    compartment1 = str_sub(elf, end = n / 2) %>% str_split(pattern = ""),
    compartment2 = str_sub(elf, start = (n / 2) + 1) %>% str_split(pattern = "")
  ) %>%
  # Switch to row-wise processing
  rowwise() %>%
  mutate(
    # find the intersection of the two compartments to find what is shared
    # between each
    common = intersect(compartment1, compartment2),
    # Find the index in the table to get the value for each item
    value = match(common , table = c(letters, LETTERS))) %>%
  ungroup()


bags %>%
  # Total up the item values
  summarise(value  = sum(value))
# # A tibble: 1 x 1
#      b
#  <int>
# 1 8109



bags %>%
  mutate(
    # Create a group variable that puts each elf into groups of 3
    group = rep(1:(nrow(.) / 3), each = 3),
    # And a second variable for pivoting 
    n = rep(1:3, length.out = nrow(.))) %>%
  select(elf, group, n) %>%
  pivot_wider(
    # `n` is used to pivot each group into 3 columns
    names_from = n,
    # We want the contents of the elves' bags in each column
    values_from = elf,
    # Tidy up the names
    names_glue = "elf_{.name}") %>%
  mutate(
    # Convert each elf into a vector
    across(starts_with("elf"), str_split, pattern = "")) %>%
  rowwise() %>%
  mutate(
    # Intersect the 3 elves to find what is common between in each group
    common = reduce(list(elf_1, elf_2, elf_3), intersect),
    # and find the value
    value = match(common , table = c(letters, LETTERS))) %>%
  ungroup() %>%
  summarise(value  = sum(value))
# # A tibble: 1 x 1
#   value
#   <int>
# 1  2738
