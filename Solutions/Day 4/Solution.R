library(tidyverse)

# https://stackoverflow.com/questions/18719934/check-if-vector-contains-another-vector
overlap <- function(x, y) {
  # If one is longer than the other, the longer one cannot be fully contained in
  # the other
  if (length(x) > length(y)) {
    return(FALSE)
  }
  # embed converts the y vector into a matrix where each column is the same
  # length as x, and each column has a sliding subset of y. We can then compare
  # those columns against x and see is any is the same
  any(apply(embed(y, length(y) - length(x) + 1), 2, identical, x))
}

data <- read.table("Solutions/Day 4/input.txt",
           sep = "-",
           col.names = c("elf_1_min", "V2", "elf_2_max")) %>%
  separate(col = 2,
           into = c('elf_1_max', 'elf_2_min'),
           sep = ",") 
# # A tibble: 1,000 x 4
#   elf_1_min elf_1_max elf_2_min elf_2_max
#       <int> <chr>     <chr>         <int>
# 1         8 18        10               19
# 2        12 69        8                15
# 3        62 77        36               50
# 4        26 27        26               91
# 5        16 23        24               63
# # ... with 995 more rows
  
data %>% 
  transmute(
    # create two list columns that have vectors for the ranges
    elf_1 = map2(elf_1_min, elf_1_max, ~ seq(from = .x, to = .y)),
    elf_2 = map2(elf_2_min, elf_2_max, ~ seq(from = .x, to = .y)),
    
    # Now we can check if the fist vector fully overlaps with the second, or if
    # the second vector fully overlaps the first. 
    one_contains_other =  map2_lgl(map2_lgl(elf_1, elf_2, overlap),
                                   map2_lgl(elf_2, elf_1, overlap),
                                   any)
  ) %>%
  count(one_contains_other)
# # A tibble: 2 x 2
#   one_contains_other     n
#   <lgl>              <int>
# 1 FALSE                552
# 2 TRUE                 448


data %>%
  tibble() %>%
  transmute(
    elf_1 = map2(elf_1_min, elf_1_max, ~ seq(from = .x, to = .y)),
    elf_2 = map2(elf_2_min, elf_2_max, ~ seq(from = .x, to = .y)),
    
    # Check for the intersection of the two vectors, and check if the length of
    # the intersection is not 0
    overlaps = map_dbl(map2(elf_1, elf_2, intersect), length) != 0
  ) %>%
  count(overlaps )
# # A tibble: 2 x 2
#   overlaps     n
#   <lgl>    <int>
# 1 FALSE      206
# 2 TRUE       794
  

