library(tidyverse)

# Challenge 1 -------------------------------------------------------------

# This table represents our understanding of who moves should be decoded. 
rps <- tibble::tribble(
  ~opponent, ~player,      ~move,
        "A",     "X",     "Rock",
        "B",     "Y",    "Paper",
        "C",     "Z", "Scissors"
)

# This table summaries the results of the games and the scores associated with
# each result.
score <- tibble::tribble(
     ~move_p,    ~move_o, ~result, ~shape, ~win, ~total,
      "Rock",     "Rock",   "tie",     1L,   3L,     4L,
     "Paper",     "Rock",   "win",     2L,   6L,     8L,
  "Scissors",     "Rock",  "lose",     3L,   0L,     3L,
      "Rock",    "Paper",  "lose",     1L,   0L,     1L,
     "Paper",    "Paper",   "tie",     2L,   3L,     5L,
  "Scissors",    "Paper",   "win",     3L,   6L,     9L,
      "Rock", "Scissors",   "win",     1L,   6L,     7L,
     "Paper", "Scissors",  "lose",     2L,   0L,     2L,
  "Scissors", "Scissors",   "tie",     3L,   3L,     6L
  )

# Import the strategy guide
strategy <- read.table("~/AOC/AOC_2022/Solutions/Day 2/input.txt", col.names = c("opponent", 'player'))
 
strategy %>% 
  # Decode opponents moves
  left_join(select(rps, opponent, move_o= move)) %>% 
  # Decode player moves
  left_join(select(rps, player, move_p= move)) %>% 
  # based on the decoded moves, find the score for each game
  left_join(score) %>% 
  # Summaries to find the total
  summarise(total = sum(total, na.rm = TRUE))
#> Joining, by = "opponent"
#> Joining, by = "player"
#> Joining, by = c("move_o", "move_p")
#>   total
#> 1 15691


# Challenge 2 -------------------------------------------------------------

# Our understanding of the strategy was wrong. Instead of decoding the players
# moves the same ways as the opponents moves, we should instead decode them like
# this.
results <- tibble::tribble(
  ~result_xyz, ~result,
          "X",  "lose",
          "Y",   "tie",
          "Z",   "win"
  )

strategy %>% 
  rename(result_xyz = player) %>% 
  # Decode the opponent's moves
  left_join(select(rps, opponent, move_o= move)) %>% 
  # Decode the result we want
  left_join(results) %>% 
  # based on the decoded move and result, calculate the score
  left_join(score) %>%  
  # summaries the result
  summarise(total = sum(total, na.rm = TRUE))
#> Joining, by = "opponent"
#> Joining, by = "result_xyz"
#> Joining, by = c("move_o", "result")
#>   total
#> 1 12989
