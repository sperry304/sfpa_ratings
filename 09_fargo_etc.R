library(tidyverse)
#library(lubridate)

setwd("~/Documents/sfpa_ratings")

all_fargo_ratings <- 
  "fargo_ratings/all_fargo_ratings.Rdata" %>% 
  read_rds()

latest_fargo_ratings <-
  all_fargo_ratings %>% 
  filter(date == max(date))

fargo_df <- 
  latest_fargo_ratings

latest_results_date <- 
  list.files("match_data", pattern = "results_no_forfeits") %>% 
  str_extract("\\d+-\\d+-\\d+") %>% 
  max()

results_no_forfeits <- 
  str_c("match_data/results_no_forfeits_", latest_results_date, ".Rdata") %>% 
  read_rds()

#player_list <- 
#  bind_rows(
#    results_no_forfeits %>% select(player = home),
#    results_no_forfeits %>% select(player = away)
#  ) %>% 
#  distinct() %>% 
#  arrange(player)

player_results_history <- function(player_of_interest) {
  results_no_forfeits %>% 
    filter(home == player_of_interest | away == player_of_interest) %>% 
    transmute(
      season,
      match_date,
      player = player_of_interest,
      opponent = if_else(home == player_of_interest, away, home),
      game_result = case_when(
        home == player_of_interest & game_winner == "home" ~ "W",
        away == player_of_interest & game_winner == "away" ~ "W",
        TRUE ~ "L"
      )
    ) %>% 
    left_join(fargo_df %>% transmute(player, rating), by = c("player" = "player")) %>% 
    left_join(fargo_df %>% transmute(player, opp_rating = rating), by = c("opponent" = "player")) %>% 
    mutate(win_prob = 1 / (1 + exp((opp_rating - rating) / 144))) %>% 
    arrange(match_date)
    #arrange(desc(win_prob))
}

player_performance <- function(player_of_interest) {
  player_results_history(player_of_interest) %>% 
    mutate(
      win_prob_round = round(win_prob, digits = 1),
      win = if_else(game_result == "W", 1, 0)
    ) %>% 
    group_by(win_prob_round) %>% 
    summarize(
      games = n(), 
      win_pct = mean(win)
    )
} 

player_results_history("Benito Taylor") %>% 
  knitr::kable()

player_of_interest <- "Benito Taylor"
results_no_forfeits %>% 
  filter(home == player_of_interest | away == player_of_interest)

player_performance("Chris Peterson")
player_performance("Skip Perry")
player_performance("Patty West")
player_performance("Mike Maxwell")
player_performance("Bobby Yulo")
player_performance("Thayer McDougle")
player_performance("Jesse La Fear")
player_performance("Mathieu Guglielmi")
player_performance("Hector Ortega")

all_fargo_ratings %>% 
  filter(player == "Gabriela Benuto") %>% 
  knitr::kable()




all_fargo_ratings <- 
  "fargo_ratings/all_fargo_ratings.Rdata" %>% 
  read_rds()

most_volatile <- 
  all_fargo_ratings %>% 
  group_by(player) %>% 
  summarize(sdev = sd(rating)) %>% 
  arrange(desc(sdev)) %>% 
  mutate(top20 = if_else(row_number() < 11, TRUE, FALSE)) 

player_list <- c(
  "Patty West", "Skip Perry", "Mike Maxwell", "Hector Ortega", "Nick Lansdown", 
  "Michael Gonzales", "Ryan Piaget", "Bob Simon"
)

all_fargo_ratings %>% 
  #left_join(most_volatile, by = "player") %>% 
  ggplot(aes(x = date, y = rating, group = player)) + 
  geom_hline(yintercept = 500, color = "white", size = 2) +
  #geom_line(alpha = 0.1) +
  geom_line(data = . %>% filter(player %in% player_list), aes(color = player))
