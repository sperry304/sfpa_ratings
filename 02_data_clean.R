library(tidyverse)
library(rvest)
library(lubridate)

setwd("~/Documents/sfpa_ratings")

spring18 <- read_rds("match_data/all_matches_2018spring.Rdata")
spring18playoffs <- read_rds("match_data/all_matches_2018springplayoffs.Rdata")
fall18 <- read_rds("match_data/all_matches_2018fall.Rdata")
fall18playoffs <- read_rds("match_data/all_matches_2018fallplayoffs.Rdata")
spring19 <- read_rds("match_data/all_matches_2019spring.Rdata")
spring19playoffs <- read_rds("match_data/all_matches_2019springplayoffs.Rdata")
spring19tournaments <- 
  read_csv("tournaments/spring2019_tournaments.csv") %>% 
  mutate(
    home_team = NA_character_, away_team = NA_character_, 
    forfeit = NA_character_, game_type = NA_character_
  )
fall19 <- read_rds("match_data/all_matches_2019fall.Rdata")

# Get combined file
results <-
  bind_rows(
    spring18 %>% 
      add_column(league = "SFPA", .before = "season") %>% 
      add_column(match_type = "regular", .before = "season") %>% 
      add_column(game_type = NA_character_),
    spring18playoffs %>% 
      add_column(league = "SFPA", .before = "season") %>% 
      add_column(match_type = "playoffs", .before = "season") %>% 
      add_column(game_type = NA_character_),
    fall18 %>% 
      add_column(league = "SFPA", .before = "season") %>% 
      add_column(match_type = "regular", .before = "season") %>% 
      add_column(game_type = NA_character_),
    fall18playoffs %>% 
      add_column(league = "SFPA", .before = "season") %>% 
      add_column(match_type = "playoffs", .before = "season") %>% 
      add_column(game_type = NA_character_),
    spring19 %>% 
      add_column(league = "SFPA", .before = "season") %>% 
      add_column(match_type = "regular", .before = "season") %>% 
      add_column(game_type = NA_character_),
    spring19playoffs %>% 
      add_column(league = "SFPA", .before = "season") %>% 
      add_column(match_type = "playoffs", .before = "season") %>% 
      add_column(game_type = NA_character_),
    spring19tournaments,
    fall19 %>% 
      add_column(league = "SFPA", .before = "season") %>% 
      add_column(match_type = "regular", .before = "season") %>% 
      add_column(game_type = NA_character_),
  )

# Pull out forfeits and omitted playoff games from data frames, clean names
remove_forfeits <- function(results_df) {
  results_df %>% 
    filter(is.na(forfeit)) %>% 
    filter(!is.na(game_winner)) %>% 
    filter(home != "Forfeited Game") %>% 
    filter(away != "Forfeited Game") %>% 
    mutate(
      home = str_replace(home, "\\(", ""),
      home = str_replace(home, "\\)", ""),
      away = str_replace(away, "\\(", ""),
      away = str_replace(away, "\\)", ""),
      home = str_replace_all(home, '\\"', ""),
      away = str_replace_all(away, '\\"', ""),
      home = str_replace(home, "\\'HJ\\'", "HJ"),
      away = str_replace(away, "\\'HJ\\'", "HJ"),
      home = str_replace(home, "\\'Stkyrice\\'\\s", ""),
      away = str_replace(away, "\\'Stkyrice\\'\\s", ""),
      home_team = str_replace(home_team, "é", "e"),
      away_team = str_replace(away_team, "é", "e"),
      home_team = str_replace(home_team, "!", ""),
      away_team = str_replace(away_team, "!", ""),
      home = if_else(home == "Chris L", "Chris Logan", home),
      away = if_else(away == "Chris L", "Chris Logan", away),
      home = if_else(home == "Jerz", "Jerz Zuluaga", home),
      away = if_else(away == "Jerz", "Jerz Zuluaga", away),
      home = if_else(home == "Mike Romano", "Michael Romano", home),
      away = if_else(away == "Mike Romano", "Michael Romano", away),
      home = if_else(home == "James Horsefall", "James Horsfall", home),
      away = if_else(away == "James Horsefall", "James Horsfall", away),
      home = if_else(home == "Ninad Desei", "Ninad Desai", home),
      away = if_else(away == "Ninad Desei", "Ninad Desai", away),
      home = if_else(home == "Jules Tanseco", "Jukers Tanseco", home),
      away = if_else(away == "Jules Tanseco", "Jukers Tanseco", away),
      home = str_trim(home),
      away = str_trim(away),
      home_team = if_else(home_team == "The Black Willows", "Black Willows", home_team),
      away_team = if_else(away_team == "The Black Willows", "Black Willows", away_team),
      home_team = if_else(home_team == "Lucky Horseshoe", "Lucky Horseshoe Unnamed", home_team),
      away_team = if_else(away_team == "Lucky Horseshoe", "Lucky Horseshoe Unnamed", away_team),
      home_team = if_else(home_team == "6 Holes of Napper Tandy", "Bare Naked 6 Holes", home_team),
      away_team = if_else(away_team == "6 Holes of Napper Tandy", "Bare Naked 6 Holes", away_team),
      home_team = if_else(home_team == "Smoke and Rumors", "Smoke & Rumors", home_team),
      away_team = if_else(away_team == "Smoke and Rumors", "Smoke & Rumors", away_team),
      home_team = if_else(home_team == "Lucky Break", "Lone Star Rebels", home_team),
      away_team = if_else(away_team == "Lucky Break", "Lone Star Rebels", away_team),
      home_team = if_else(home_team == "Mixing in Action", "Mix VANGIE", home_team),
      away_team = if_else(away_team == "Mixing in Action", "Mix VANGIE", away_team),
      home_team = if_else(home_team == "Harry Harringtons v 2.0", "Harry Harringtons", home_team),
      away_team = if_else(away_team == "Harry Harringtons v 2.0", "Harry Harringtons", away_team),
      t1_start_rating = NA_real_, 
      t1_end_rating = NA_real_,
      t2_start_rating = NA_real_, 
      t2_end_rating = NA_real_
    )
}

results_no_forfeits <- 
  results %>% 
  remove_forfeits()
  
latest_match_date <- 
  results_no_forfeits %>% 
  pull(match_date) %>% 
  max()

rnf_save_path <- str_c("match_data/results_no_forfeits_", latest_match_date, ".Rdata")

saveRDS(results_no_forfeits, rnf_save_path)
