library(tidyverse)
library(rvest)
library(lubridate)

setwd("~/Documents/sfpa_ratings")

spring18 <- readRDS("match_data/all_matches_2018spring.Rdata")
spring18playoffs <- readRDS("match_data/all_matches_2018springplayoffs.Rdata")
fall18 <- readRDS("match_data/all_matches_2018fall.Rdata")
fall18playoffs <- readRDS("match_data/all_matches_2018fallplayoffs.Rdata")
spring19 <- readRDS("match_data/all_matches_2019spring.Rdata")

# With this, need functions from 01_data_scrape.Rmd
#spring19 <- 
#  spring19 %>% 
#  bind_rows(week_urls_to_all_matches("https://www.sfpapool.org/stats/week/63/"))

# Get combined file
results <-
  bind_rows(
    spring18,
    spring18playoffs,
    fall18,
    fall18playoffs,
    spring19
  )

results_17_18 <-
  bind_rows(
    spring18,
    spring18playoffs,
    fall18,
    fall18playoffs
  )

results_19 <-
  spring19

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
      p1_start_rating = NA_real_, 
      p1_end_rating = NA_real_,
      p2_start_rating = NA_real_, 
      p2_end_rating = NA_real_,
      t1_start_rating = NA_real_, 
      t1_end_rating = NA_real_,
      t2_start_rating = NA_real_, 
      t2_end_rating = NA_real_
    )
}

results_17_18_no_forfeits <- remove_forfeits(results_17_18)
results_19_no_forfeits <- remove_forfeits(results_19)
results_no_forfeits <- remove_forfeits(results)
train <- results_17_18_no_forfeits
test <- results_19_no_forfeits

# Get optimal home advantage parameter
# Remove week 1 since teams in the same bar play the same table
actual_home_win_pct <- 
  results_no_forfeits %>% 
  filter(week_number > 1) %>% 
  group_by(game_winner) %>% 
  count() %>% 
  group_by() %>% 
  mutate(pct = n / sum(n)) %>% 
  filter(game_winner == "home") %>% 
  pull(pct)

home_advantage <- log((1 - actual_home_win_pct) / actual_home_win_pct) / log(10) * -400
home_advantage

results_no_forfeits %>% 
  filter(week_number > 1) %>% 
  group_by(season, game_winner) %>% 
  count() %>% 
  group_by(season) %>% 
  mutate(pct = round(n / sum(n), 2))

latest_match_date <- 
  results_no_forfeits %>% 
  pull(match_date) %>% 
  max()

rnf_save_path <- str_c("match_data/results_no_forfeits_", latest_match_date, ".Rdata")

saveRDS(results_no_forfeits, rnf_save_path)