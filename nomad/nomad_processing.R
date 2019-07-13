library(tidyverse)
library(rvest)
library(lubridate)
library(readODS)

name_list <- 
  read_ods("Documents/sfpa_ratings/nomad/nomad_names.ods") %>% 
  #filter(is.na(need_to_check)) %>% 
  select(nickname, name)

url_to_game_results_df <- function(url) {
  # Takes in a URL, creates data frame with date, players, result
  url %>% 
    read_html %>% 
    html_nodes("td") %>% 
    html_text() %>%
    enframe(name = NULL) %>% 
    mutate(
      value = str_trim(str_remove_all(value, "\t|\n"))
    ) %>% 
    mutate(
      row_type = rep(
        c("view", "game_type", "date_short", "blank", 
          "home", "result", "away", "date"),
        nrow(.) / 8
      ),
      game_num = floor(nrow(.) / 8 - row_number() / 8 + 1)
    ) %>% 
    filter(!(row_type %in% c("view", "blank"))) %>% 
    spread(row_type, value) %>% 
    select(-game_num) %>% 
    mutate(
      date = str_remove_all(date, "@|th| PT(.)+"),
      date = if_else(
        str_detect(date, "\\'18"),
        str_c(date, " 2018"),
        if_else(
          str_detect(date, "\\'17"),
          str_c(date, " 2017"),
          str_c(date, " 2019")
        )
      ),
      date = str_remove_all(date, " \\'17| \\'18"),
      date = parse_date_time(date, orders = "%a %b %d %I:%M%p%Y", tz = "America/Los_Angeles")
    )
}

process_game_results_df <- function(game_results_df) {
  # Processes game results DF with results from multiple URLs
  df <- tibble()
  
  if (any(game_results_df$game_type == "#8ball (s)")) {
    set.seed(1)
    scotch_games <- 
      game_results_df %>% 
      filter(game_type == "#8ball (s)") %>% 
      separate(away, into = c("away1", "away2"), sep = "&") %>% 
      mutate_at(vars(contains("away")), str_trim) %>% 
      gather(key = type, value = away, away1:away2) %>% 
      select(-type) %>% 
      separate(home, into = c("home1", "home2"), sep = "&") %>% 
      mutate_at(vars(contains("home")), str_trim) %>% 
      gather(key = type, value = home, home1:home2) %>% 
      arrange(date) %>% 
      select(-type) %>% 
      mutate(
        game_winner = sample(c("home", "away"), nrow(.), replace = TRUE),
        game_type = "scotch"
      )
    
    df <- bind_rows(df, scotch_games)
  }
  
  if (any(game_results_df$game_type == "#8ball")) {
    set.seed(1)
    regular_games <- 
      game_results_df %>% 
      filter(game_type == "#8ball") %>% 
      mutate(
        game_winner = sample(c("home", "away"), nrow(.), replace = TRUE),
        game_type = NA_character_
      )
    
    df <- 
      bind_rows(df, regular_games)
  }
  
  df %>% 
    mutate(
      home2 = if_else(game_winner == "home", home, away),
      away2 = if_else(game_winner == "away", home, away),
    ) %>% 
    transmute(
      date, game_num, home = home2, away = away2, game_winner, game_type
    ) %>% 
    left_join(name_list %>% transmute(home = nickname, home_name = name), by = "home") %>% 
    left_join(name_list %>% transmute(away = nickname, away_name = name), by = "away") %>% 
    transmute(
      date, game_num, home = home_name, away = away_name, game_winner, game_type
    ) %>% 
    arrange(date, game_num)
}

url_list_to_final_df <- function(url_list) {
  map_dfr(url_list, url_to_game_results_df) %>% 
    group_by(date_short) %>% 
    arrange(date) %>% 
    mutate(game_num = row_number()) %>% 
    ungroup() %>% 
    process_game_results_df
}

# Happy
url_list <- 
  c(
    str_c("https://nomadpool.com/games?page=", 7:2, "&status=Final&venue_id=2"),
    "https://nomadpool.com/games?status=Final&venue_id=2"
  )

df <- 
  url_list %>% 
  url_list_to_final_df

# Slate
url_list <- 
  c(
    str_c("https://nomadpool.com/games?page=", 7:2, "&status=Final&venue_id=54"),
    "https://nomadpool.com/games?status=Final&venue_id=54"
  )

df <- 
  url_list %>% 
  url_list_to_final_df


