library(tidyverse)
library(rvest)
library(lubridate)
library(readxl)

name_list <- 
  read_ods("Documents/sfpa_ratings/nomad/nomad_names.ods") %>% 
  select(nickname, name)

process_slate_nomad_page <- function(url) {
  web_page <- 
    read_html(url)
  
  web_page_df <-
    web_page %>% 
    html_nodes("td") %>% 
    html_text() %>%
    enframe(name = NULL) %>% 
    mutate(
      value = str_trim(str_remove_all(value, "\t|\n"))
    )
  
  set.seed(1)
  df <- 
    web_page_df %>% 
    filter(
      value == "Defeated" | lag(value == "Defeated") | 
        lead(value == "Defeated") | lag(lag(value == "Defeated"))
    ) %>% 
    mutate(
      competitor = rep(c("home", "result", "away", "date"), nrow(.) / 4),
      game_num = floor(nrow(.) / 4 - row_number() / 4 + 1)
    ) %>% 
    filter(competitor != "result") %>% 
    spread(competitor, value) %>% 
    mutate(game_winner = sample(c("home", "away"), nrow(.), replace = TRUE)) %>% 
    mutate(
      home2 = if_else(game_winner == "home", home, away),
      away2 = if_else(game_winner == "away", home, away),
    ) %>% 
    transmute(
      date = str_remove_all(date, "@|th| PT(.)+"),
      home = home2, away = away2, game_winner,
      date = if_else(
        str_detect(date, "\\'18"),
        str_c(date, " 2018"),
        if_else(
          str_detect(date, "\\'17"),
          str_c(date, " 2017"),
          str_c(date, " 2019")
        )
      )
    ) %>% 
    mutate(
      date = str_remove_all(date, " \\'17"),
      date = str_remove_all(date, " \\'18"),
      date = parse_date_time(date, orders = "%a %b %d %I:%M%p%Y", tz = "America/Los_Angeles")
    ) %>% 
    left_join(name_list %>% transmute(home = nickname, home_name = name), by = "home") %>% 
    left_join(name_list %>% transmute(away = nickname, away_name = name), by = "away")
  
  df
}

#process_slate_nomad_page("https://nomadpool.com/games?page=4&status=Final&venue_id=54")

url_list <- c(
  "https://nomadpool.com/games?page=7&status=Final&venue_id=54",
  "https://nomadpool.com/games?page=6&status=Final&venue_id=54",
  "https://nomadpool.com/games?page=5&status=Final&venue_id=54",
  "https://nomadpool.com/games?page=4&status=Final&venue_id=54",
  "https://nomadpool.com/games?page=3&status=Final&venue_id=54",
  "https://nomadpool.com/games?page=2&status=Final&venue_id=54",
  "https://nomadpool.com/games?status=Final&venue_id=54"
)

df <- map_dfr(url_list, process_slate_nomad_page)

df %>% 
  filter(date > "2018-01-01")




process_happy_nomad_page <- function(url) {
  url <- "https://nomadpool.com/games?page=6&status=Final&venue_id=2"
  
  web_page <- 
    read_html(url)
  
  web_page_df <-
    web_page %>% 
    html_nodes("td") %>% 
    html_text() %>%
    enframe(name = NULL) %>% 
    mutate(
      value = str_trim(str_remove_all(value, "\t|\n"))
    )
  
  set.seed(1)
  df <-
    web_page_df %>% 
    filter(
      value == "Defeated" | lag(value == "Defeated") | 
        lead(value == "Defeated") | lag(lag(value == "Defeated"))
    ) %>% 
    mutate(
      competitor = rep(c("home", "result", "away", "date"), nrow(.) / 4),
      game_num = floor(nrow(.) / 4 - row_number() / 4 + 1)
    ) %>% 
    filter(competitor != "result") %>% 
    spread(competitor, value) %>% 
    separate(away, into = c("away1", "away2"), sep = "&") %>% 
    mutate_at(vars(contains("away")), str_trim) %>% 
    gather(key = type, value = away, away1:away2) %>% 
    select(-type) %>% 
    separate(home, into = c("home1", "home2"), sep = "&") %>% 
    mutate_at(vars(contains("home")), str_trim) %>% 
    gather(key = type, value = home, home1:home2) %>% 
    arrange(game_num) %>% 
    select(-type) %>% 
    mutate(game_winner = sample(c("home", "away"), nrow(.), replace = TRUE)) %>% 
    mutate(
      home2 = if_else(game_winner == "home", home, away),
      away2 = if_else(game_winner == "away", home, away),
    ) %>% 
    transmute(
      date = str_remove_all(date, "@|th| PT(.)+"),
      home = home2, away = away2, game_winner,
      date = if_else(
        str_detect(date, "\\'18"),
        str_c(date, " 2018"),
        if_else(
          str_detect(date, "\\'17"),
          str_c(date, " 2017"),
          str_c(date, " 2019")
        )
      )
    ) %>% 
    mutate(
      date = str_remove_all(date, " \\'17"),
      date = str_remove_all(date, " \\'18"),
      date = parse_date_time(date, orders = "%a %b %d %I:%M%p%Y", tz = "America/Los_Angeles")
    ) %>% 
    filter(!(date > "2019-05-25" & date < "2019-05-28")) %>% 
    inner_join(name_list %>% transmute(home = nickname, home_name = name), by = "home") %>% 
    inner_join(name_list %>% transmute(away = nickname, away_name = name), by = "away")
  
  df
  #filter(!is.na(home_name), !is.na(away_name))
}
## OLD
#url <- "https://nomadpool.com/games?page=7&status=Final&venue_id=2"
#game_results_df <- url_to_game_results_df(url)