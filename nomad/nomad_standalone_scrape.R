library(tidyverse)
library(rvest)
library(lubridate)
library(readODS)
library(magrittr)

setwd("~/Documents/sfpa_ratings")

# Set URL
url <- "https://nomadpool.com/tournaments/3186"

# Takes in a URL, creates data frame with date, players, result
url_to_game_results_df <- function(url) {
  df <- 
    url %>% 
    read_html() %>% 
    html_nodes("table") %>% 
    #### EDIT HERE
    #extract2(23) %>% 
    extract2(length(.)) %>% 
    #### EDIT HERE
    html_nodes("td") %>% 
    html_text() %>% 
    enframe(name = NULL) %>% 
    mutate(
      value = str_trim(str_remove_all(value, "\t|\n"))
    ) %>% 
    filter(!str_detect(value, "^@")) %>% ## this is because some start @rocksden
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
      date = str_remove_all(date, "on\\sT\\-1"),
      date = str_remove_all(date, "on\\sT\\-2"),
      date = str_remove_all(date, "\\son"),
      date = str_trim(date),
      date = str_remove_all(date, "\\s\\d+s$|\\d+s$"),
      date = str_remove_all(date, "\\s\\d+m$|\\d+m$"),
      date = str_remove_all(date, "\\d+h$"),
      date = if_else(
        str_detect(date, "\\'19"),
        str_c(date, " 2019"),
        if_else(
          str_detect(date, "\\'18"),
          str_c(date, " 2018"),
          if_else(
            str_detect(date, "\\'17"),
            str_c(date, " 2017"),
            if_else(
              str_detect(date, "\\'16"),
              str_c(date, " 2016"),
              if_else(
                str_detect(date, "\\'15"),
                str_c(date, " 2015"),
                if_else(
                  str_detect(date, "\\'14"),
                  str_c(date, " 2014"),
                  if_else(
                    str_detect(date, "\\'13"),
                    str_c(date, " 2013"),
                    if_else(
                      str_detect(date, "\\'12"),
                      str_c(date, " 2012"),
                      str_c(date, " 2020")
                    )
                  )
                )
              )
            )
          )
        )
      ),
      date = str_remove_all(date, " \\'12| \\'13| \\'14| \\'15| \\'16| \\'17| \\'18| \\'19"),
      date = parse_date_time(date, orders = "%a %b %d %I:%M%p%Y", tz = "America/Los_Angeles"),
      date_short = if_else(
        date_short == "", 
        str_c((wday(date, label = TRUE)), month(date, label = TRUE), day(date), sep = " "), 
        date_short
      )
    ) %>% 
    mutate(
      multi_game = str_extract(result, "\\d+-\\d+")
    ) %>% 
    separate(multi_game, into = c("multi_game_win", "multi_game_loss"), sep = "-")
  
  if (2 %in% df$multi_game_win) {
    df <- 
      bind_rows(
        df, 
        df %>% filter(multi_game_win == 2)
      )
  }
  
  if (1 %in% df$multi_game_loss) {
    df <- 
      bind_rows(
        df, 
        df %>% 
          filter(multi_game_loss == 1) %>% 
          distinct() %>% 
          mutate(
            home_temp = away,
            away = home,
            home = home_temp
          ) %>% 
          select(-home_temp)
      )
  }
  
  df %>% 
    arrange(date)
}

# Processes game results DF with results from multiple URLs
process_game_results_df <- function(game_results_df) {
  df <- tibble()
  
  game_types <- 
    game_results_df %>% 
    select(game_type) %>% 
    distinct() %>% 
    pull()
  
  if ("#8ball (s)" %in% game_types | "#mini8 (s)" %in% game_types) {
    set.seed(1)
    scotch_games <- 
      game_results_df %>% 
      filter(game_type %in% c("#8ball (s)", "#mini8 (s)")) %>% 
      separate(home, into = c("home", "home2"), sep = "&") %>% 
      separate(away, into = c("away", "away2"), sep = "&") %>% 
      mutate_at(vars(contains("home"), contains("away")), str_trim) %>% 
      transmute(
        date, 
        date_short = str_c(date_short, year(date), sep = " "), 
        home, home2, away, away2,
        game_winner = "home",
        game_type = case_when(
          game_type == "#8ball (s)" ~ "scotch_8ball",
          game_type == "#mini8 (s)" ~ "scotch_8ball_mini"
        )
      )
    
    df <- bind_rows(df, scotch_games)
  }
  
  if ("#8ball" %in% game_types | "#9ball" %in% game_types | "#10ball" %in% game_types | "#mini8" %in% game_types) {
    set.seed(1)
    regular_games <- 
      game_results_df %>% 
      filter(game_type %in% c("#8ball", "#9ball", "#10ball", "#mini8")) %>% 
      transmute(
        date, 
        date_short = str_c(date_short, year(date), sep = " "), 
        home, home2 = home, away, away2 = away,
        game_winner = "home",
        game_type = str_remove(game_type, "#")
      )
    
    df <- 
      bind_rows(df, regular_games)
  }
  
  df %>% 
    arrange(date) %>% 
    mutate(result = "Defeated") %>% 
    select(-contains("multi"))
}

url_list_to_nickname_df <- function(url_list) {
  map_dfr(url_list, url_to_game_results_df) %>% 
    process_game_results_df() %>% 
    group_by(date_short) %>% 
    arrange(date) %>% 
    mutate(
      game_num = row_number(),
      season = case_when(
        month(date) < 7 ~ str_c("Spring ", year(date)),
        TRUE ~ str_c("Fall ", year(date))
      )
    ) %>% 
    ungroup() %>% 
    filter(!(home %in% c("1", "2", "3", "4", "A", "B", "C", "D"))) %>% 
    filter(!(home2 %in% c("1", "2", "3", "4", "A", "B", "C", "D"))) %>% 
    filter(!(away %in% c("1", "2", "3", "4", "A", "B", "C", "D"))) %>% 
    filter(!(away2 %in% c("1", "2", "3", "4", "A", "B", "C", "D")))
}

new_standalone_games <- 
  url %>% 
  url_list_to_nickname_df()

old_standalone_games <- 
  read_rds("nomad/nomad_standalone_tourneys.Rdata")

old_standalone_games %>% 
  count()

new_standalone_games %>% 
  count()

updated_standalone_df <-
  old_standalone_games %>% 
  bind_rows(new_standalone_games)

updated_standalone_df %>% 
  count()

write_rds(updated_standalone_df, "nomad/nomad_standalone_tourneys.Rdata")
