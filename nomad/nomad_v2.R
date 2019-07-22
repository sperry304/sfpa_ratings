library(tidyverse)
library(rvest)
library(lubridate)
library(readODS)

setwd("~/Documents/sfpa_ratings")

name_list <- 
  read_ods("nomad/nomad_names.ods") %>% 
  filter(is.na(need_to_check)) %>% 
  select(nickname, name)

url_to_game_results_df <- function(url) {
  # Takes in a URL, creates data frame with date, players, result
  df <-
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
          if_else(
            str_detect(date, "\\'16"),
            str_c(date, " 2016"),
            str_c(date, " 2019")
          )
        )
      ),
      date = str_remove_all(date, " \\'16| \\'17| \\'18"),
      date = parse_date_time(date, orders = "%a %b %d %I:%M%p%Y", tz = "America/Los_Angeles")
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

process_game_results_df <- function(game_results_df) {
  # Processes game results DF with results from multiple URLs
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
  
  if ("#8ball" %in% game_types | "#9ball" %in% game_types | "#10ball" %in% game_types) {
    set.seed(1)
    regular_games <- 
      game_results_df %>% 
      filter(game_type %in% c("#8ball", "#9ball", "#10ball")) %>% 
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

#game_results_df <- 
#  url_to_game_results_df("https://nomadpool.com/games?page=11&status=Final&venue_id=54")

#url <- "https://nomadpool.com/games?page=13&status=Final&venue_id=54"

#df_test <- process_game_results_df(game_results_df)

url_list_to_nickname_df <- function(url_list) {
  map_dfr(url_list, url_to_game_results_df) %>% 
    process_game_results_df() %>% 
    group_by(date_short) %>% 
    arrange(date) %>% 
    ungroup() %>% 
    mutate(
      game_num = row_number(),
      season = case_when(
        month(date) < 7 ~ str_c("Spring ", year(date)),
        TRUE ~ str_c("Fall ", year(date))
      )
    ) %>% 
    filter(!(home %in% c("1", "2", "3", "4", "A", "B", "C", "D"))) %>% 
    filter(!(away %in% c("1", "2", "3", "4", "A", "B", "C", "D")))
}

# Happy
happy_url_list <- 
  c(
    str_c("https://nomadpool.com/games?page=", 63:2, "&status=Final&venue_id=2"),
    "https://nomadpool.com/games?status=Final&venue_id=2"
  )

happy_df <- 
  happy_url_list %>% 
  url_list_to_nickname_df()

write_rds(happy_df, "nomad/happy_games_2018.Rdata")


# Slate
slate_url_list <- 
  c(
    str_c("https://nomadpool.com/games?page=", 16:2, "&status=Final&venue_id=54"),
    "https://nomadpool.com/games?status=Final&venue_id=54"
  )

slate_df <- 
  slate_url_list %>% 
  url_list_to_nickname_df()

write_rds(slate_df, "nomad/slate_games_2016.Rdata")


happy_df <-
  read_rds("nomad/happy_games_2018.Rdata")

slate_df <-
  read_rds("nomad/slate_games_2016.Rdata")

happy_players <- 
  bind_rows(
    happy_df %>% transmute(player = home),
    happy_df %>% transmute(player = home2),
    happy_df %>% transmute(player = away),
    happy_df %>% transmute(player = away2)
  ) %>% 
  group_by(player) %>% 
  count(name = "num_games") %>% 
  arrange(-num_games) %>% 
  ungroup() %>% 
  left_join(name_list, by = c("player" = "nickname"))

slate_players <- 
  bind_rows(
    slate_df %>% transmute(player = home),
    slate_df %>% transmute(player = home2),
    slate_df %>% transmute(player = away),
    slate_df %>% transmute(player = away2)
  ) %>% 
  group_by(player) %>% 
  count(name = "num_games") %>% 
  arrange(-num_games) %>% 
  ungroup() %>% 
  left_join(name_list, by = c("player" = "nickname"))

all_players <- 
  slate_players %>% 
  transmute(player, name, slate_games = num_games) %>% 
  full_join(happy_players %>% transmute(player, name, happy_games = num_games), by = c("player", "name")) %>% 
  mutate_if(is.numeric, ~ replace_na(., 0)) %>% 
  mutate(
    total_games = happy_games + slate_games
  ) %>% 
  arrange(desc(total_games))



happy_df %>% 
  filter(home == "acracchiolo0921" | home2 == "acracchiolo0921" | 
           away == "acracchiolo0921" | away2 == "acracchiolo0921") %>% 
  knitr::kable()


happy_df_named <- 
  happy_df %>% 
  left_join(name_list %>% transmute(home = nickname, name)) %>% 
  #mutate(home = name) %>% 
  #select(-name) %>% 
  left_join(name_list %>% transmute(home2 = nickname, name2 = name))
  #mutate(home2 = name) %>% 
  #select(-name)

# Slate
#slate_url_list <- 
#  c(
#    str_c("https://nomadpool.com/games?page=", 8:2, "&status=Final&venue_id=54"),
#    "https://nomadpool.com/games?status=Final&venue_id=54"
#  )

#slate_df <- 
#  slate_url_list %>% 
#  url_list_to_nickname_df()

bind_rows(
  happy_df %>% transmute(player = home),
  happy_df %>% transmute(player = home2),
  happy_df %>% transmute(player = away),
  happy_df %>% transmute(player = away2)
) %>% 
  distinct() %>% 
  arrange(player) %>% 
  knitr::kable()





happy_ratings <- 
  happy_players %>% 
  mutate(rating = 500)

results_df <- happy_df
ratings_df <- happy_ratings
player_of_interest <- "robert"

games_involving_player <- function(player_of_interest, results_df) {
  results_df %>% 
    filter(
      home == player_of_interest | home2 == player_of_interest |
        away == player_of_interest | away2 == player_of_interest
    ) %>% 
    transmute(
      date,
      season,
      game_type,
      player = player_of_interest,
      teammate = case_when(
        home == player_of_interest ~ home2,
        home2 == player_of_interest ~ home,
        away == player_of_interest ~ away2,
        away2 == player_of_interest ~ away
      ),
      opponent1 = case_when(
        home == player_of_interest ~ away2,
        home2 == player_of_interest ~ away,
        away == player_of_interest ~ home2,
        away2 == player_of_interest ~ home
      ),
      opponent2 = case_when(
        home == player_of_interest ~ away,
        home2 == player_of_interest ~ away2,
        away == player_of_interest ~ home,
        away2 == player_of_interest ~ home2
      ),
      game_result = case_when(
        home == player_of_interest & game_winner == "home" ~ "W",
        home2 == player_of_interest & game_winner == "home" ~ "W",
        away == player_of_interest & game_winner == "away" ~ "W",
        away2 == player_of_interest & game_winner == "away" ~ "W",
        TRUE ~ "L"
      )
    )
}

append_ratings <- function(games_involving_player_df, ratings_df) {
  games_involving_player_df %>% 
    inner_join(ratings_df %>% transmute(player, player_rating = rating), by = "player") %>% 
    inner_join(ratings_df %>% transmute(teammate = player, teammate_rating = rating), by = "teammate") %>% 
    inner_join(ratings_df %>% transmute(opponent1 = player, opponent1_rating = rating), by = "opponent1") %>% 
    inner_join(ratings_df %>% transmute(opponent2 = player, opponent2_rating = rating), by = "opponent2") %>% 
    mutate(
      max_team_rating = pmax(player_rating, teammate_rating),
      min_team_rating = pmin(player_rating, teammate_rating),
      max_opponent_rating = pmax(opponent1_rating, opponent2_rating),
      min_opponent_rating = pmin(opponent1_rating, opponent2_rating)
    )
}
  
get_updated_rating <- function(player_of_interest, results_df, ratings_df, a) {
  game_count <- 
    ratings_df %>% 
    filter(player == player_of_interest) %>% 
    select(num_games) %>% 
    pull()
  
  if (game_count == 0) {
    return(
      ratings_df %>% 
        filter(player == player_of_interest) %>% 
        select(rating) %>% 
        pull()
    )
  }
  
  b <- (a - 1) / 500
  
  games_involving_player(player_of_interest, happy_df) %>% 
    append_ratings(ratings_df) %>% 
    mutate(
      A = 1 / ((2 * max_team_rating + min_team_rating) / 3 + (2 * max_opponent_rating + min_opponent_rating) / 3),
      W = if_else(game_result == "W", 1, 0),
      decay = case_when(
        season == "Spring 2018" ~ 0.6,
        season == "Fall 2018" ~ 0.8,
        season == "Spring 2019" ~ 1.0,
        season == "Fall 2019" ~ 1.0
      )
    ) %>% 
    summarize(rating = ((a - 1) + sum(W * decay)) / (b + sum(A * decay))) %>% 
    pull(rating)
}

results_to_ratings <- function(results_df, a, mae_stop = 100) {
  fargo_ratings <- 
    bind_rows(
      results_df %>% transmute(player = home),
      results_df %>% transmute(player = home2),
      results_df %>% transmute(player = away),
      results_df %>% transmute(player = away2)
    ) %>% 
    group_by(player) %>% 
    count(name = "num_games") %>% 
    arrange(-num_games) %>% 
    ungroup() %>% 
    mutate(rating = 500)
  
  abs_diff <- 100000
  while (abs_diff > mae_stop) {
    old_ratings <- fargo_ratings %>% pull(rating)
    for (i in 1:nrow(fargo_ratings)) {
      player_name <- fargo_ratings$player[i]
      fargo_ratings$rating[i] <- 
        get_updated_rating(player_name, results_df = results_df, ratings_df = fargo_ratings, a = a)
    }
    new_ratings <- fargo_ratings %>% pull(rating)
    print(str_c("Mean absolute difference: ", sum(abs(old_ratings - new_ratings))))
    abs_diff <- sum(abs(old_ratings - new_ratings))
  }
  
  fargo_ratings
}

test_ratings <- 
  results_to_ratings(happy_df, a = 3, mae_stop = 100) %>% 
  mutate(
    raw_rating = rating,
    rating = log(rating) * 144,
    rating = rating - mean(rating) + 500
  )

test_ratings %>% 
  arrange(desc(rating)) %>% 
  knitr::kable()

test_ratings %>% 
  filter(player == "Belle")


games_involving_player("2", happy_df)
