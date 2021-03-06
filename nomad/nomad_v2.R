library(tidyverse)
library(rvest)
library(lubridate)
library(readODS)

setwd("~/Documents/sfpa_ratings")

name_list <- 
  read_ods("nomad/nomad_names.ods") %>% 
  filter(is.na(need_to_check)) %>% 
  select(nickname, name)

url <- "https://nomadpool.com/games?page=72&status=Final&venue_id=4"

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
      date = str_remove_all(date, "\\s\\d+s$|\\d+s$"),
      date = str_remove_all(date, "\\d+m$"),
      date = if_else(
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
                    str_c(date, " 2019")
                  )
                )
              )
            )
          )
        )
      ),
      date = str_remove_all(date, " \\'12| \\'13| \\'14| \\'15| \\'16| \\'17| \\'18"),
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

# Happy
happy_url_list <- 
  c(
    str_c("https://nomadpool.com/games?page=", 2, "&venue_id=2"),
    "https://nomadpool.com/games?venue_id=2"
  )

happy_df <- 
  happy_url_list %>% 
  url_list_to_nickname_df()

new_happy_games <- 
  happy_df %>% 
  filter(home == home2)

old_happy_games <- 
  read_rds("nomad/happy_games_2016_2018_v1.Rdata")

old_happy_games %>% 
  count()

new_happy_games %>% 
  count()

updated_happy_df <-
  old_happy_games %>% 
  bind_rows(new_happy_games)

updated_happy_df %>% 
  count()

write_rds(updated_happy_df, "nomad/happy_games_2016_2018_v1.Rdata")


# Slate
slate_url_list <- 
  c(
    str_c("https://nomadpool.com/games?page=", 2, "&venue_id=54"),
    "https://nomadpool.com/games?venue_id=54"
  )

slate_df <- 
  slate_url_list %>% 
  url_list_to_nickname_df()

# Choose only new tournament here
new_slate_games <- 
  slate_df %>% 
  filter(month(date) > 8)

old_slate_games <- 
  read_rds("nomad/slate_games_2016.Rdata")

old_slate_games %>% 
  count()

new_slate_games %>% 
  count()

updated_slate_df <-
  old_slate_games %>% 
  bind_rows(new_slate_games)

updated_slate_df %>% 
  count()

write_rds(updated_slate_df, "nomad/slate_games_2016.Rdata")

# Blackthorn
thorn_url_list <- 
  c(
    str_c("https://nomadpool.com/games?page=", 74:2, "&status=Final&venue_id=4"),
    "https://nomadpool.com/games?status=Final&venue_id=4"
  )

thorn_df <- 
  thorn_url_list %>% 
  url_list_to_nickname_df()

write_rds(thorn_df, "nomad/thorn_games_2012.Rdata")

# 540
fiveforty_url_list <- 
  "https://nomadpool.com/games?status=Final&venue_id=17"

fiveforty_df <- 
  fiveforty_url_list %>% 
  url_list_to_nickname_df()

fiveforty_df <-
  fiveforty_df %>% 
  filter(year(date) == 2017)

write_rds(fiveforty_df, "nomad/fiveforty_games_2017.Rdata")

# Iron Throne
ironthrone_url_list <- 
  "https://nomadpool.com/tournaments/2654"

url <- "https://nomadpool.com/tournaments/2654"

ironthrone_df <- 
  ironthrone_url_list %>% 
  url_list_to_nickname_df()

fiveforty_df <-
  fiveforty_df %>% 
  filter(year(date) == 2017)

write_rds(fiveforty_df, "nomad/fiveforty_games_2017.Rdata")





slate_df <-
  read_rds("nomad/slate_games_2016.Rdata")

happy_df <-
  read_rds("nomad/happy_games_2016_2018_v1.Rdata")

all_singles <- 
  slate_df %>% 
  filter(home == home2) %>% 
  bind_rows(
    happy_df %>% filter(home == home2)
  ) %>% 
  select(-c(home2, away2, result))
  
all_singles_num_matches_by_player <- 
  all_singles %>% 
  transmute(player = home) %>% 
  bind_rows(
    all_singles %>% transmute(player = away)
  ) %>% 
  group_by(player) %>% 
  count(sort = TRUE) %>% 
  left_join(name_list %>% transmute(player = nickname, name), by = "player")

all_doubles <- 
  slate_df %>% 
  filter(home != home2) %>% 
  bind_rows(
    happy_df %>% filter(home != home2)
  )

all_doubles_num_matches_by_player <-
  all_doubles %>% 
  transmute(player = home) %>% 
  bind_rows(all_doubles %>% transmute(player = home2)) %>% 
  bind_rows(all_doubles %>% transmute(player = away)) %>% 
  bind_rows(all_doubles %>% transmute(player = away2)) %>% 
  group_by(player) %>% 
  count(sort = TRUE) %>% 
  left_join(name_list %>% transmute(player = nickname, name), by = "player")


happy_df %>% 
  filter(away == "Benito T")

happy_singles <-
  happy_df %>% 
  filter(home == home2)

happy_singles_named <- 
  happy_singles %>% 
  select(-c(home2, away2, result)) %>% 
  left_join(name_list %>% transmute(home = nickname, home_actual = name), by = "home") %>% 
  left_join(name_list %>% transmute(away = nickname, away_actual = name), by = "away") 

happy_singles_named_filtered <- 
  happy_singles_named %>% 
  filter(!is.na(home_actual), !is.na(away_actual), !is.na(season))


thorn_singles_named <- 
  thorn_df %>% 
  filter(home == home2) %>% 
  select(-c(home2, away2, result)) %>% 
  left_join(name_list %>% transmute(home = nickname, home_actual = name), by = "home") %>% 
  left_join(name_list %>% transmute(away = nickname, away_actual = name), by = "away") 


thorn_singles_named %>% transmute(player = home) %>% 
  bind_rows(thorn_singles_named %>% transmute(player = away)) %>% 
  count(player, sort = TRUE) %>% 
  left_join(name_list %>% transmute(player = nickname, name), by = "player") %>% 
  knitr::kable()


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

all_players_total <- 
  slate_players %>% 
  transmute(player, name, slate_games = num_games) %>% 
  full_join(happy_players %>% transmute(player, name, happy_games = num_games), by = c("player", "name")) %>% 
  mutate_if(is.numeric, ~ replace_na(., 0)) %>% 
  group_by(name) %>% 
  summarize(slate_games = sum(slate_games), happy_games = sum(happy_games)) %>% 
  mutate(
    total_games = happy_games + slate_games
  ) %>% 
  arrange(desc(total_games))

# Slate
#slate_url_list <- 
#  c(
#    str_c("https://nomadpool.com/games?page=", 8:2, "&status=Final&venue_id=54"),
#    "https://nomadpool.com/games?status=Final&venue_id=54"
#  )

#slate_df <- 
#  slate_url_list %>% 
#  url_list_to_nickname_df()

nomad_games <- 
  bind_rows(
    slate_df %>% mutate(league = "slate"),
    happy_df %>% mutate(league = "happy")
  ) %>% 
  inner_join(name_list %>% transmute(home = nickname, home_actual = name), by = "home") %>% 
  inner_join(name_list %>% transmute(home2 = nickname, home2_actual = name), by = "home2") %>% 
  inner_join(name_list %>% transmute(away = nickname, away_actual = name), by = "away") %>% 
  inner_join(name_list %>% transmute(away2 = nickname, away2_actual = name), by = "away2") %>% 
  transmute(
    league,
    match_type = "tournament",
    season,
    match_date = date, 
    week_number = 75,
    home_team = NA_character_,
    away_team = NA_character_,
    game_num,
    home = home_actual,
    home2 = home2_actual,
    away = away_actual,
    away2 = away2_actual,
    game_winner,
    forfeit = NA_character_,
    game_type
  ) %>% 
  arrange(match_date, game_num)

nomad_players <- 
  bind_rows(
    nomad_games %>% transmute(player = home),
    nomad_games %>% transmute(player = home2),
    nomad_games %>% transmute(player = away),
    nomad_games %>% transmute(player = away2)
  ) %>% 
  group_by(player) %>% 
  count(name = "num_games") %>% 
  arrange(-num_games) %>% 
  ungroup()

latest_results_date <- 
  list.files("match_data", pattern = "results_no_forfeits") %>% 
  str_extract("\\d+-\\d+-\\d+") %>% 
  max()

#latest_results_date <- "2019-03-12"

results_no_forfeits_path <-
  str_c("match_data/results_no_forfeits_", latest_results_date, ".Rdata")

results_no_forfeits <- 
  results_no_forfeits_path %>% 
  read_rds()

all_games <- 
  results_no_forfeits %>% 
  add_column(home2 = NA_character_, .after = "home") %>% 
  add_column(away2 = NA_character_, .after = "away") %>% 
  mutate(
    home2 = if_else(is.na(home2), home, home2),
    away2 = if_else(is.na(away2), away, away2),
    match_date = parse_date_time(match_date, orders = "%Y-%m-%d"),
    game_type = "8ball"
  ) %>% 
  bind_rows(nomad_games)

results_df <- all_games

games_involving_player <- function(player_of_interest, results_df) {
  results_df %>% 
    filter(
      home == player_of_interest | home2 == player_of_interest |
        away == player_of_interest | away2 == player_of_interest
    ) %>% 
    transmute(
      match_date,
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
    ratings_df$num_games[ratings_df$player == player_of_interest]

  if (game_count == 0) {
    return(
      ratings_df %>% 
        filter(player == player_of_interest) %>% 
        select(rating) %>% 
        pull()
    )
  }
  
  b <- (a - 1) / 500
  
  games_involving_player(player_of_interest, results_df) %>% 
    append_ratings(ratings_df) %>% 
    mutate(
      A = 1 / ((max_team_rating + min_team_rating) / 2 + (max_opponent_rating + min_opponent_rating) / 2),
      W = if_else(game_result == "W", 1, 0),
      decay = case_when(
        season == "Spring 2016" ~ 0.4,
        season == "Fall 2016" ~ 0.5,
        season == "Spring 2017" ~ 0.6,
        season == "Fall 2017" ~ 0.7,
        season == "Spring 2018" ~ 0.8,
        season == "Fall 2018" ~ 0.9,
        season == "Spring 2019" ~ 1.0,
        season == "Fall 2019" ~ 1.0
      ),
      scotch_wt = if_else(str_detect(game_type, "scotch"), 0.5, 1)
    ) %>% 
    summarize(rating = ((a - 1) + sum(W * decay * scotch_wt)) / (b + sum(A * decay * scotch_wt))) %>% 
    pull(rating)
}

results_to_ratings <- function(results_df, a, mae_stop = 100) {
  player_list <- 
    bind_rows(
      results_df %>% transmute(player = home),
      results_df %>% transmute(player = home2),
      results_df %>% transmute(player = away),
      results_df %>% transmute(player = away2)
    ) %>% 
    distinct() %>% 
    arrange(player) %>% 
    pull()
  
  print(str_c("Number of players: ", len(player_list)))
  print(str_c("Number of games: ", results_df %>% count() %>% pull(n)))
  
  count_games_by_player <- function(player_of_interest) {
    games_involving_player(player_of_interest, results_df) %>% 
      count() %>% 
      transmute(player = player_of_interest, num_games = n)
  }
  
  ratings_df <- 
    map_dfr(player_list, count_games_by_player) %>% 
    arrange(desc(num_games)) %>% 
    mutate(rating = 500)
  
  abs_diff <- 100000
  while (abs_diff > mae_stop) {
    old_ratings <- ratings_df %>% pull(rating)
    for (i in 1:nrow(ratings_df)) {
      player_name <- ratings_df$player[i]
      ratings_df$rating[i] <- 
        get_updated_rating(player_name, results_df = results_df, ratings_df = ratings_df, a = a)
    }
    new_ratings <- ratings_df %>% pull(rating)
    print(str_c("Mean absolute difference: ", sum(abs(old_ratings - new_ratings))))
    abs_diff <- sum(abs(old_ratings - new_ratings))
  }
  
  ratings_df
}

test_ratings <- 
  results_to_ratings(all_games %>% filter(!str_detect(game_type, "scotch")), a = 3, mae_stop = 100) %>% 
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


games_involving_player("Evan Burgess", all_games) %>% 
  knitr::kable()

set.seed(1)
all_games %>% 
  filter(str_detect(game_type, "scotch")) %>% 
  select(home:game_winner) %>% 
  mutate(shuffle = sample(c(0, 1), size = nrow(.), replace = TRUE)) %>% 
  mutate(
    player = if_else(shuffle == 0, home, away),
    teammate = if_else(shuffle == 0, home2, away2),
    opponent1 = if_else(shuffle == 0, away, home),
    opponent2 = if_else(shuffle == 0, away2, home2),
    game_winner = if_else(shuffle == 0, "home", "away")
  ) %>% 
  inner_join(test_ratings %>% transmute(player, player_rating = rating), by = "player") %>% 
  inner_join(test_ratings %>% transmute(teammate = player, teammate_rating = rating), by = "teammate") %>% 
  inner_join(test_ratings %>% transmute(opponent1 = player, opponent1_rating = rating), by = "opponent1") %>% 
  inner_join(test_ratings %>% transmute(opponent2 = player, opponent2_rating = rating), by = "opponent2") %>% 
  select(-c(home:away2, shuffle)) %>% 
  mutate(
    Hmax = pmax(player_rating, teammate_rating),
    Hmin = pmin(player_rating, teammate_rating),
    Amax = pmax(opponent1_rating, opponent2_rating),
    Amin = pmin(opponent1_rating, opponent2_rating)
  ) %>% 
  mutate(
    home_win = if_else(game_winner == "home", 1, 0),
    win_prob = 1 / (1 + exp(((Amax + Amin) / 2 - (Hmax + Hmin) / 2) / 144)),
    win_prob_round = round(win_prob, 1)
  ) %>% 
  group_by(win_prob_round) %>% 
  summarize(
    mean_home_win = mean(home_win),
    total_games = n()
  ) %>% 
  ggplot(aes(x = win_prob_round, y = mean_home_win)) +
  geom_abline(slope = 1, intercept = 0, color = "white", size = 2) +
  geom_point() +
  coord_fixed() +
  scale_x_continuous(
    limits = c(0, 1),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    expand = c(0, 0)
  )
  
  