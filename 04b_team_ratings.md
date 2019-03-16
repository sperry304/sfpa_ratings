SFPA Team ELO Ratings
================
Skip Perry
3/15/2019

``` r
# Set K based on optimization
K <- 4

# Data frame of all team names for all seasons
team_names <- 
  bind_rows(
    results_no_forfeits %>% transmute(team = home_team),
    results_no_forfeits %>% transmute(team = away_team)
  ) %>% 
  distinct() %>% 
  arrange(team)

# Data frame of team names in the current Spring 2019 season
team_names_2019 <- 
  bind_rows(
    results_19_no_forfeits %>% transmute(team = home_team),
    results_19_no_forfeits %>% transmute(team = away_team)
  ) %>% 
  distinct() %>% 
  arrange(team)

# Helper function for ELO match win probability
match_win_probability <- function(team_of_interest, home_rating, away_rating, home_advantage) {
  if (team_of_interest == "home") {
    prob <- 1 / (1 + 10 ^ ((away_rating - (home_rating + home_advantage)) / 400))
  } else {
    prob <- 1 / (1 + 10 ^ (((home_rating + home_advantage) - away_rating) / 400))
  }
  prob
}

# Helper function to get a team's rating
get_rating <- function(team_name, ratings_df) {
  ratings_df %>% 
    filter(team == team_name) %>% 
    pull(rating)
}

# Initialize ratings at 1500
elo_team_ratings <-
  team_names %>% 
  mutate(rating = 1500)

# Populate real-time match ratings throughout results data frame
for (i in 1:nrow(results_no_forfeits)) {
  team1 <- results_no_forfeits$home_team[i]
  team2 <- results_no_forfeits$away_team[i]
  team1_rating <- get_rating(team1, elo_team_ratings)
  team2_rating <- get_rating(team2, elo_team_ratings)
  results_no_forfeits$p1_start_rating[i] <- team1_rating
  results_no_forfeits$p2_start_rating[i] <- team2_rating
  team1_expected <-
    match_win_probability(
      team_of_interest = "home", home_rating = team1_rating, away_rating = team2_rating,
      home_advantage = home_advantage
    )
  team2_expected <-
    match_win_probability(
      team_of_interest = "away", home_rating = team1_rating, away_rating = team2_rating,
      home_advantage = home_advantage
    )
  winner <- results_no_forfeits$game_winner[i]
  S1 <- ifelse(winner == "home", 1, 0)
  S2 <- ifelse(winner == "home", 0, 1)
  team1_rating_new <- team1_rating + K * (S1 - team1_expected)
  team2_rating_new <- team2_rating + K * (S2 - team2_expected)
  elo_team_ratings$rating[elo_team_ratings$team == team1] <- team1_rating_new
  elo_team_ratings$rating[elo_team_ratings$team == team2] <- team2_rating_new
  results_no_forfeits$p1_end_rating[i] <- team1_rating_new
  results_no_forfeits$p2_end_rating[i] <- team2_rating_new
}

#elo_team_ratings %>% 
#  inner_join(team_names_2019, by = "team") %>% 
#  mutate(rating = round(rating)) %>% 
#  arrange(-rating) %>% 
#  knitr::kable()
```

``` r
team_matches_and_ratings <- function(team_of_interest) {
  df <- 
    results_no_forfeits %>% 
    filter(away_team == team_of_interest | home_team == team_of_interest) %>% 
    mutate(
      team = team_of_interest,
      opponent = case_when(
        away_team == team_of_interest ~ home_team,
        TRUE ~ away_team
      ),
      opponent_rating = case_when(
        away_team == team_of_interest ~ p1_start_rating,
        TRUE ~ p2_start_rating
      ),
      result = case_when(
        away_team == team_of_interest & game_winner == "away" ~ "W",
        home_team == team_of_interest & game_winner == "home" ~ "W",
        TRUE ~ "L"
      ),
      new_rating = case_when(
        away_team == team_of_interest ~ p2_end_rating,
        TRUE ~ p1_end_rating
      )
    ) %>% 
    select(match_date, team, opponent, opponent_rating, result, new_rating)
  
  row1_start_date <- df %>% pull(match_date) %>% min() - 7
  
  new_row1 <- 
    tribble(
      ~match_date, ~team, ~opponent, ~opponent_rating, ~result, ~new_rating,
      row1_start_date, team_of_interest, NA, NA, NA, 1500
    )
  
  bind_rows(new_row1, df)
}

team_matches_and_ratings <- 
  map_dfr(team_names_2019 %>% pull(), team_matches_and_ratings)

team_matches_and_ratings_sliced <- 
  team_matches_and_ratings %>% 
  group_by(match_date, team) %>% 
  slice(n()) %>% 
  ungroup() %>% 
  mutate(new_rating = round(new_rating))

#saveRDS(team_matches_and_ratings_sliced, str_c("other_data/team_matches_and_ratings_sliced", today(), ".Rdata"))

win_pct_2019 <- 
  team_matches_and_ratings %>% 
  filter(!is.na(result), match_date > "2019-01-01") %>% 
  group_by(team, result) %>% 
  count() %>% 
  ungroup() %>% 
  spread(result, n) %>% 
  transmute(
    team, 
    win_pct = W / (W + L),
    record = if_else(win_pct >= 0.5, ".500+", "<.500")
  )
```

``` r
# Plot current ratings by team with shading for winning/losing record
elo_team_ratings %>% 
  mutate(rating = round(rating)) %>% 
  inner_join(team_names_2019, by = "team") %>% 
  left_join(win_pct_2019, by = "team") %>%  
  ggplot(aes(x = reorder(team, rating), y = rating, fill = record)) +
  geom_hline(yintercept = 1500, size = 2, color = "white") +
  geom_col() + 
  geom_text(aes(label = rating, y = rating - 20), color = "white", size = 3) + 
  coord_flip(ylim = c(1200, 1700)) +
  scale_y_continuous(
    expand = c(0, 0)
  ) +
  labs(
    y = "Rating",
    title = "SFPA Team ELO Ratings",
    fill = "Record"
  ) + 
  theme(
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )
```

![](04b_team_ratings_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
plot_teams <- function(teams_of_interest) {
  team_matches_and_ratings_sliced %>% 
    ggplot(aes(x = match_date, y = new_rating)) +
    geom_hline(yintercept = 1500, color = "white", size = 2) +
    geom_line(aes(group = team), color = "gray60", alpha = 0.6) +
    geom_point(color = "gray60", size = 0.8, alpha = 0.6) +
    geom_line(aes(color = team, group = team), data = . %>% filter(team %in% teams_of_interest)) +
    geom_point(aes(color = team, group = team), size = 1, data = . %>% filter(team %in% teams_of_interest)) +
    geom_text(
      aes(label = new_rating, color = team, x = match_date + 12), 
      size = 3, 
      data = . %>% filter(team %in% teams_of_interest) %>% group_by(team) %>% slice(n()),
      show.legend = FALSE
    ) +
    labs(
      x = "Match Date", y = "ELO Rating",
      title = "SFPA Team Rating Trends, 2018-2019"
    ) +
    theme(
      legend.title = element_blank(),
      plot.title = element_text(hjust = 0.5),
      legend.position = "bottom"
    )
}

team_list <- c(
  "Tandy Tokers", "Route 101 Rawhides", "Golden Slate Warriors", "Clean Slate",
  "Lucky Horseshoe Caballeros", "Smoke & Rumors"
)

plot_teams(team_list)
```

![](04b_team_ratings_files/figure-markdown_github/unnamed-chunk-5-1.png)