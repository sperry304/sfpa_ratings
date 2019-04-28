SFPA Team ELO Ratings
================
Skip Perry
3/15/2019

``` r
# Set K based on optimization worksheet
K <- 23

# Data frame of players and team names in Spring 2019 - helper for next helper function
players_by_team_19 <- 
  bind_rows(
    results_19_no_forfeits %>% transmute(player = home, team = home_team),
    results_19_no_forfeits %>% transmute(player = away, team = away_team)
  ) %>% 
    distinct() %>% 
    arrange(team, player)

# Helper function to add team names to a data frame containing a column for player
append_team_names <- function(df) {
  df %>% 
    full_join(players_by_team_19, by = "player")
}

# Helper function to get a player's rating
get_rating <- function(player_name, ratings_df) {
  ratings_df %>% 
    filter(player == player_name) %>% 
    pull(rating)
}

# Helper function to get player list
full_player_list <- function(df) {
  bind_rows(
    df %>% transmute(player = home), 
    df %>% transmute(player = away)
  ) %>% 
    distinct(player) %>% 
    arrange(player)
}

# Helper function for ELO match win probability
match_win_probability <- function(player_of_interest, home_rating, away_rating, home_advantage) {
  if (player_of_interest == "home") {
    prob <- 1 / (1 + 10 ^ ((away_rating - (home_rating + home_advantage)) / 400))
  } else {
    prob <- 1 / (1 + 10 ^ (((home_rating + home_advantage) - away_rating) / 400))
  }
  prob
}

# Construct summary data frame of base 1500 ratings using the distinct players seen in the time period
elo_ratings <-
  full_player_list(results_no_forfeits) %>% 
  mutate(rating = 1500)

# Update the ratings table
for (i in 1:nrow(results_no_forfeits)) {
  player1 <- results_no_forfeits$home[i]
  player2 <- results_no_forfeits$away[i]
  player1_rating <- get_rating(player1, elo_ratings)
  player2_rating <- get_rating(player2, elo_ratings)
  results_no_forfeits$p1_start_rating[i] <- player1_rating
  results_no_forfeits$p2_start_rating[i] <- player2_rating
  player1_expected <-
    match_win_probability(
      player_of_interest = "home", home_rating = player1_rating, away_rating = player2_rating,
      home_advantage = home_advantage
    )
  player2_expected <-
    match_win_probability(
      player_of_interest = "away", home_rating = player1_rating, away_rating = player2_rating,
      home_advantage = home_advantage
    )
  winner <- results_no_forfeits$game_winner[i]
  S1 <- ifelse(winner == "home", 1, 0)
  S2 <- ifelse(winner == "home", 0, 1)
  player1_rating_new = player1_rating + K * (S1 - player1_expected)
  player2_rating_new = player2_rating + K * (S2 - player2_expected)
  elo_ratings$rating[elo_ratings$player == player1] <- player1_rating_new
  elo_ratings$rating[elo_ratings$player == player2] <- player2_rating_new
  results_no_forfeits$p1_end_rating[i] <- player1_rating_new
  results_no_forfeits$p2_end_rating[i] <- player2_rating_new
}

elo_ratings %>% 
  mutate(rating = round(rating)) %>% 
  arrange(-rating) %>% 
  append_team_names()
```

    ## # A tibble: 346 x 3
    ##    player          rating team                      
    ##    <chr>            <dbl> <chr>                     
    ##  1 Ryan Piaget       1798 Clean Slate               
    ##  2 Skip Perry        1798 Tandy Tokers              
    ##  3 Evan Burgess      1789 Lucky Horseshoe Caballeros
    ##  4 Evan Burgess      1789 <NA>                      
    ##  5 Hector Ortega     1789 <NA>                      
    ##  6 Mike Maxwell      1758 Route 101 Rawhides        
    ##  7 Mike Maxwell      1758 <NA>                      
    ##  8 Chris DuCoing     1730 Smoke & Rumors            
    ##  9 Tom Seymour       1729 Route 101 Rawhides        
    ## 10 Thayer McDougle   1726 Lucky Horseshoe Caballeros
    ## # … with 336 more rows

``` r
# Set K based on optimization
K_team <- 4

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
get_team_rating <- function(team_name, ratings_df) {
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
  if (is.na(team1)) { 
    next 
  } 
  team1_rating <- get_team_rating(team1, elo_team_ratings)
  team2_rating <- get_team_rating(team2, elo_team_ratings)
  results_no_forfeits$t1_start_rating[i] <- team1_rating
  results_no_forfeits$t2_start_rating[i] <- team2_rating
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
  team1_rating_new <- team1_rating + K_team * (S1 - team1_expected)
  team2_rating_new <- team2_rating + K_team * (S2 - team2_expected)
  elo_team_ratings$rating[elo_team_ratings$team == team1] <- team1_rating_new
  elo_team_ratings$rating[elo_team_ratings$team == team2] <- team2_rating_new
  results_no_forfeits$t1_end_rating[i] <- team1_rating_new
  results_no_forfeits$t2_end_rating[i] <- team2_rating_new
}

elo_team_ratings %>% 
  inner_join(team_names_2019, by = "team") %>% 
  mutate(rating = round(rating)) %>% 
  arrange(-rating)
```

    ## # A tibble: 35 x 2
    ##    team                       rating
    ##    <chr>                       <dbl>
    ##  1 Route 101 Rawhides           1663
    ##  2 Lucky Horseshoe Caballeros   1650
    ##  3 Golden Slate Warriors        1641
    ##  4 Clean Slate                  1608
    ##  5 Smoke & Rumors               1606
    ##  6 Cafe Ballbusters             1559
    ##  7 Dovre & Out                  1555
    ##  8 Tandy Tokers                 1550
    ##  9 Cinch You're Down There      1543
    ## 10 Rumors Never Die             1537
    ## # … with 25 more rows

``` r
latest_match_date <- 
  results_no_forfeits %>% 
  pull(match_date) %>% 
  max()

saveRDS(elo_ratings, str_c("other_data/elo_ratings_", latest_match_date, ".Rdata"))
saveRDS(elo_team_ratings, str_c("other_data/elo_team_ratings_", latest_match_date, ".Rdata"))
saveRDS(results_no_forfeits, str_c("match_data/results_no_forfeits_", latest_match_date, ".Rdata"))
```
