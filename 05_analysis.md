SFPA Ratings
================
Skip Perry
March 2019

``` r
# Function to get a data frame of matches and real-time ratings for a particular player
player_matches_and_ratings <- function(player_of_interest) {
  df <- 
    results_no_forfeits %>% 
    filter(away == player_of_interest | home == player_of_interest) %>% 
    mutate(
      player = player_of_interest,
      opponent = case_when(
        away == player_of_interest ~ home,
        TRUE ~ away
      ),
      opponent_rating = case_when(
        away == player_of_interest ~ p1_start_rating,
        TRUE ~ p2_start_rating
      ),
      result = case_when(
        away == player_of_interest & game_winner == "away" ~ "W",
        home == player_of_interest & game_winner == "home" ~ "W",
        TRUE ~ "L"
      ),
      new_rating = case_when(
        away == player_of_interest ~ p2_end_rating,
        TRUE ~ p1_end_rating
      )
    ) %>% 
    select(match_date, player, opponent, opponent_rating, result, new_rating)
  
  row1_start_date <- df %>% pull(match_date) %>% min() - 7
  
  new_row1 <- 
    tribble(
      ~match_date, ~player, ~opponent, ~opponent_rating, ~result, ~new_rating,
      row1_start_date, player_of_interest, NA, NA, NA, 1500
    )
  
  bind_rows(new_row1, df)
}

win_loss_record <- function(player_of_interest) {
  player_matches_and_ratings(player_of_interest = player_of_interest) %>% 
    slice(-1) %>% 
    group_by(result) %>% 
    count() %>% 
    arrange(desc(result))
}

#win_loss_record(player_of_interest = "Mike Maxwell")
#player_matches_and_ratings(player_of_interest = "Mike Maxwell")

# Helper function to add team names to a data frame containing a column for player
append_team_names <- function(df) {
  df %>% 
    full_join(players_by_team_19, by = "player")
}

# Helper functions to get a list of all the players in a team (ever, or just now)
get_team_players <- function(team_name) {
  home_players <- 
    results_no_forfeits %>% 
    filter(home_team == team_name) %>% 
    select(home) %>% 
    distinct() %>% 
    pull()
  
  away_players <- 
    results_no_forfeits %>% 
    filter(away_team == team_name) %>% 
    select(away) %>% 
    distinct() %>% 
    pull()
  
  unique(c(home_players, away_players))
}

get_current_team_players <- function(team_name) {
  home_players <- 
    results_19_no_forfeits %>% 
    filter(home_team == team_name) %>% 
    select(home) %>% 
    distinct() %>% 
    pull()
  
  away_players <- 
    results_19_no_forfeits %>% 
    filter(away_team == team_name) %>% 
    select(away) %>% 
    distinct() %>% 
    pull()
  
  unique(c(home_players, away_players))
}

# Data frame of players and team names in Spring 2019 - helper for next helper function
players_by_team_19 <- 
  bind_rows(
    results_19_no_forfeits %>% transmute(player = home, team = home_team),
    results_19_no_forfeits %>% transmute(player = away, team = away_team)
  ) %>% 
    distinct() %>% 
    arrange(team, player)

# List of the top 10 players active in Spring 2019
top_players <-
  elo_ratings %>% 
  arrange(desc(rating)) %>% 
  append_team_names() %>% 
  filter(!is.na(team)) %>% 
  slice(1:10) %>% 
  pull(player)
```

``` r
# Show best players
elo_ratings %>% 
  mutate(rating = round(rating)) %>% 
  arrange(desc(rating)) %>% 
  append_team_names()
```

    ## # A tibble: 327 x 3
    ##    player          rating team                      
    ##    <chr>            <dbl> <chr>                     
    ##  1 Skip Perry        1803 Tandy Tokers              
    ##  2 Hector Ortega     1789 <NA>                      
    ##  3 Ryan Piaget       1770 Clean Slate               
    ##  4 Mike Maxwell      1760 Route 101 Rawhides        
    ##  5 Wyatt Moss        1724 Naked Lunch Nice Rack     
    ##  6 Evan Burgess      1710 Lucky Horseshoe Caballeros
    ##  7 Tom Seymour       1698 Route 101 Rawhides        
    ##  8 Andy Luong        1697 <NA>                      
    ##  9 Rhys Hughes       1696 Golden Slate Warriors     
    ## 10 Thayer McDougle   1690 Lucky Horseshoe Caballeros
    ## # … with 317 more rows

``` r
# Plot player ratings over time
plot_player_ratings_by_group <- function(player_list, list_name) {
  map_dfr(player_list, player_matches_and_ratings) %>% 
    group_by(match_date, player) %>% 
    slice(n()) %>% 
    ggplot(aes(x = match_date, y = new_rating, group = player, color = player)) +
    geom_hline(yintercept = 1500, color = "white", size = 2) +
    geom_line() +
    geom_point(size = 0.8) +
    labs(
      x = "Match Date", y = "Rating",
      title = str_c(list_name, " Player Ratings")
    ) +
    theme(
      legend.title = element_blank(),
      plot.title = element_text(hjust = 0.5),
      legend.position = "bottom",
      legend.text = element_text(size = 10)
  )
}

plot_player_ratings_by_group(
  player_list = top_players,
  list_name = "Top 10"
)
```

![](05_analysis_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
plot_player_ratings_by_group(
  player_list = get_current_team_players("Tandy Tokers"),
  list_name = "Tandy Tokers"
)
```

![](05_analysis_files/figure-markdown_github/unnamed-chunk-2-2.png)

``` r
plot_player_ratings_by_group(
  player_list = get_current_team_players("Lucky Horseshoe Caballeros"),
  list_name = "Lucky Horseshoe Caballeros"
)
```

![](05_analysis_files/figure-markdown_github/unnamed-chunk-2-3.png)

``` r
# Player improvement in the 2019 season 
player_names_2019 <- 
  bind_rows(
    results_19_no_forfeits %>% select(home) %>% transmute(player = home),
    results_19_no_forfeits %>% select(away) %>% transmute(player = away)
  ) %>% 
    distinct() %>% 
    arrange(player)

current_players_elo_history <-
  map_dfr(player_names_2019 %>% pull(), player_matches_and_ratings)

matches_current_season <- 
  current_players_elo_history %>% 
  filter(str_detect(match_date, "2019"))

final_pre_2019_matches <-
  current_players_elo_history %>% 
  filter(str_detect(match_date, "2018")) %>% 
  group_by(player) %>% 
  slice(n())

df <- 
  bind_rows(matches_current_season, final_pre_2019_matches) %>% 
  arrange(player, match_date)

ratings_changes <- 
  df %>% 
    group_by(player) %>% 
    slice(1, n()) %>% 
    select(player, new_rating) %>% 
    mutate(key = c("initial", "final")) %>% 
    spread(key = "key", value = "new_rating") %>% 
    mutate(diff = final - initial) %>% 
    arrange(desc(diff)) %>% 
    mutate(new_old = if_else(initial == 1500, "New player", "Old player")) %>% 
    append_team_names() %>% 
    select(player, initial, final, diff, new_old, team)

#ratings_changes %>% 
  #filter(new_old == "New player") %>% 
#  ggplot(aes(x = reorder(player, diff), y = diff, fill = new_old)) +
#  geom_col() +
#  theme(
#    axis.text.x = element_blank(),
#    axis.ticks.x = element_blank(),
#    axis.title = element_blank(),
#    legend.position = "bottom",
#    plot.title = element_text(hjust = 0.5)
#  ) +
#  labs(
#    title = "ELO Rating Changes by Player, Spring 2019"
#  )

ratings_changes
```

    ## # A tibble: 211 x 6
    ## # Groups:   player [211]
    ##    player         initial final  diff new_old    team                    
    ##    <chr>            <dbl> <dbl> <dbl> <chr>      <chr>                   
    ##  1 Jon Williams     1500  1614. 114.  New player Cafe Ballbusters        
    ##  2 Tae Yim          1500  1609. 109.  New player Cafe 2 for 1's          
    ##  3 Erik Proctor     1377. 1473.  95.4 Old player Cafe Cafaholics         
    ##  4 Troy Brunet      1361. 1453.  91.8 Old player Hole in the Wall Bangers
    ##  5 Darrell Haslip   1570. 1657.  87.0 Old player Smoke & Rumors          
    ##  6 Justin Taylor    1508. 1594.  85.6 Old player Mixfits                 
    ##  7 Rene Denis       1564. 1649.  84.7 Old player Smoke & Rumors          
    ##  8 Jason Rogers     1511. 1591.  79.9 Old player Clean Slate             
    ##  9 Bob Rice         1500  1573.  72.8 New player Cafe Cafaholics         
    ## 10 David Norris     1452. 1521.  68.9 Old player House of Ginger         
    ## # … with 201 more rows

``` r
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
        away_team == team_of_interest ~ t1_start_rating,
        TRUE ~ t2_start_rating
      ),
      result = case_when(
        away_team == team_of_interest & game_winner == "away" ~ "W",
        home_team == team_of_interest & game_winner == "home" ~ "W",
        TRUE ~ "L"
      ),
      new_rating = case_when(
        away_team == team_of_interest ~ t2_end_rating,
        TRUE ~ t1_end_rating
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

![](05_analysis_files/figure-markdown_github/unnamed-chunk-5-1.png)

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

![](05_analysis_files/figure-markdown_github/unnamed-chunk-6-1.png)

``` r
# Show plot with ratings by team and player to see best teams 
# Includes column for the best 4 players on the team 
elo_player_team_ratings <- 
  elo_ratings %>% 
  append_team_names() %>% 
  filter(!is.na(team)) %>% 
  group_by(team) %>% 
  arrange(team, desc(rating)) %>% 
  mutate(
    team_rank = row_number(),
    top4 = team_rank < 5
  ) %>% 
  ungroup() %>% 
  mutate(qtile = as.factor(ntile(rating, 6)))

elo_player_team_ratings %>% 
  group_by(team) %>% 
  mutate(team_mean = mean(rating)) %>% 
  ggplot(aes(x = reorder(team, team_mean))) +
  geom_point(aes(y = team_mean), color = "gray50", size = 1.7, alpha = 0.5) +
  geom_point(aes(y = rating, color = qtile), alpha = 0.5) +
  coord_flip() +
  theme(
    axis.title = element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(
    title = "Ratings by Team and Player (Full Team)",
    color = "Sextile"
  )
```

![](05_analysis_files/figure-markdown_github/unnamed-chunk-7-1.png)

``` r
elo_player_team_ratings %>% 
  filter(top4 == TRUE) %>% 
  group_by(team) %>% 
  mutate(team_mean = mean(rating)) %>% 
  ggplot(aes(x = reorder(team, team_mean))) +
  geom_point(aes(y = team_mean), color = "gray50", size = 1.7, alpha = 0.5) +
  geom_point(aes(y = rating, color = qtile), alpha = 0.5) +
  coord_flip() +
  theme(
    axis.title = element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(
    title = "Ratings by Team and Player (Top 4 Only)",
    color = "Sextile"
  )
```

![](05_analysis_files/figure-markdown_github/unnamed-chunk-7-2.png)