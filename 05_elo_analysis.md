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

# Helper function to add team names to a data frame containing a column for player
append_team_names <- function(df) {
  df %>% 
    full_join(players_by_team_19, by = "player")
}

# Data frame of players and team names in Spring 2019 - helper for next helper function
players_by_team_19 <- 
  bind_rows(
    results_19_no_forfeits %>% transmute(player = home, team = home_team),
    results_19_no_forfeits %>% transmute(player = away, team = away_team)
  ) %>% 
  distinct() %>% 
  arrange(team, player) %>% 
  filter(!is.na(team))

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
  append_team_names() %>% 
  head(25) %>% 
  knitr::kable()
```

| player           | rating | team                       |
| :--------------- | -----: | :------------------------- |
| Skip Perry       |   1830 | Tandy Tokers               |
| Ryan Piaget      |   1798 | Clean Slate                |
| Hector Ortega    |   1789 | NA                         |
| Evan Burgess     |   1770 | Lucky Horseshoe Caballeros |
| Mike Maxwell     |   1768 | Route 101 Rawhides         |
| Tom Seymour      |   1742 | Route 101 Rawhides         |
| Thayer McDougle  |   1730 | Lucky Horseshoe Caballeros |
| Wyatt Moss       |   1726 | Naked Lunch Nice Rack      |
| Diogo Martini    |   1712 | Golden Slate Warriors      |
| Dave Ward        |   1710 | Dovre & Out                |
| Bob Simon        |   1705 | Route 101 Rawhides         |
| Andy Luong       |   1697 | NA                         |
| Nick Callado     |   1687 | NA                         |
| Stefano Lopez    |   1684 | NA                         |
| Chris DuCoing    |   1682 | Smoke & Rumors             |
| Patty West       |   1681 | Golden Slate Warriors      |
| Hugo Valseca     |   1680 | NA                         |
| Rhys Hughes      |   1680 | Golden Slate Warriors      |
| Danny Mullan     |   1674 | Route 101 Rawhides         |
| Joshua Maldonado |   1672 | Route 101 Rawhides         |
| Ben Green        |   1669 | Golden Slate Warriors      |
| Rene Denis       |   1669 | Smoke & Rumors             |
| James Neale      |   1656 | Lucky Horseshoe Caballeros |
| Jesse La Fear    |   1656 | NA                         |
| Eugene Fan       |   1653 | Rumors Never Die           |

``` r
# Plot player ratings over time
plot_player_ratings_by_group <- function(player_list, list_name) {
  map_dfr(player_list, player_matches_and_ratings) %>% 
    #filter(match_date > "2018-12-11") %>% 
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

![](05_elo_analysis_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
plot_player_ratings_by_group(
  player_list = get_current_team_players("Tandy Tokers"),
  list_name = "Tandy Tokers"
)
```

![](05_elo_analysis_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->

``` r
plot_player_ratings_by_group(
  player_list = get_current_team_players("Lucky Horseshoe Caballeros"),
  list_name = "Lucky Horseshoe Caballeros"
)
```

![](05_elo_analysis_files/figure-gfm/unnamed-chunk-2-3.png)<!-- -->

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
    select(player, initial, final, diff, new_old, team) %>%
    ungroup()

# Players with the biggest improvement
ratings_changes %>% 
  transmute(
    player, initial = round(initial), final = round(final), 
    diff = round(diff), team, new_old
  ) %>% 
  head(25) %>% 
  knitr::kable()
```

| player            | initial | final | diff | team                         | new\_old   |
| :---------------- | ------: | ----: | ---: | :--------------------------- | :--------- |
| Jon Williams      |    1500 |  1635 |  135 | Cafe Ballbusters             | New player |
| Tae Yim           |    1500 |  1621 |  121 | Cafe 2 for 1’s               | New player |
| Mathieu Guglielmi |    1406 |  1519 |  112 | Tandy Tokers                 | Old player |
| John McNulty      |    1371 |  1481 |  110 | Lone Star Longhorns          | Old player |
| Rene Denis        |    1564 |  1669 |  105 | Smoke & Rumors               | Old player |
| Mark Butler       |    1469 |  1570 |  101 | Golden Slate Warriors        | Old player |
| Ryan Piaget       |    1701 |  1798 |   97 | Clean Slate                  | Old player |
| Troy Brunet       |    1361 |  1455 |   94 | Hole in the Wall Bangers     | Old player |
| Julien Roeser     |    1458 |  1548 |   90 | Lucky Horseshoe Glue Factory | Old player |
| Chris DuCoing     |    1597 |  1682 |   85 | Smoke & Rumors               | Old player |
| Skip Perry        |    1745 |  1830 |   85 | Tandy Tokers                 | Old player |
| Nick Lansdown     |    1558 |  1643 |   85 | Lucky Horseshoe Caballeros   | Old player |
| Alex Peralta      |    1500 |  1583 |   83 | Rumors Never Die             | New player |
| Tom Seymour       |    1659 |  1742 |   83 | Route 101 Rawhides           | Old player |
| Darrell Haslip    |    1570 |  1651 |   81 | Smoke & Rumors               | Old player |
| Ari Fehrenkamp    |    1367 |  1447 |   80 | Wicked Bitches of the West   | Old player |
| Joel Talevi       |    1573 |  1651 |   78 | Clean Slate                  | Old player |
| Anthony Hydron    |    1441 |  1517 |   75 | Lucky Horseshoe Glue Factory | Old player |
| Ninad Desei       |    1368 |  1444 |   75 | Pilsner Penguins             | Old player |
| Jason Rogers      |    1512 |  1585 |   73 | Clean Slate                  | Old player |
| Thayer McDougle   |    1659 |  1730 |   71 | Lucky Horseshoe Caballeros   | Old player |
| Wyatt Moss        |    1656 |  1726 |   70 | Naked Lunch Nice Rack        | Old player |
| Keith Deming      |    1423 |  1493 |   70 | Naked Lunch Nice Rack        | Old player |
| Gerlie Mendoza    |    1501 |  1565 |   64 | Pilsner Penguins             | Old player |
| Patty West        |    1619 |  1681 |   62 | Golden Slate Warriors        | Old player |

``` r
# Players with the biggest declines
ratings_changes %>% 
  transmute(
    player, initial = round(initial), final = round(final), 
    diff = round(diff), team, new_old
  ) %>% 
  arrange(diff) %>% 
  head(25) %>% 
  knitr::kable()
```

| player            | initial | final |  diff | team                         | new\_old   |
| :---------------- | ------: | ----: | ----: | :--------------------------- | :--------- |
| Levon Sanossian   |    1500 |  1327 | \-173 | Lone Star Rebels             | New player |
| Kurt Weitzmann    |    1500 |  1330 | \-170 | Black Willows                | New player |
| Brady Ralston     |    1500 |  1357 | \-143 | Lone Star Longhorns          | New player |
| Keelin Ingoldsby  |    1500 |  1373 | \-127 | Harry’s Humdingers           | New player |
| Dorien Lezinski   |    1500 |  1383 | \-117 | House of Ginger              | New player |
| Jonathen Diego    |    1500 |  1387 | \-113 | House of Ginger              | New player |
| Fran Herman       |    1600 |  1494 | \-106 | Pilsner Penguins             | Old player |
| Tetyana Swan      |    1500 |  1396 | \-104 | Lone Star Longhorns          | New player |
| Austin Day        |    1500 |  1396 | \-104 | Cafe 2 for 1’s               | New player |
| Sheree Taft       |    1574 |  1476 |  \-98 | Cafe Cafaholics              | Old player |
| Gerald Borjas     |    1461 |  1377 |  \-84 | Hole in the Wall Howlers     | Old player |
| Robbie Connelly   |    1396 |  1316 |  \-81 | Lone Star Rebels             | Old player |
| John Larkin       |    1500 |  1419 |  \-81 | Harry’s Humdingers           | New player |
| Tim Doyle         |    1452 |  1371 |  \-81 | Bare Naked 6 Holes           | Old player |
| Peter Lee         |    1509 |  1429 |  \-80 | Ginger Strokes               | Old player |
| Carlos Rodrigues  |    1500 |  1427 |  \-73 | Lucky Horseshoe Glue Factory | New player |
| Emily Adams       |    1570 |  1498 |  \-72 | Pilsner Innmates             | Old player |
| Aja Cayetano      |    1279 |  1208 |  \-70 | Harry’s Humdingers           | Old player |
| Caleb Christian   |    1500 |  1431 |  \-69 | Hole in the Wall Bangers     | New player |
| Sharon Yencharis  |    1500 |  1432 |  \-68 | Lone Star Rebels             | New player |
| Eric Kalisa       |    1519 |  1451 |  \-68 | Tandy Tokers                 | Old player |
| Antonio Herrera   |    1468 |  1402 |  \-66 | Cafe Cafaholics              | Old player |
| Steven Chamberlin |    1408 |  1343 |  \-65 | Cinchsationals               | Old player |
| Ian Montbrun      |    1585 |  1522 |  \-63 | Cinch You’re Down There      | Old player |
| Nick Giangreco    |    1647 |  1585 |  \-62 | Lone Star Rebels             | Old player |

``` r
# Data frame of all team names for all seasons
team_names <- 
  bind_rows(
    results_no_forfeits %>% transmute(team = home_team),
    results_no_forfeits %>% transmute(team = away_team)
  ) %>% 
  distinct() %>% 
  arrange(team) %>% 
  filter(!is.na(team))

# Data frame of team names in the current Spring 2019 season
team_names_2019 <- 
  bind_rows(
    results_19_no_forfeits %>% transmute(team = home_team),
    results_19_no_forfeits %>% transmute(team = away_team)
  ) %>% 
  distinct() %>% 
  arrange(team) %>% 
  filter(!is.na(team))

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
  filter(!is.na(team)) %>% 
  group_by(match_date, team) %>% 
  slice(n()) %>% 
  ungroup() %>% 
  mutate(new_rating = round(new_rating))

win_pct_2019 <- 
  team_matches_and_ratings %>% 
  filter(!is.na(result), !is.na(team), match_date > "2019-01-01") %>% 
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

![](05_elo_analysis_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

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

![](05_elo_analysis_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

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

![](05_elo_analysis_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

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

![](05_elo_analysis_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->
