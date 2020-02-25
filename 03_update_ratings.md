SFPA Ratings v.1
================
Skip Perry
January 2020

``` r
latest_results_date <- 
  list.files("match_data", pattern = "results_no_forfeits") %>% 
  str_extract("\\d+-\\d+-\\d+") %>% 
  max()

results_no_forfeits <- 
  str_c("match_data/results_no_forfeits_", latest_results_date, ".Rdata") %>% 
  read_rds()

# Computes udpated ratings for all players until the absolute difference 
# in the vector of ratings between iterations reaches the desired threshold
# Usually takes about 20-30 iterations to converge
results_to_ratings <- function(results_df, a, stop_value = 100) {
  start_time <- Sys.time()

  # Set up initial ratings at 500
  fargo_ratings <- 
    bind_rows(
      results_df %>% transmute(player = home), 
      results_df %>% transmute(player = away)
    ) %>% 
    distinct() %>% 
    arrange(player) %>% 
    mutate(rating = 500)
  
  # Creates a data frame with columns for season-player-opponent-result-decay 
  # for a single player. This definition is inside the function because it uses 
  # results_df which is passed into results_to_ratings.
  collect_game_results <- function(player_of_interest) {
    results_df %>% 
      filter(away == player_of_interest | home == player_of_interest) %>% 
      transmute(
        season,
        match_date,
        player = player_of_interest,
        opponent = if_else(home == player_of_interest, away, home),
        game_result = case_when(
          home == player_of_interest & game_winner == "home" ~ "W",
          away == player_of_interest & game_winner == "away" ~ "W",
          TRUE ~ "L"
        )
      )
  }

  # This un-tidy data frame has 2 rows for every game played. For each game, 
  # one row has player 1 as the target and another row has player 2 as the 
  # target. Results can then be grouped by player and produce a fast,
  # vectorized rating update after joining to the latest rating predictions. 
  # Adds decay step factor of 5% for every six months from the latest game. 
  collected_game_results <- 
    map_dfr(fargo_ratings %>% pull(player), collect_game_results) %>% 
    mutate(
      max_date = max(match_date),
      date_diff = interval(max_date, match_date) %/% months(6),
      decay = 1 + date_diff * 0.05
    ) %>% 
    select(-max_date, -date_diff)
  
  # Set up convergence and MAP parameters 
  abs_diff <- 1e5
  n_iter <- 0
  b <- (a - 1) / 500
  
  # Update ratings using iterative MAP algorithm from 05_info_faq.md
  while (abs_diff > stop_value) {
    n_iter <- n_iter + 1
    
    # Existing vector of player ratings for convergence test
    old_ratings <- fargo_ratings %>% pull(rating)

    # Update rating estimates
    fargo_ratings <- 
      # Start with collected results, arranged by player 
      collected_game_results %>%
      # Add current player ratings
      inner_join(
        fargo_ratings %>% transmute(player, player_rating = rating), 
        by = "player"
      ) %>% 
      # Add current opponent ratings
      inner_join(
        fargo_ratings %>% transmute(opponent = player, opponent_rating = rating), 
        by = "opponent"
      ) %>% 
      # Implement MAP algorithm 
      mutate(
        A = 1 / (player_rating + opponent_rating),
        W = if_else(game_result == "W", 1, 0)
      ) %>% 
      group_by(player) %>% 
      summarize(rating = ((a - 1) + sum(W * decay)) / (b + sum(A * decay)))

    # New vector of player ratings for convergence test
    new_ratings <- fargo_ratings %>% pull(rating) 
    
    print(str_c("Sum of absolute difference: ", sum(abs(old_ratings - new_ratings))))
    
    # Break out of loop if abs_diff < stop_value
    abs_diff <- sum(abs(old_ratings - new_ratings))
  }
  end_time <- Sys.time()
  time_taken <- round(end_time - start_time, 2)

  print(glue("Number of iterations: {n_iter}"))
  print(glue("Time taken: {time_taken} seconds"))
  
  fargo_ratings
}
```

``` r
fargo_df <- 
  results_to_ratings(results_df = results_no_forfeits, a = 3, stop_value = 50) %>% 
  mutate(
    raw_rating = rating,
    rating = log(rating) * 144,
    rating = rating - mean(rating) + 500
  )
```

    ## Note: method with signature 'Timespan#Timespan' chosen for function '%/%',
    ##  target signature 'Interval#Period'.
    ##  "Interval#ANY", "ANY#Period" would also be valid

    ## [1] "Sum of absolute difference: 62394.0071804515"
    ## [1] "Sum of absolute difference: 25001.5515195053"
    ## [1] "Sum of absolute difference: 14914.1044929087"
    ## [1] "Sum of absolute difference: 10550.934031887"
    ## [1] "Sum of absolute difference: 8034.94109191956"
    ## [1] "Sum of absolute difference: 6288.11447385319"
    ## [1] "Sum of absolute difference: 4984.47654555763"
    ## [1] "Sum of absolute difference: 3973.35369572464"
    ## [1] "Sum of absolute difference: 3173.04304063443"
    ## [1] "Sum of absolute difference: 2538.01687719959"
    ## [1] "Sum of absolute difference: 2032.13995820528"
    ## [1] "Sum of absolute difference: 1627.54272791232"
    ## [1] "Sum of absolute difference: 1303.59894265901"
    ## [1] "Sum of absolute difference: 1044.11194018427"
    ## [1] "Sum of absolute difference: 836.491111152716"
    ## [1] "Sum of absolute difference: 670.124954667654"
    ## [1] "Sum of absolute difference: 536.84535362586"
    ## [1] "Sum of absolute difference: 430.101245289751"
    ## [1] "Sum of absolute difference: 344.659558086638"
    ## [1] "Sum of absolute difference: 276.332393806349"
    ## [1] "Sum of absolute difference: 221.711813048033"
    ## [1] "Sum of absolute difference: 178.163864114618"
    ## [1] "Sum of absolute difference: 143.469549753575"
    ## [1] "Sum of absolute difference: 115.946022032994"
    ## [1] "Sum of absolute difference: 94.1687663272772"
    ## [1] "Sum of absolute difference: 77.0815277700129"
    ## [1] "Sum of absolute difference: 63.7550680698949"
    ## [1] "Sum of absolute difference: 53.4342501116157"
    ## [1] "Sum of absolute difference: 45.446346101776"
    ## Number of iterations: 29
    ## Time taken: 4.13 seconds

``` r
fargo_df %>% 
  arrange(desc(rating))
```

    ## # A tibble: 589 x 3
    ##    player          rating raw_rating
    ##    <chr>            <dbl>      <dbl>
    ##  1 Hector Ortega     744.      2251.
    ##  2 Mike Maxwell      737.      2143.
    ##  3 Alvin Ho          712.      1802.
    ##  4 Nick Lansdown     691.      1563.
    ##  5 Diogo Martini     691.      1562.
    ##  6 Ryan Piaget       687.      1521.
    ##  7 Evan Burgess      687.      1513.
    ##  8 Rhys Hughes       685.      1495.
    ##  9 Matt Frisbie      682.      1466.
    ## 10 Thayer McDougle   681.      1456.
    ## # … with 579 more rows

``` r
saveRDS(fargo_df, str_c("fargo_ratings/fargo_", latest_results_date, ".Rdata"))
```

``` r
# Create a single file with all known player ratings
fargo_files <- 
  list.files("fargo_ratings", pattern = "^fargo")

fargo_dates <- 
  fargo_files %>% 
  str_extract("\\d+-\\d+-\\d+") %>% 
  enframe(name = NULL) %>% 
  arrange(value) %>% 
  transmute(
    source = row_number(),
    date = ymd(value)
  )

all_fargo_ratings <- 
  str_c("fargo_ratings/", fargo_files) %>% 
  map_dfr(read_rds, .id = "source") %>% 
  mutate(source = as.numeric(source)) %>% 
  left_join(fargo_dates, by = "source") %>% 
  select(-source)

saveRDS(all_fargo_ratings, str_c("fargo_ratings/all_fargo_ratings.Rdata"))
```
