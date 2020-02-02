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
results_to_ratings <- function(results_df, a, mae_stop = 100) {
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
        player = player_of_interest,
        opponent = if_else(home == player_of_interest, away, home),
        game_result = case_when(
          home == player_of_interest & game_winner == "home" ~ "W",
          away == player_of_interest & game_winner == "away" ~ "W",
          TRUE ~ "L"
        ),
        decay = case_when(
          season == "Spring 2012" ~ 0.3,
          season == "Fall 2012" ~ 0.35,
          season == "Spring 2013" ~ 0.4,
          season == "Fall 2013" ~ 0.45,
          season == "Spring 2014" ~ 0.5,
          season == "Fall 2014" ~ 0.55,
          season == "Spring 2015" ~ 0.6,
          season == "Fall 2015" ~ 0.65,
          season == "Spring 2016" ~ 0.7,
          season == "Fall 2016" ~ 0.75,
          season == "Spring 2017" ~ 0.8,
          season == "Fall 2017" ~ 0.85,
          season == "Spring 2018" ~ 0.9,
          season == "Fall 2018" ~ 0.95,
          season == "Spring 2019" ~ 1.0,
          season == "Fall 2019" ~ 1.0,
          season == "Spring 2020" ~ 1.0
        )
      )
  }

  # This un-tidy data frame has 2 rows for every game played. For each game, 
  # there is one row with player 1 as the target and another row with player 2 
  # as the target. Results can then be grouped by player and allow for a fast,
  # vectorized rating update after joining to the current ratings. 
  collected_game_results <- 
    map_dfr(fargo_ratings %>% pull(player), collect_game_results)
  
  # Set hyperparameters and convergence limits  
  abs_diff <- 100000
  n_iter <- 0
  b <- (a - 1) / 500
  
  # Update ratings
  while (abs_diff > mae_stop) {
    n_iter <- n_iter + 1
    old_ratings <- fargo_ratings %>% pull(rating) # For convergence test

    fargo_ratings <- 
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
      # Use current ratings to calculate new ratings for each player
      mutate(
        A = 1 / (player_rating + opponent_rating),
        W = if_else(game_result == "W", 1, 0)
      ) %>% 
      group_by(player) %>% 
      summarize(rating = ((a - 1) + sum(W * decay)) / (b + sum(A * decay)))

    new_ratings <- fargo_ratings %>% pull(rating) # For convergence test
    
    print(str_c("Sum of absolute difference: ", sum(abs(old_ratings - new_ratings))))
    
    abs_diff <- sum(abs(old_ratings - new_ratings))
  }
  end_time <- Sys.time()
  time_taken <- end_time - start_time

  print(str_c("Number of iterations: ", n_iter))
  print(str_c("Time taken: ", round(time_taken, 2), " seconds"))
  
  fargo_ratings
}
```

``` r
fargo_df <- 
  results_to_ratings(results_df = results_no_forfeits, a = 3, mae_stop = 50) %>% 
  mutate(
    raw_rating = rating,
    rating = log(rating) * 144,
    rating = rating - mean(rating) + 500
  )
```

    ## [1] "Sum of absolute difference: 60727.6638567693"
    ## [1] "Sum of absolute difference: 24218.3954083644"
    ## [1] "Sum of absolute difference: 14555.5804508633"
    ## [1] "Sum of absolute difference: 10361.4898515032"
    ## [1] "Sum of absolute difference: 7934.68046940143"
    ## [1] "Sum of absolute difference: 6253.89108773027"
    ## [1] "Sum of absolute difference: 4990.14755556356"
    ## [1] "Sum of absolute difference: 4003.29151001657"
    ## [1] "Sum of absolute difference: 3216.04919887608"
    ## [1] "Sum of absolute difference: 2586.86418846875"
    ## [1] "Sum of absolute difference: 2082.5159516389"
    ## [1] "Sum of absolute difference: 1676.70811133989"
    ## [1] "Sum of absolute difference: 1350.17950052367"
    ## [1] "Sum of absolute difference: 1087.24828726042"
    ## [1] "Sum of absolute difference: 875.354866091785"
    ## [1] "Sum of absolute difference: 704.668380555015"
    ## [1] "Sum of absolute difference: 567.21823085787"
    ## [1] "Sum of absolute difference: 456.586205237712"
    ## [1] "Sum of absolute difference: 367.551211926015"
    ## [1] "Sum of absolute difference: 295.92120642044"
    ## [1] "Sum of absolute difference: 238.346443259844"
    ## [1] "Sum of absolute difference: 192.229854056864"
    ## [1] "Sum of absolute difference: 155.283516039974"
    ## [1] "Sum of absolute difference: 125.686635684515"
    ## [1] "Sum of absolute difference: 102.181344790666"
    ## [1] "Sum of absolute difference: 83.4471885597901"
    ## [1] "Sum of absolute difference: 68.67710079717"
    ## [1] "Sum of absolute difference: 57.1127751762216"
    ## [1] "Sum of absolute difference: 48.1442204365166"
    ## [1] "Number of iterations: 29"
    ## [1] "Time taken: 3.15 seconds"

``` r
fargo_df %>% 
  arrange(desc(rating))
```

    ## # A tibble: 572 x 3
    ##    player          rating raw_rating
    ##    <chr>            <dbl>      <dbl>
    ##  1 Hector Ortega     739.      2171.
    ##  2 Mike Maxwell      738.      2157.
    ##  3 Alvin Ho          712.      1807.
    ##  4 Nick Lansdown     694.      1597.
    ##  5 Diogo Martini     693.      1580.
    ##  6 Ryan Piaget       689.      1533.
    ##  7 Evan Burgess      688.      1531.
    ##  8 Rhys Hughes       686.      1510.
    ##  9 Thayer McDougle   686.      1503.
    ## 10 Bobby Yulo        684.      1490.
    ## # … with 562 more rows

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
