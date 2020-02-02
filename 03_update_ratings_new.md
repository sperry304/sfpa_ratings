SFPA Ratings v.1
================
Skip Perry
August 2019

``` r
# Computes udpated ratings for all players until the absolute difference 
# in the vector of ratings between iterations reaches the desired threshold
# Usually takes about 20-30 iterations to converge
results_to_ratings <- function(results_df, a, mae_stop = 100) {
  
  # Set up initial ratings at 500
  fargo_ratings <- 
    bind_rows(
      results_df %>% transmute(player = home), 
      results_df %>% transmute(player = away)
    ) %>% 
    distinct() %>% 
    arrange(player) %>% 
    mutate(rating = 500)
  
  # Turn all of the games into season-player-opponent-result-decay format to pass 
  # into the get_updated_rating function
  # # # This definition is inside the function because it uses results_df which  
  # # # is passed into results_to_ratings
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

  collected_game_results <- 
    map_dfr(fargo_ratings %>% pull(player), collect_game_results)
  
  abs_diff <- 100000
  n_iter <- 0
  b <- (a - 1) / 500
  
  while (abs_diff > mae_stop) {
    
    #start_time <- Sys.time() ### TIMING

    n_iter <- n_iter + 1
    old_ratings <- fargo_ratings %>% pull(rating) # For convergence test

    fargo_ratings <- 
      collected_game_results %>% 
      inner_join(
        fargo_ratings %>% transmute(player, player_rating = rating), 
        by = "player"
      ) %>% 
      inner_join(
        fargo_ratings %>% transmute(opponent = player, opponent_rating = rating), 
        by = "opponent"
      ) %>% 
      mutate(
        A = 1 / (player_rating + opponent_rating),
        W = if_else(game_result == "W", 1, 0)
      ) %>% 
      group_by(player) %>% 
      summarize(rating = ((a - 1) + sum(W * decay)) / (b + sum(A * decay)))

    new_ratings <- fargo_ratings %>% pull(rating) # For convergence test
    
    print(str_c("Sum of absolute difference: ", sum(abs(old_ratings - new_ratings))))
    
    #end_time <- Sys.time() ### TIMING
    #print(end_time - start_time) ### TIMING

    abs_diff <- sum(abs(old_ratings - new_ratings))
  }
  print(str_c("Number of iterations: ", n_iter))
  
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

    ## [1] "Sum of absolute difference: 60857.3837559568"
    ## [1] "Sum of absolute difference: 24261.1647944961"
    ## [1] "Sum of absolute difference: 14600.2816097301"
    ## [1] "Sum of absolute difference: 10386.7989935682"
    ## [1] "Sum of absolute difference: 7951.03502380144"
    ## [1] "Sum of absolute difference: 6262.28004767788"
    ## [1] "Sum of absolute difference: 4992.64873277543"
    ## [1] "Sum of absolute difference: 4001.72394213314"
    ## [1] "Sum of absolute difference: 3211.87928484926"
    ## [1] "Sum of absolute difference: 2581.16237237709"
    ## [1] "Sum of absolute difference: 2076.07207643211"
    ## [1] "Sum of absolute difference: 1670.01932990926"
    ## [1] "Sum of absolute difference: 1343.66366801252"
    ## [1] "Sum of absolute difference: 1081.05770569894"
    ## [1] "Sum of absolute difference: 869.613269942336"
    ## [1] "Sum of absolute difference: 699.422657955289"
    ## [1] "Sum of absolute difference: 562.476524778679"
    ## [1] "Sum of absolute difference: 452.326702751977"
    ## [1] "Sum of absolute difference: 363.773863751773"
    ## [1] "Sum of absolute difference: 292.575217608388"
    ## [1] "Sum of absolute difference: 235.350669737397"
    ## [1] "Sum of absolute difference: 189.39043271104"
    ## [1] "Sum of absolute difference: 152.58636209077"
    ## [1] "Sum of absolute difference: 123.13912370927"
    ## [1] "Sum of absolute difference: 99.5642943776248"
    ## [1] "Sum of absolute difference: 80.849584642122"
    ## [1] "Sum of absolute difference: 65.9428201809554"
    ## [1] "Sum of absolute difference: 54.1863343084452"
    ## [1] "Sum of absolute difference: 44.9856272578961"
    ## [1] "Number of iterations: 29"

``` r
fargo_df %>% 
  arrange(desc(rating))
```

    ## # A tibble: 572 x 3
    ##    player          rating raw_rating
    ##    <chr>            <dbl>      <dbl>
    ##  1 Hector Ortega     738.      2156.
    ##  2 Mike Maxwell      738.      2156.
    ##  3 Alvin Ho          719.      1891.
    ##  4 Diogo Martini     694.      1596.
    ##  5 Nick Lansdown     694.      1596.
    ##  6 Ryan Piaget       689.      1534.
    ##  7 Evan Burgess      688.      1531.
    ##  8 Rhys Hughes       686.      1509.
    ##  9 Thayer McDougle   686.      1502.
    ## 10 Bobby Yulo        684.      1488.
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
