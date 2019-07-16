SFPA Fargo Ratings v.3
================
Skip Perry
May 2019

``` r
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

get_updated_rating <- function(player_of_interest, results_df, ratings_df, a) {
  game_count <- 
    results_df %>% 
    filter(away == player_of_interest | home == player_of_interest) %>% 
    count() %>% 
    pull()
  
  if (game_count == 0) {
    return(ratings_df$rating[ratings_df$player == player_of_interest])
  }
  
  b <- (a - 1) / 500
  
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
      )
    ) %>% 
    inner_join(ratings_df %>% transmute(player, player_rating = rating), by = "player") %>% 
    inner_join(ratings_df %>% transmute(opponent = player, opponent_rating = rating), by = "opponent") %>% 
    mutate(
      A = 1 / (player_rating + opponent_rating),
      W = if_else(game_result == "W", 1, 0),
      decay = case_when(
        season == "Spring 2018" ~ 0.6,
        season == "Fall 2018" ~ 0.8,
        season == "Spring 2019" ~ 1.0
      )
    ) %>% 
    summarize(rating = ((a - 1) + sum(W * decay)) / (b + sum(A * decay))) %>% 
    pull(rating)
}

results_to_ratings <- function(results_df, a, mae_stop = 100) {
  player_list <- 
    bind_rows(
      results_df %>% transmute(player = home), 
      results_df %>% transmute(player = away)
    ) %>% 
    distinct() %>% 
    arrange(player)
  
  fargo_ratings <- 
    player_list %>% 
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

fargo_df <- 
  results_to_ratings(results_no_forfeits, a = 3, mae_stop = 100) %>% 
  mutate(
    raw_rating = rating,
    rating = log(rating) * 144,
    rating = rating - mean(rating) + 500
  )
```

    ## [1] "Mean absolute difference: 37223.8338722934"
    ## [1] "Mean absolute difference: 17312.641655577"
    ## [1] "Mean absolute difference: 10583.7151607892"
    ## [1] "Mean absolute difference: 7263.17897885356"
    ## [1] "Mean absolute difference: 5235.22044930138"
    ## [1] "Mean absolute difference: 3840.04644304956"
    ## [1] "Mean absolute difference: 2840.79297112155"
    ## [1] "Mean absolute difference: 2115.19877722696"
    ## [1] "Mean absolute difference: 1579.96372553696"
    ## [1] "Mean absolute difference: 1182.24989366509"
    ## [1] "Mean absolute difference: 887.139177079158"
    ## [1] "Mean absolute difference: 667.285403229547"
    ## [1] "Mean absolute difference: 502.263798416891"
    ## [1] "Mean absolute difference: 378.302671122789"
    ## [1] "Mean absolute difference: 285.155412489598"
    ## [1] "Mean absolute difference: 215.012745161634"
    ## [1] "Mean absolute difference: 162.175732073944"
    ## [1] "Mean absolute difference: 122.377125551261"
    ## [1] "Mean absolute difference: 92.3755844560383"

``` r
fargo_df %>% 
  arrange(desc(rating))
```

    ## # A tibble: 354 x 3
    ##    player          rating raw_rating
    ##    <chr>            <dbl>      <dbl>
    ##  1 Hector Ortega     758.      2446.
    ##  2 Mike Maxwell      726.      1960.
    ##  3 Evan Burgess      698.      1605.
    ##  4 Ryan Piaget       697.      1595.
    ##  5 Skip Perry        691.      1535.
    ##  6 Nick Lansdown     684.      1462.
    ##  7 Bob Simon         683.      1453.
    ##  8 Thayer McDougle   682.      1438.
    ##  9 Rhys Hughes       678.      1405.
    ## 10 Stefano Lopez     673.      1350.
    ## # … with 344 more rows

``` r
saveRDS(fargo_df, str_c("other_data/fargo_", latest_results_date, ".Rdata"))
```
