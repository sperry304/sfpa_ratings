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
        season == "Spring 2016" ~ 0.7,
        season == "Fall 2016" ~ 0.75,
        season == "Spring 2017" ~ 0.8,
        season == "Fall 2017" ~ 0.85,
        season == "Spring 2018" ~ 0.9,
        season == "Fall 2018" ~ 0.95,
        season == "Spring 2019" ~ 1.0,
        season == "Fall 2019" ~ 1.0
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
  results_to_ratings(results_no_forfeits, a = 3, mae_stop = 50) %>% 
  mutate(
    raw_rating = rating,
    rating = log(rating) * 144,
    rating = rating - mean(rating) + 500
  )
```

    ## [1] "Mean absolute difference: 45197.4000394801"
    ## [1] "Mean absolute difference: 19426.1092389406"
    ## [1] "Mean absolute difference: 11893.0247651626"
    ## [1] "Mean absolute difference: 8335.09512908505"
    ## [1] "Mean absolute difference: 6089.48417273591"
    ## [1] "Mean absolute difference: 4532.46358851698"
    ## [1] "Mean absolute difference: 3402.09954946256"
    ## [1] "Mean absolute difference: 2566.77684961621"
    ## [1] "Mean absolute difference: 1944.03545145189"
    ## [1] "Mean absolute difference: 1477.17879830821"
    ## [1] "Mean absolute difference: 1124.87584476408"
    ## [1] "Mean absolute difference: 857.673750913234"
    ## [1] "Mean absolute difference: 654.852700623405"
    ## [1] "Mean absolute difference: 500.419429588733"
    ## [1] "Mean absolute difference: 382.618844804668"
    ## [1] "Mean absolute difference: 292.647498896952"
    ## [1] "Mean absolute difference: 223.903363817711"
    ## [1] "Mean absolute difference: 171.389709472026"
    ## [1] "Mean absolute difference: 131.268946253002"
    ## [1] "Mean absolute difference: 100.585385051902"
    ## [1] "Mean absolute difference: 77.1201185275588"
    ## [1] "Mean absolute difference: 59.200639360827"
    ## [1] "Mean absolute difference: 45.5414378493624"

``` r
fargo_df %>% 
  arrange(desc(rating))
```

    ## # A tibble: 425 x 3
    ##    player           rating raw_rating
    ##    <chr>             <dbl>      <dbl>
    ##  1 Hector Ortega      729.      2021.
    ##  2 Mike Maxwell       727.      1982.
    ##  3 Evan Burgess       698.      1628.
    ##  4 Skip Perry         697.      1609.
    ##  5 Ryan Piaget        694.      1585.
    ##  6 Bob Simon          689.      1531.
    ##  7 Michael Gonzales   684.      1472.
    ##  8 Nick Lansdown      683.      1464.
    ##  9 Rhys Hughes        681.      1446.
    ## 10 Nick Callado       680.      1438.
    ## # … with 415 more rows

``` r
saveRDS(fargo_df, str_c("other_data/fargo_", latest_results_date, ".Rdata"))
```
