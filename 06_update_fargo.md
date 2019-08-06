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
        season == "Spring 2018" ~ 0.7,
        season == "Fall 2018" ~ 0.8,
        season == "Spring 2019" ~ 0.9,
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

    ## [1] "Mean absolute difference: 39587.7145415439"
    ## [1] "Mean absolute difference: 17895.5431988675"
    ## [1] "Mean absolute difference: 10904.2325619171"
    ## [1] "Mean absolute difference: 7486.10772638972"
    ## [1] "Mean absolute difference: 5363.44669236056"
    ## [1] "Mean absolute difference: 3915.251162658"
    ## [1] "Mean absolute difference: 2885.90987410991"
    ## [1] "Mean absolute difference: 2140.86094887684"
    ## [1] "Mean absolute difference: 1593.84669455046"
    ## [1] "Mean absolute difference: 1189.69600101319"
    ## [1] "Mean absolute difference: 890.204133627076"
    ## [1] "Mean absolute difference: 667.561841479325"
    ## [1] "Mean absolute difference: 501.472123414206"
    ## [1] "Mean absolute difference: 377.196125859461"
    ## [1] "Mean absolute difference: 284.101136234972"
    ## [1] "Mean absolute difference: 214.531185272012"
    ## [1] "Mean absolute difference: 162.541745881818"
    ## [1] "Mean absolute difference: 123.566045181847"
    ## [1] "Mean absolute difference: 94.5096427957949"
    ## [1] "Mean absolute difference: 73.1049358021878"
    ## [1] "Mean absolute difference: 57.4520655472372"
    ## [1] "Mean absolute difference: 46.0026631091736"

``` r
fargo_df %>% 
  arrange(desc(rating))
```

    ## # A tibble: 378 x 3
    ##    player          rating raw_rating
    ##    <chr>            <dbl>      <dbl>
    ##  1 Hector Ortega     757.      2444.
    ##  2 Mike Maxwell      725.      1951.
    ##  3 Evan Burgess      698.      1619.
    ##  4 Skip Perry        693.      1562.
    ##  5 Ryan Piaget       692.      1558.
    ##  6 Bob Simon         688.      1508.
    ##  7 Nick Lansdown     683.      1462.
    ##  8 Thayer McDougle   679.      1420.
    ##  9 Rhys Hughes       679.      1417.
    ## 10 Jesse La Fear     673.      1362.
    ## # … with 368 more rows

``` r
saveRDS(fargo_df, str_c("other_data/fargo_", latest_results_date, ".Rdata"))
```
