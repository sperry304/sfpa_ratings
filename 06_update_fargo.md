SFPA Fargo Ratings v.3
================
Skip Perry
May 2019

``` r
latest_results_date <- 
  list.files("match_data", pattern = "results_no_forfeits") %>% 
  str_extract("\\d+-\\d+-\\d+") %>% 
  max()

results_no_forfeits <- 
  str_c("match_data/results_no_forfeits_", latest_results_date, ".Rdata") %>% 
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
```

``` r
fargo_df <- 
  results_to_ratings(results_no_forfeits, a = 3, mae_stop = 50) %>% 
  mutate(
    raw_rating = rating,
    rating = log(rating) * 144,
    rating = rating - mean(rating) + 500
  )
```

    ## [1] "Mean absolute difference: 44704.668831123"
    ## [1] "Mean absolute difference: 19387.4234441037"
    ## [1] "Mean absolute difference: 11867.6208140662"
    ## [1] "Mean absolute difference: 8309.16723572448"
    ## [1] "Mean absolute difference: 6071.23627034187"
    ## [1] "Mean absolute difference: 4520.56129312091"
    ## [1] "Mean absolute difference: 3394.03790933308"
    ## [1] "Mean absolute difference: 2561.15211248802"
    ## [1] "Mean absolute difference: 1939.99296859378"
    ## [1] "Mean absolute difference: 1474.22837868949"
    ## [1] "Mean absolute difference: 1122.7170833915"
    ## [1] "Mean absolute difference: 856.042587491189"
    ## [1] "Mean absolute difference: 653.615256464574"
    ## [1] "Mean absolute difference: 499.470406434779"
    ## [1] "Mean absolute difference: 381.882761492874"
    ## [1] "Mean absolute difference: 292.074875289866"
    ## [1] "Mean absolute difference: 223.455069663227"
    ## [1] "Mean absolute difference: 171.032882107377"
    ## [1] "Mean absolute difference: 130.983727187919"
    ## [1] "Mean absolute difference: 100.359177421099"
    ## [1] "Mean absolute difference: 76.9369092243038"
    ## [1] "Mean absolute difference: 59.0434393480531"
    ## [1] "Mean absolute difference: 45.4004793233492"

``` r
fargo_df %>% 
  arrange(desc(rating))
```

    ## # A tibble: 423 x 3
    ##    player           rating raw_rating
    ##    <chr>             <dbl>      <dbl>
    ##  1 Hector Ortega      729.      2011.
    ##  2 Mike Maxwell       727.      1981.
    ##  3 Evan Burgess       698.      1628.
    ##  4 Skip Perry         697.      1608.
    ##  5 Ryan Piaget        695.      1585.
    ##  6 Bob Simon          689.      1530.
    ##  7 Michael Gonzales   684.      1470.
    ##  8 Nick Lansdown      683.      1462.
    ##  9 Rhys Hughes        681.      1446.
    ## 10 Nick Callado       681.      1438.
    ## # … with 413 more rows

``` r
saveRDS(fargo_df, str_c("fargo_ratings/fargo_", latest_results_date, ".Rdata"))
```

``` r
fargo_files <- 
  list.files("fargo_ratings", pattern = "fargo")

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

``` r
# Do this to update previous fargo ratings after adding new data
date_list <- 
  results_no_forfeits %>% 
  select(match_date) %>% 
  distinct() %>% 
  arrange(match_date) %>% 
  pull()

length(date_list)

for (i in 71:102) {
  results_date <- date_list[i]
  
  print(results_date)
  
  fargo_df <- 
    results_to_ratings(results_no_forfeits %>% filter(match_date <= results_date), a = 3, mae_stop = 50) %>% 
    mutate(
      raw_rating = rating,
      rating = log(rating) * 144,
      rating = rating - mean(rating) + 500
    )
  
  saveRDS(fargo_df, str_c("fargo_ratings/fargo_", results_date, ".Rdata"))
}
```
