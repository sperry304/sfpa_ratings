SFPA Ratings v.1
================
Skip Perry
August 2019

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
  n_iter <- 0
  while (abs_diff > mae_stop) {
    n_iter <- n_iter + 1
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
  print(str_c("Number of iterations: ", n_iter))
  
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

    ## [1] "Mean absolute difference: 54616.8535648443"
    ## [1] "Mean absolute difference: 22647.0497643958"
    ## [1] "Mean absolute difference: 13409.8591531681"
    ## [1] "Mean absolute difference: 9217.66049131892"
    ## [1] "Mean absolute difference: 6704.89499325998"
    ## [1] "Mean absolute difference: 5004.41038353445"
    ## [1] "Mean absolute difference: 3781.68047195564"
    ## [1] "Mean absolute difference: 2875.15350921884"
    ## [1] "Mean absolute difference: 2194.35765614295"
    ## [1] "Mean absolute difference: 1678.16562044306"
    ## [1] "Mean absolute difference: 1286.09032415291"
    ## [1] "Mean absolute difference: 987.061696481261"
    ## [1] "Mean absolute difference: 758.914049124838"
    ## [1] "Mean absolute difference: 585.078406216763"
    ## [1] "Mean absolute difference: 452.670944672919"
    ## [1] "Mean absolute difference: 352.457048126266"
    ## [1] "Mean absolute difference: 277.040597131791"
    ## [1] "Mean absolute difference: 220.87845974196"
    ## [1] "Mean absolute difference: 179.302418660182"
    ## [1] "Mean absolute difference: 148.771952232115"
    ## [1] "Mean absolute difference: 126.804018736124"
    ## [1] "Mean absolute difference: 111.208961306008"
    ## [1] "Mean absolute difference: 99.0183401520261"
    ## [1] "Mean absolute difference: 89.0049300861722"
    ## [1] "Mean absolute difference: 80.5934457991019"
    ## [1] "Mean absolute difference: 73.4197070226071"
    ## [1] "Mean absolute difference: 67.2386727662251"
    ## [1] "Mean absolute difference: 61.8593950235237"
    ## [1] "Mean absolute difference: 57.1327611222419"
    ## [1] "Mean absolute difference: 52.9420098377601"
    ## [1] "Mean absolute difference: 49.195403103857"
    ## [1] "Number of iterations: 31"

``` r
fargo_df %>% 
  arrange(desc(rating))
```

    ## # A tibble: 497 x 3
    ##    player           rating raw_rating
    ##    <chr>             <dbl>      <dbl>
    ##  1 Mike Maxwell       737.      2128.
    ##  2 Hector Ortega      736.      2111.
    ##  3 Skip Perry         702.      1669.
    ##  4 Ryan Piaget        700.      1650.
    ##  5 Evan Burgess       699.      1635.
    ##  6 Bob Simon          693.      1575.
    ##  7 Michael Gonzales   693.      1574.
    ##  8 Nick Lansdown      691.      1546.
    ##  9 Rhys Hughes        686.      1493.
    ## 10 Diogo Martini      685.      1485.
    ## # … with 487 more rows

``` r
saveRDS(fargo_df, str_c("fargo_ratings/fargo_", latest_results_date, ".Rdata"))
```

``` r
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

``` r
# Do this to update previous fargo ratings after adding new games to database
# from before current week, making changes to player names, etc.
date_list <- 
  results_no_forfeits %>% 
  select(match_date) %>% 
  distinct() %>% 
  arrange(match_date) %>% 
  pull()

num_dates <- 63
# Last time ran back to 2018-08-07

#length(date_list)
tail(date_list, num_dates)

#for (i in 71:102) {
for (i in 1:num_dates) {
  results_date <- tail(date_list, num_dates)[i]
  
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
