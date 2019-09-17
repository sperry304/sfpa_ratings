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

# Uses game database and current rating estimates to provide a new rating 
# estimate for a given player 
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

# Computes udpated ratings for all players until the absolute difference 
# in the vector of ratings between iterations reaches the desired threshold
# Usually takes about 20-30 iterations to converge
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

    ## [1] "Mean absolute difference: 56401.6148807023"
    ## [1] "Mean absolute difference: 23418.3911635623"
    ## [1] "Mean absolute difference: 13879.5241542663"
    ## [1] "Mean absolute difference: 9512.70251991048"
    ## [1] "Mean absolute difference: 6942.96096773621"
    ## [1] "Mean absolute difference: 5181.93314347946"
    ## [1] "Mean absolute difference: 3911.31016542647"
    ## [1] "Mean absolute difference: 2971.63875266562"
    ## [1] "Mean absolute difference: 2267.20256992"
    ## [1] "Mean absolute difference: 1733.32410781795"
    ## [1] "Mean absolute difference: 1328.07339185848"
    ## [1] "Mean absolute difference: 1019.32748264557"
    ## [1] "Mean absolute difference: 784.057249151063"
    ## [1] "Mean absolute difference: 604.801188719594"
    ## [1] "Mean absolute difference: 468.637904699936"
    ## [1] "Mean absolute difference: 365.35862518687"
    ## [1] "Mean absolute difference: 287.47566073749"
    ## [1] "Mean absolute difference: 229.699593853349"
    ## [1] "Mean absolute difference: 186.905391438451"
    ## [1] "Mean absolute difference: 155.729196623794"
    ## [1] "Mean absolute difference: 133.430998057037"
    ## [1] "Mean absolute difference: 117.327212810658"
    ## [1] "Mean absolute difference: 104.596630761566"
    ## [1] "Mean absolute difference: 94.0940039361816"
    ## [1] "Mean absolute difference: 85.2528020129178"
    ## [1] "Mean absolute difference: 77.7065976834079"
    ## [1] "Mean absolute difference: 71.2002031575717"
    ## [1] "Mean absolute difference: 65.5344757370784"
    ## [1] "Mean absolute difference: 60.5536861727948"
    ## [1] "Mean absolute difference: 56.1357301156789"
    ## [1] "Mean absolute difference: 52.1845521279014"
    ## [1] "Mean absolute difference: 48.6242871985396"
    ## [1] "Number of iterations: 32"

``` r
fargo_df %>% 
  arrange(desc(rating))
```

    ## # A tibble: 519 x 3
    ##    player           rating raw_rating
    ##    <chr>             <dbl>      <dbl>
    ##  1 Mike Maxwell       736.      2133.
    ##  2 Hector Ortega      732.      2066.
    ##  3 Skip Perry         702.      1677.
    ##  4 Ryan Piaget        699.      1650.
    ##  5 Evan Burgess       697.      1625.
    ##  6 Bob Simon          694.      1586.
    ##  7 Michael Gonzales   693.      1580.
    ##  8 Nick Lansdown      691.      1553.
    ##  9 Alvin Ho           687.      1509.
    ## 10 Rhys Hughes        685.      1491.
    ## # … with 509 more rows

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
