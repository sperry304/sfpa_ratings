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

    ## [1] "Mean absolute difference: 56592.2933241122"
    ## [1] "Mean absolute difference: 23426.3265798308"
    ## [1] "Mean absolute difference: 13870.1519056437"
    ## [1] "Mean absolute difference: 9504.13022514167"
    ## [1] "Mean absolute difference: 6936.78427080683"
    ## [1] "Mean absolute difference: 5177.17635614753"
    ## [1] "Mean absolute difference: 3908.94355713291"
    ## [1] "Mean absolute difference: 2970.8788519962"
    ## [1] "Mean absolute difference: 2267.08335354023"
    ## [1] "Mean absolute difference: 1733.4900663423"
    ## [1] "Mean absolute difference: 1328.43352391184"
    ## [1] "Mean absolute difference: 1019.81264299719"
    ## [1] "Mean absolute difference: 784.681246700143"
    ## [1] "Mean absolute difference: 605.564067098324"
    ## [1] "Mean absolute difference: 469.482313274931"
    ## [1] "Mean absolute difference: 366.38631847099"
    ## [1] "Mean absolute difference: 288.67795359848"
    ## [1] "Mean absolute difference: 231.113189182123"
    ## [1] "Mean absolute difference: 188.479421559642"
    ## [1] "Mean absolute difference: 157.458100567541"
    ## [1] "Mean absolute difference: 135.336194286698"
    ## [1] "Mean absolute difference: 119.223476584195"
    ## [1] "Mean absolute difference: 106.40961077595"
    ## [1] "Mean absolute difference: 95.8016721063359"
    ## [1] "Mean absolute difference: 86.8572223786176"
    ## [1] "Mean absolute difference: 79.214907610391"
    ## [1] "Mean absolute difference: 72.6187993190555"
    ## [1] "Mean absolute difference: 66.8691476792999"
    ## [1] "Mean absolute difference: 61.809717225204"
    ## [1] "Mean absolute difference: 57.3179744755294"
    ## [1] "Mean absolute difference: 53.29749444343"
    ## [1] "Mean absolute difference: 49.6720892149939"
    ## [1] "Number of iterations: 32"

``` r
fargo_df %>% 
  arrange(desc(rating))
```

    ## # A tibble: 519 x 3
    ##    player           rating raw_rating
    ##    <chr>             <dbl>      <dbl>
    ##  1 Mike Maxwell       737.      2134.
    ##  2 Hector Ortega      732.      2067.
    ##  3 Skip Perry         702.      1678.
    ##  4 Ryan Piaget        700.      1651.
    ##  5 Evan Burgess       698.      1626.
    ##  6 Bob Simon          694.      1588.
    ##  7 Michael Gonzales   693.      1580.
    ##  8 Nick Lansdown      691.      1554.
    ##  9 Alvin Ho           687.      1509.
    ## 10 Rhys Hughes        685.      1492.
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
