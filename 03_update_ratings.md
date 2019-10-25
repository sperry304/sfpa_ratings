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
    print(str_c("Sum of absolute difference: ", sum(abs(old_ratings - new_ratings))))
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

    ## [1] "Sum of absolute difference: 59231.0779950405"
    ## [1] "Sum of absolute difference: 24378.5967184269"
    ## [1] "Sum of absolute difference: 14480.4453292367"
    ## [1] "Sum of absolute difference: 10010.557573734"
    ## [1] "Sum of absolute difference: 7392.20951962152"
    ## [1] "Sum of absolute difference: 5583.02531760048"
    ## [1] "Sum of absolute difference: 4259.82997089974"
    ## [1] "Sum of absolute difference: 3264.89938324692"
    ## [1] "Sum of absolute difference: 2507.99238501598"
    ## [1] "Sum of absolute difference: 1929.84964212291"
    ## [1] "Sum of absolute difference: 1487.35131692715"
    ## [1] "Sum of absolute difference: 1147.48238763726"
    ## [1] "Sum of absolute difference: 886.425422740423"
    ## [1] "Sum of absolute difference: 685.991939773065"
    ## [1] "Sum of absolute difference: 532.032830852261"
    ## [1] "Sum of absolute difference: 413.864294184174"
    ## [1] "Sum of absolute difference: 323.338858809695"
    ## [1] "Sum of absolute difference: 254.514585190282"
    ## [1] "Sum of absolute difference: 202.690235205801"
    ## [1] "Sum of absolute difference: 164.08499524031"
    ## [1] "Sum of absolute difference: 135.351751910521"
    ## [1] "Sum of absolute difference: 114.398102971199"
    ## [1] "Sum of absolute difference: 99.7660710159841"
    ## [1] "Sum of absolute difference: 88.6752919376944"
    ## [1] "Sum of absolute difference: 79.661872134021"
    ## [1] "Sum of absolute difference: 72.1258816422949"
    ## [1] "Sum of absolute difference: 65.7310423110535"
    ## [1] "Sum of absolute difference: 60.2485567219627"
    ## [1] "Sum of absolute difference: 55.5012923968854"
    ## [1] "Sum of absolute difference: 51.350375580377"
    ## [1] "Sum of absolute difference: 47.6867231375498"
    ## [1] "Number of iterations: 31"

``` r
fargo_df %>% 
  arrange(desc(rating))
```

    ## # A tibble: 548 x 3
    ##    player          rating raw_rating
    ##    <chr>            <dbl>      <dbl>
    ##  1 Mike Maxwell      737.      2140.
    ##  2 Hector Ortega     729.      2030.
    ##  3 Alvin Ho          702.      1681.
    ##  4 Evan Burgess      697.      1628.
    ##  5 Nick Lansdown     694.      1591.
    ##  6 Ryan Piaget       694.      1588.
    ##  7 Skip Perry        690.      1549.
    ##  8 Bob Simon         689.      1541.
    ##  9 Thayer McDougle   687.      1513.
    ## 10 Rhys Hughes       685.      1497.
    ## # … with 538 more rows

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
