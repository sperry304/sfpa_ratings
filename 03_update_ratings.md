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

    ## [1] "Sum of absolute difference: 59047.3625207322"
    ## [1] "Sum of absolute difference: 24224.0515356126"
    ## [1] "Sum of absolute difference: 14346.8964128285"
    ## [1] "Sum of absolute difference: 9923.47946708878"
    ## [1] "Sum of absolute difference: 7322.44975489371"
    ## [1] "Sum of absolute difference: 5522.29006712813"
    ## [1] "Sum of absolute difference: 4209.14972398552"
    ## [1] "Sum of absolute difference: 3223.84272890722"
    ## [1] "Sum of absolute difference: 2475.15029898444"
    ## [1] "Sum of absolute difference: 1903.39588770927"
    ## [1] "Sum of absolute difference: 1466.17446945091"
    ## [1] "Sum of absolute difference: 1130.90314232796"
    ## [1] "Sum of absolute difference: 873.467619634451"
    ## [1] "Sum of absolute difference: 675.959423778552"
    ## [1] "Sum of absolute difference: 524.577596667596"
    ## [1] "Sum of absolute difference: 408.619646509675"
    ## [1] "Sum of absolute difference: 320.236661002794"
    ## [1] "Sum of absolute difference: 253.284789482589"
    ## [1] "Sum of absolute difference: 203.349929133249"
    ## [1] "Sum of absolute difference: 166.15751107302"
    ## [1] "Sum of absolute difference: 138.729500963613"
    ## [1] "Sum of absolute difference: 119.17801855269"
    ## [1] "Sum of absolute difference: 105.2041371887"
    ## [1] "Sum of absolute difference: 94.0517986822254"
    ## [1] "Sum of absolute difference: 84.7971240507855"
    ## [1] "Sum of absolute difference: 77.0010431510005"
    ## [1] "Sum of absolute difference: 70.348912206778"
    ## [1] "Sum of absolute difference: 64.6161273723336"
    ## [1] "Sum of absolute difference: 59.6267536428354"
    ## [1] "Sum of absolute difference: 55.2427511740329"
    ## [1] "Sum of absolute difference: 51.3555795097545"
    ## [1] "Sum of absolute difference: 47.8796627960676"
    ## [1] "Number of iterations: 32"

``` r
fargo_df %>% 
  arrange(desc(rating))
```

    ## # A tibble: 547 x 3
    ##    player          rating raw_rating
    ##    <chr>            <dbl>      <dbl>
    ##  1 Mike Maxwell      737.      2139.
    ##  2 Hector Ortega     730.      2038.
    ##  3 Alvin Ho          703.      1692.
    ##  4 Evan Burgess      698.      1635.
    ##  5 Skip Perry        694.      1591.
    ##  6 Ryan Piaget       693.      1585.
    ##  7 Nick Lansdown     693.      1578.
    ##  8 Bob Simon         691.      1557.
    ##  9 Thayer McDougle   685.      1500.
    ## 10 Rhys Hughes       685.      1496.
    ## # … with 537 more rows

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
