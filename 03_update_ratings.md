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
        season == "Fall 2019" ~ 1.0,
        season == "Spring 2020" ~ 1.0
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

    ## [1] "Sum of absolute difference: 59047.7792557733"
    ## [1] "Sum of absolute difference: 24464.4601037863"
    ## [1] "Sum of absolute difference: 14538.6428777318"
    ## [1] "Sum of absolute difference: 10134.5444114516"
    ## [1] "Sum of absolute difference: 7537.63971608931"
    ## [1] "Sum of absolute difference: 5728.56941457221"
    ## [1] "Sum of absolute difference: 4398.75312130063"
    ## [1] "Sum of absolute difference: 3392.48592153888"
    ## [1] "Sum of absolute difference: 2621.58736043888"
    ## [1] "Sum of absolute difference: 2028.93258214317"
    ## [1] "Sum of absolute difference: 1572.81317833618"
    ## [1] "Sum of absolute difference: 1220.68712690932"
    ## [1] "Sum of absolute difference: 948.690270300301"
    ## [1] "Sum of absolute difference: 738.75254366312"
    ## [1] "Sum of absolute difference: 577.05703290727"
    ## [1] "Sum of absolute difference: 452.546631045164"
    ## [1] "Sum of absolute difference: 357.536177825299"
    ## [1] "Sum of absolute difference: 285.458294854662"
    ## [1] "Sum of absolute difference: 231.517263405322"
    ## [1] "Sum of absolute difference: 191.353560107839"
    ## [1] "Sum of absolute difference: 161.832597562143"
    ## [1] "Sum of absolute difference: 140.952726586668"
    ## [1] "Sum of absolute difference: 125.601939044417"
    ## [1] "Sum of absolute difference: 113.173521539216"
    ## [1] "Sum of absolute difference: 102.77293521921"
    ## [1] "Sum of absolute difference: 93.9347357429465"
    ## [1] "Sum of absolute difference: 86.3436276440195"
    ## [1] "Sum of absolute difference: 79.759793931697"
    ## [1] "Sum of absolute difference: 73.9944483121878"
    ## [1] "Sum of absolute difference: 68.8987551058153"
    ## [1] "Sum of absolute difference: 64.3551434556906"
    ## [1] "Sum of absolute difference: 60.2705078241433"
    ## [1] "Sum of absolute difference: 56.5708905345575"
    ## [1] "Sum of absolute difference: 53.1973263856134"
    ## [1] "Sum of absolute difference: 50.1025970513372"
    ## [1] "Sum of absolute difference: 47.2486969226562"
    ## [1] "Number of iterations: 36"

``` r
fargo_df %>% 
  arrange(desc(rating))
```

    ## # A tibble: 549 x 3
    ##    player          rating raw_rating
    ##    <chr>            <dbl>      <dbl>
    ##  1 Mike Maxwell      737.      2138.
    ##  2 Hector Ortega     732.      2065.
    ##  3 Alvin Ho          719.      1882.
    ##  4 Nick Lansdown     697.      1615.
    ##  5 Diogo Martini     688.      1524.
    ##  6 Evan Burgess      688.      1521.
    ##  7 Ryan Piaget       688.      1521.
    ##  8 Thayer McDougle   687.      1513.
    ##  9 Rhys Hughes       686.      1504.
    ## 10 Bob Simon         686.      1496.
    ## # … with 539 more rows

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
