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

    ## [1] "Sum of absolute difference: 61065.3626540196"
    ## [1] "Sum of absolute difference: 24943.6554888669"
    ## [1] "Sum of absolute difference: 14789.128746525"
    ## [1] "Sum of absolute difference: 10295.7350777393"
    ## [1] "Sum of absolute difference: 7649.01053802111"
    ## [1] "Sum of absolute difference: 5816.97068809527"
    ## [1] "Sum of absolute difference: 4471.75165585166"
    ## [1] "Sum of absolute difference: 3453.34096983562"
    ## [1] "Sum of absolute difference: 2672.296297529"
    ## [1] "Sum of absolute difference: 2071.19005813308"
    ## [1] "Sum of absolute difference: 1607.80089359437"
    ## [1] "Sum of absolute difference: 1249.51167381019"
    ## [1] "Sum of absolute difference: 972.148608632895"
    ## [1] "Sum of absolute difference: 757.525851549458"
    ## [1] "Sum of absolute difference: 591.682898055535"
    ## [1] "Sum of absolute difference: 463.755260935666"
    ## [1] "Sum of absolute difference: 365.533130405559"
    ## [1] "Sum of absolute difference: 290.588009019401"
    ## [1] "Sum of absolute difference: 234.151408612309"
    ## [1] "Sum of absolute difference: 192.035516735281"
    ## [1] "Sum of absolute difference: 160.751422602693"
    ## [1] "Sum of absolute difference: 138.11932152256"
    ## [1] "Sum of absolute difference: 122.07745492685"
    ## [1] "Sum of absolute difference: 109.431278430287"
    ## [1] "Sum of absolute difference: 99.0266001657501"
    ## [1] "Sum of absolute difference: 90.2429895252969"
    ## [1] "Sum of absolute difference: 82.7301872345531"
    ## [1] "Sum of absolute difference: 76.2424452140188"
    ## [1] "Sum of absolute difference: 70.5861096090344"
    ## [1] "Sum of absolute difference: 65.6083881624703"
    ## [1] "Sum of absolute difference: 61.1885235150765"
    ## [1] "Sum of absolute difference: 57.230867746577"
    ## [1] "Sum of absolute difference: 53.6594548755697"
    ## [1] "Sum of absolute difference: 50.4137509331984"
    ## [1] "Sum of absolute difference: 47.4453281709196"
    ## [1] "Number of iterations: 35"

``` r
fargo_df %>% 
  arrange(desc(rating))
```

    ## # A tibble: 564 x 3
    ##    player          rating raw_rating
    ##    <chr>            <dbl>      <dbl>
    ##  1 Mike Maxwell      738.      2164.
    ##  2 Hector Ortega     736.      2125.
    ##  3 Alvin Ho          714.      1826.
    ##  4 Nick Lansdown     694.      1593.
    ##  5 Ryan Piaget       691.      1562.
    ##  6 Diogo Martini     689.      1541.
    ##  7 Evan Burgess      689.      1536.
    ##  8 Bob Simon         688.      1524.
    ##  9 Rhys Hughes       686.      1509.
    ## 10 Thayer McDougle   686.      1506.
    ## # … with 554 more rows

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
