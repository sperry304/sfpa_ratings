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

    ## [1] "Mean absolute difference: 54341.0585482704"
    ## [1] "Mean absolute difference: 22788.8914025535"
    ## [1] "Mean absolute difference: 13590.1466662151"
    ## [1] "Mean absolute difference: 9392.56049133697"
    ## [1] "Mean absolute difference: 6849.31449321121"
    ## [1] "Mean absolute difference: 5106.87120453755"
    ## [1] "Mean absolute difference: 3848.27986443161"
    ## [1] "Mean absolute difference: 2919.79898073912"
    ## [1] "Mean absolute difference: 2224.97569490549"
    ## [1] "Mean absolute difference: 1699.15939613991"
    ## [1] "Mean absolute difference: 1300.5533396267"
    ## [1] "Mean absolute difference: 997.210041523414"
    ## [1] "Mean absolute difference: 766.327221609993"
    ## [1] "Mean absolute difference: 590.603922792519"
    ## [1] "Mean absolute difference: 457.242861004167"
    ## [1] "Mean absolute difference: 356.28644464349"
    ## [1] "Mean absolute difference: 280.255385579133"
    ## [1] "Mean absolute difference: 223.971242486969"
    ## [1] "Mean absolute difference: 182.362942633791"
    ## [1] "Mean absolute difference: 152.141130107392"
    ## [1] "Mean absolute difference: 130.610224027157"
    ## [1] "Mean absolute difference: 115.082032052346"
    ## [1] "Mean absolute difference: 102.794832586768"
    ## [1] "Mean absolute difference: 92.6404787436656"
    ## [1] "Mean absolute difference: 84.0762651100141"
    ## [1] "Mean absolute difference: 76.7513599315436"
    ## [1] "Mean absolute difference: 70.4220112143064"
    ## [1] "Mean absolute difference: 64.8981729790294"
    ## [1] "Mean absolute difference: 60.03134308918"
    ## [1] "Mean absolute difference: 55.7051527266681"
    ## [1] "Mean absolute difference: 51.8280940909447"
    ## [1] "Mean absolute difference: 48.327905321512"
    ## [1] "Number of iterations: 32"

``` r
fargo_df %>% 
  arrange(desc(rating))
```

    ## # A tibble: 498 x 3
    ##    player           rating raw_rating
    ##    <chr>             <dbl>      <dbl>
    ##  1 Mike Maxwell       737.      2133.
    ##  2 Hector Ortega      734.      2079.
    ##  3 Skip Perry         702.      1674.
    ##  4 Ryan Piaget        700.      1651.
    ##  5 Evan Burgess       698.      1627.
    ##  6 Bob Simon          695.      1587.
    ##  7 Michael Gonzales   693.      1574.
    ##  8 Nick Lansdown      691.      1551.
    ##  9 Alvin Ho           687.      1510.
    ## 10 Diogo Martini      686.      1494.
    ## # … with 488 more rows

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
