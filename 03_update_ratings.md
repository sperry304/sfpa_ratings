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

    ## [1] "Mean absolute difference: 57817.1309279825"
    ## [1] "Mean absolute difference: 23888.3980837912"
    ## [1] "Mean absolute difference: 13984.0655296006"
    ## [1] "Mean absolute difference: 9559.76073673504"
    ## [1] "Mean absolute difference: 6975.30933790966"
    ## [1] "Mean absolute difference: 5224.37469101459"
    ## [1] "Mean absolute difference: 3958.10664776598"
    ## [1] "Mean absolute difference: 3015.47968005183"
    ## [1] "Mean absolute difference: 2304.76412090208"
    ## [1] "Mean absolute difference: 1765.42267900604"
    ## [1] "Mean absolute difference: 1355.6741394962"
    ## [1] "Mean absolute difference: 1042.87544973843"
    ## [1] "Mean absolute difference: 804.537519910572"
    ## [1] "Mean absolute difference: 622.723405475913"
    ## [1] "Mean absolute difference: 484.342138974684"
    ## [1] "Mean absolute difference: 379.554946227713"
    ## [1] "Mean absolute difference: 300.785626282908"
    ## [1] "Mean absolute difference: 242.251479599025"
    ## [1] "Mean absolute difference: 198.782094919857"
    ## [1] "Mean absolute difference: 167.095121234502"
    ## [1] "Mean absolute difference: 144.664341178115"
    ## [1] "Mean absolute difference: 128.048147654503"
    ## [1] "Mean absolute difference: 114.637363064974"
    ## [1] "Mean absolute difference: 103.441306084544"
    ## [1] "Mean absolute difference: 93.9576116030496"
    ## [1] "Mean absolute difference: 85.832652285859"
    ## [1] "Mean absolute difference: 78.802555829265"
    ## [1] "Mean absolute difference: 72.6607321772765"
    ## [1] "Mean absolute difference: 67.245120188939"
    ## [1] "Mean absolute difference: 62.4282629749484"
    ## [1] "Mean absolute difference: 58.1095963881735"
    ## [1] "Mean absolute difference: 54.209463857952"
    ## [1] "Mean absolute difference: 50.6644747841081"
    ## [1] "Mean absolute difference: 47.4239067872548"
    ## [1] "Number of iterations: 34"

``` r
fargo_df %>% 
  arrange(desc(rating))
```

    ## # A tibble: 527 x 3
    ##    player           rating raw_rating
    ##    <chr>             <dbl>      <dbl>
    ##  1 Mike Maxwell       737.      2134.
    ##  2 Hector Ortega      730.      2033.
    ##  3 Skip Perry         707.      1735.
    ##  4 Evan Burgess       699.      1641.
    ##  5 Ryan Piaget        696.      1608.
    ##  6 Bob Simon          694.      1583.
    ##  7 Michael Gonzales   694.      1581.
    ##  8 Nick Lansdown      691.      1550.
    ##  9 Alvin Ho           688.      1526.
    ## 10 Rhys Hughes        685.      1492.
    ## # … with 517 more rows

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
