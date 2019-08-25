SFPA Fargo Ratings v.3
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

    ## [1] "Mean absolute difference: 53832.4199437815"
    ## [1] "Mean absolute difference: 22009.2020102864"
    ## [1] "Mean absolute difference: 12974.6915098894"
    ## [1] "Mean absolute difference: 8922.15336857491"
    ## [1] "Mean absolute difference: 6504.27630089133"
    ## [1] "Mean absolute difference: 4871.13913166015"
    ## [1] "Mean absolute difference: 3689.26900210382"
    ## [1] "Mean absolute difference: 2808.56014681409"
    ## [1] "Mean absolute difference: 2145.36355306807"
    ## [1] "Mean absolute difference: 1642.39405053632"
    ## [1] "Mean absolute difference: 1259.72481039824"
    ## [1] "Mean absolute difference: 968.036758555094"
    ## [1] "Mean absolute difference: 745.685168338768"
    ## [1] "Mean absolute difference: 576.08451318375"
    ## [1] "Mean absolute difference: 447.147953099235"
    ## [1] "Mean absolute difference: 349.931533194592"
    ## [1] "Mean absolute difference: 276.887452662984"
    ## [1] "Mean absolute difference: 222.642538848413"
    ## [1] "Mean absolute difference: 182.423317886293"
    ## [1] "Mean absolute difference: 152.854636014847"
    ## [1] "Mean absolute difference: 132.147944546203"
    ## [1] "Mean absolute difference: 116.794984518664"
    ## [1] "Mean absolute difference: 104.443450376348"
    ## [1] "Mean absolute difference: 94.1293692392283"
    ## [1] "Mean absolute difference: 85.3887850451208"
    ## [1] "Mean absolute difference: 77.8920976229532"
    ## [1] "Mean absolute difference: 71.3985155855594"
    ## [1] "Mean absolute difference: 65.71981248882"
    ## [1] "Mean absolute difference: 60.7085025211224"
    ## [1] "Mean absolute difference: 56.2486718066797"
    ## [1] "Mean absolute difference: 52.248879356713"
    ## [1] "Mean absolute difference: 48.6366661529943"
    ## [1] "Number of iterations: 32"

``` r
fargo_df %>% 
  arrange(desc(rating))
```

    ## # A tibble: 493 x 3
    ##    player           rating raw_rating
    ##    <chr>             <dbl>      <dbl>
    ##  1 Hector Ortega      733.      2089.
    ##  2 Mike Maxwell       733.      2087.
    ##  3 Evan Burgess       701.      1666.
    ##  4 Skip Perry         701.      1665.
    ##  5 Ryan Piaget        699.      1645.
    ##  6 Michael Gonzales   691.      1557.
    ##  7 Bob Simon          691.      1552.
    ##  8 Nick Lansdown      689.      1534.
    ##  9 Diogo Martini      686.      1507.
    ## 10 Rhys Hughes        684.      1484.
    ## # … with 483 more rows

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
# Do this to update previous fargo ratings after adding new data
# or making other changes to player names etc.
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
