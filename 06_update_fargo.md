SFPA Fargo Ratings v.3
================
Skip Perry
April 2019

``` r
fargo_df <- 
  results_to_ratings(results_no_forfeits, a = 3, mae_stop = 100) %>% 
  mutate(
    raw_rating = rating,
    rating = log(rating) * 144,
    rating = rating - mean(rating) + 500
  )
```

``` r
fargo_df %>% 
  arrange(desc(rating))
```

    ## # A tibble: 331 x 3
    ##    player          rating raw_rating
    ##    <chr>            <dbl>      <dbl>
    ##  1 Hector Ortega     731.      2034.
    ##  2 Skip Perry        714.      1804.
    ##  3 Mike Maxwell      712.      1771.
    ##  4 Ryan Piaget       704.      1683.
    ##  5 Evan Burgess      684.      1465.
    ##  6 Bob Simon         681.      1432.
    ##  7 Thayer McDougle   679.      1412.
    ##  8 Tom Seymour       675.      1378.
    ##  9 Stefano Lopez     672.      1345.
    ## 10 Diogo Martini     670.      1328.
    ## # … with 321 more rows

``` r
saveRDS(fargo_df, str_c("other_data/fargo_", latest_results_date, ".Rdata"))
```
