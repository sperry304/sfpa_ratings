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

    ## # A tibble: 330 x 3
    ##    player          rating raw_rating
    ##    <chr>            <dbl>      <dbl>
    ##  1 Hector Ortega     731.      2022.
    ##  2 Skip Perry        716.      1832.
    ##  3 Mike Maxwell      710.      1757.
    ##  4 Ryan Piaget       702.      1662.
    ##  5 Evan Burgess      688.      1503.
    ##  6 Tom Seymour       675.      1371.
    ##  7 Bob Simon         673.      1359.
    ##  8 Thayer McDougle   673.      1352.
    ##  9 Stefano Lopez     671.      1338.
    ## 10 Diogo Martini     668.      1308.
    ## # … with 320 more rows

``` r
saveRDS(fargo_df, str_c("other_data/fargo_", latest_results_date, ".Rdata"))
```
