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
    ##  1 Hector Ortega     732.      2040.
    ##  2 Mike Maxwell      715.      1817.
    ##  3 Skip Perry        706.      1701.
    ##  4 Ryan Piaget       702.      1662.
    ##  5 Evan Burgess      684.      1459.
    ##  6 Thayer McDougle   683.      1451.
    ##  7 Bob Simon         681.      1437.
    ##  8 Stefano Lopez     672.      1350.
    ##  9 Rhys Hughes       672.      1349.
    ## 10 Jesse La Fear     668.      1313.
    ## # … with 321 more rows

``` r
saveRDS(fargo_df, str_c("other_data/fargo_", latest_results_date, ".Rdata"))
```
