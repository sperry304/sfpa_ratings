SFPA Player Ratings, v.1
================
Skip Perry
March 2019

``` r
plot_team_results <- function(team_name) {
  p <- 
    fargo_df %>% 
    filter(team == team_name) %>% 
    ggplot(aes(x = match_date, y = rating, group = player, color = player)) +
    geom_hline(yintercept = 500, color = "white", size = 2) +
    geom_line() +
    geom_point(size = 0.8) +
    coord_cartesian(ylim = c(0, 2000)) +
    labs(
      x = "Match Date", y = "Rating",
      title = str_c(team_name, " Player Ratings")
    ) +
    theme(
      legend.title = element_blank(),
      plot.title = element_text(hjust = 0.5),
      legend.position = "bottom",
      legend.text = element_text(size = 10)
    )
  print(p)
}

walk(team_list, plot_team_results)
```

![](08_fargo_all_teams_files/figure-markdown_github/unnamed-chunk-1-1.png)![](08_fargo_all_teams_files/figure-markdown_github/unnamed-chunk-1-2.png)![](08_fargo_all_teams_files/figure-markdown_github/unnamed-chunk-1-3.png)![](08_fargo_all_teams_files/figure-markdown_github/unnamed-chunk-1-4.png)![](08_fargo_all_teams_files/figure-markdown_github/unnamed-chunk-1-5.png)![](08_fargo_all_teams_files/figure-markdown_github/unnamed-chunk-1-6.png)![](08_fargo_all_teams_files/figure-markdown_github/unnamed-chunk-1-7.png)![](08_fargo_all_teams_files/figure-markdown_github/unnamed-chunk-1-8.png)![](08_fargo_all_teams_files/figure-markdown_github/unnamed-chunk-1-9.png)![](08_fargo_all_teams_files/figure-markdown_github/unnamed-chunk-1-10.png)![](08_fargo_all_teams_files/figure-markdown_github/unnamed-chunk-1-11.png)![](08_fargo_all_teams_files/figure-markdown_github/unnamed-chunk-1-12.png)![](08_fargo_all_teams_files/figure-markdown_github/unnamed-chunk-1-13.png)![](08_fargo_all_teams_files/figure-markdown_github/unnamed-chunk-1-14.png)![](08_fargo_all_teams_files/figure-markdown_github/unnamed-chunk-1-15.png)![](08_fargo_all_teams_files/figure-markdown_github/unnamed-chunk-1-16.png)![](08_fargo_all_teams_files/figure-markdown_github/unnamed-chunk-1-17.png)![](08_fargo_all_teams_files/figure-markdown_github/unnamed-chunk-1-18.png)![](08_fargo_all_teams_files/figure-markdown_github/unnamed-chunk-1-19.png)![](08_fargo_all_teams_files/figure-markdown_github/unnamed-chunk-1-20.png)![](08_fargo_all_teams_files/figure-markdown_github/unnamed-chunk-1-21.png)![](08_fargo_all_teams_files/figure-markdown_github/unnamed-chunk-1-22.png)![](08_fargo_all_teams_files/figure-markdown_github/unnamed-chunk-1-23.png)![](08_fargo_all_teams_files/figure-markdown_github/unnamed-chunk-1-24.png)![](08_fargo_all_teams_files/figure-markdown_github/unnamed-chunk-1-25.png)![](08_fargo_all_teams_files/figure-markdown_github/unnamed-chunk-1-26.png)![](08_fargo_all_teams_files/figure-markdown_github/unnamed-chunk-1-27.png)![](08_fargo_all_teams_files/figure-markdown_github/unnamed-chunk-1-28.png)![](08_fargo_all_teams_files/figure-markdown_github/unnamed-chunk-1-29.png)![](08_fargo_all_teams_files/figure-markdown_github/unnamed-chunk-1-30.png)![](08_fargo_all_teams_files/figure-markdown_github/unnamed-chunk-1-31.png)![](08_fargo_all_teams_files/figure-markdown_github/unnamed-chunk-1-32.png)![](08_fargo_all_teams_files/figure-markdown_github/unnamed-chunk-1-33.png)![](08_fargo_all_teams_files/figure-markdown_github/unnamed-chunk-1-34.png)
