SFPA Ratings - Data Scraping
================
Skip Perry
March 2019

``` r
# Produces a list of URLs of score sheets for a given completed week
# Starts with week_url of form https://www.sfpapool.org/stats/week/52/

get_week_match_urls <- function(week_url) {
  webpage <- read_html(week_url)

  webpage %>% 
  html_nodes("tr") %>% 
  html_nodes("td") %>% 
  html_node("a") %>% 
  html_attr("href") %>% 
  enframe(name = NULL) %>% 
  filter(str_detect(value, "score_sheet")) %>% 
  transmute(score_sheets = str_c("https://www.sfpapool.org", value)) %>% 
  pull(score_sheets)
}

#get_week_match_urls("https://www.sfpapool.org/stats/week/58/")

# Helper function for date
date_to_season <- function(match_date) {
  str_c(
    ifelse(match_date %>% month < 7, "Spring", "Fall"),
    match_date %>% year, 
    sep = " "
  )
}

# Parses a given score sheet and turns it into a data frame
# Score sheet has form https://www.sfpapool.org/stats/score_sheet/1223/

parse_score_sheet <- function(url) {
  #url <- 'https://www.sfpapool.org/stats/score_sheet/1042/'

  webpage <- read_html(url)
  
  match_date <-
    webpage %>% 
    html_nodes("h5") %>% 
    html_text() %>% 
    enframe(name = NULL) %>% 
    slice(2) %>% 
    pull() %>% 
    str_replace("Sept.", "Sep.") %>% 
    str_extract("\\w+.\\s\\d+,\\s\\d+") %>% 
    mdy()

  season <- date_to_season(match_date)
  
  week_number <- 
    webpage %>% 
    html_nodes("h5") %>% 
    html_text %>% 
    enframe(name = NULL) %>% 
    filter(str_detect(value, pattern = "Week")) %>% 
    as.character() %>% 
    str_extract(pattern = "\\d+") %>% 
    as.numeric()
  
  # Playoff games are labeled 50
  week_number <- ifelse(week_number == 0, 50, week_number)
  
  away_team <- 
    webpage %>% 
    html_nodes("h4") %>% 
    html_text() %>% 
    enframe(name = NULL) %>% 
    filter(str_detect(value, "Summaries")) %>% 
    slice(1) %>% 
    as.character() %>% 
    str_remove("\\sSummaries")
  
  home_team <- 
    webpage %>% 
    html_nodes("h4") %>% 
    html_text() %>% 
    enframe(name = NULL) %>% 
    filter(str_detect(value, "Summaries")) %>% 
    slice(2) %>% 
    as.character() %>% 
    str_remove("\\sSummaries")
  
  game_results <- 
    webpage %>% 
    html_nodes("tr") %>% 
    html_nodes("td") %>% 
    html_nodes("div") %>% 
    html_nodes("input") %>% 
    html_attr("checked") %>% 
    enframe(name = NULL) %>% 
    mutate(
      team = rep(c("home", "away"), nrow(.) / 2),
      game_num = rep(1:(nrow(.) / 2), each = 2)
    ) %>% 
    spread(key = team, value = value) %>% 
    transmute(
      game_winner = case_when(
        !is.na(home) ~ "home",
        !is.na(away) ~ "away"
      )
    )
  
  player_matchups <-
    webpage %>% 
    html_nodes("div") %>% 
    html_nodes("div") %>% 
    html_nodes("div") %>% 
    html_text() %>% 
    enframe(name = NULL) %>% 
    filter(str_detect(value, "\\w+\\s\\w+|\\w+|--")) %>% 
    filter(!str_detect(value, "^\n")) %>% 
    filter(!str_detect(value, "^\\d+")) %>% 
    filter(value != "A") %>% 
    filter(value != "B") %>% 
    filter(value != "C") %>% 
    filter(value != "D") %>% 
    filter(value != "Home TB") %>% 
    filter(value != "Away TB") %>% 
    mutate(
      value = if_else(value == "--", "Forfeited Game", value),
      team = rep(c("home", "away"), nrow(.) / 2),
      game_num = rep(1:(nrow(.) / 2), each = 2)
    ) %>% 
    spread(key = "team", value = "value") %>% 
    bind_cols(game_results) %>% 
    select(game_num, home, away, game_winner)
  
  forfeited_games <- 
    webpage %>% 
    html_nodes("tr") %>% 
    html_nodes("td") %>% 
    toString() %>% 
    str_extract_all(pattern = 'form-\\d+-forfeit\\"\\schecked') %>% 
    unlist() %>% 
    enframe(name = NULL) %>% 
    transmute(
      game_num = as.numeric(str_extract(value, pattern = "\\d+")) + 1,
      forfeit = "forfeit"
    )
  
  match_summary <- 
    player_matchups %>% 
    mutate(
      season = season,
      match_date = match_date,
      week_number = week_number,
      home_team = home_team,
      away_team = away_team
    ) %>% 
    select(season, match_date, week_number, home_team, away_team, game_num:game_winner) %>% 
    left_join(forfeited_games, by = "game_num")
  
  match_summary
}

#parse_score_sheet("https://www.sfpapool.org/stats/score_sheet/1343/")

# Function to turn a list of week URLs into a data frame of match results for a whole season
# Cycles through each week URL, grabs the matches from that week, adds to data frame
week_urls_to_all_matches <- function(week_urls) {
  all_matches <- tibble() 

  for (i in 1:length(week_urls)) {
    print(str_c("Starting week ", i))
    match_urls <- get_week_match_urls(week_urls[i])
    week_match_data <- map_dfr(match_urls, parse_score_sheet)
    all_matches <- bind_rows(all_matches, week_match_data)
    print(str_c("Week ", i, " complete"))
  }
  
  all_matches
}

#week_urls_to_all_matches(str_c("https://www.sfpapool.org/stats/week/", c(57:58)))
```

``` r
# Summarize previous file
prev_matches <- readRDS("match_data/all_matches_2019springplayoffs.Rdata")

prev_matches %>% 
  group_by(match_date) %>% 
  count()
```

    ## # A tibble: 2 x 2
    ## # Groups:   match_date [2]
    ##   match_date     n
    ##   <date>     <int>
    ## 1 2019-05-14   170
    ## 2 2019-05-16   136

``` r
# Get updated Spring 2019 results and check
new_data_urls <- str_c("https://www.sfpapool.org/stats/week/", 72) # Most recent week
new_matches <- week_urls_to_all_matches(new_data_urls)
```

    ## [1] "Starting week 1"
    ## [1] "Week 1 complete"

``` r
bind_rows(prev_matches, new_matches) %>% 
  group_by(match_date) %>% 
  count()
```

    ## # A tibble: 2 x 2
    ## # Groups:   match_date [2]
    ##   match_date     n
    ##   <date>     <int>
    ## 1 2019-05-14   170
    ## 2 2019-05-16   272

``` r
# Add to previous results
all_matches_2019springplayoffs <- bind_rows(prev_matches, new_matches)
  
saveRDS(all_matches_2019springplayoffs, "match_data/all_matches_2019springplayoffs.Rdata")
```

Scrape an outside file from a tournament:

Spring 2018 data generation:

Fall 2018 data generation:
