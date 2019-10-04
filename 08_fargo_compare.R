library(tidyverse)
library(readODS)

df <- read_ods("~/Documents/sfpa_ratings/fargo_ratings/sfpa_mikepage_ratings.ods")

p <- c(0.1, 0.3, 0.5, 0.7, 0.9)
p_names <- map_chr(p, ~paste0(.x*100, "%"))
p_funs <- 
  map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
  set_names(nm = p_names)

quantiles_raw <-
  df %>% 
  group_by(date) %>%
  summarize_at(vars(fargo_rating), p_funs) 

quantiles_tidy <- 
  quantiles_raw %>% 
  gather(key = pctile, value = fargo_rating, -date)

df %>% 
  group_by(date) %>% 
  summarize(mean = mean(fargo_rating)) %>% 
  left_join(quantiles_raw, by = "date")

df %>% 
  ggplot(aes(x = date, y = fargo_rating, group = player)) +
  geom_line(alpha = 0.15) +
  geom_line(data = quantiles_tidy, aes(group = pctile, color = pctile), size = 0.9) +
  geom_point(data = quantiles_tidy, aes(group = pctile, color = pctile)) +
  coord_cartesian(ylim = c(300, 600))

df %>% 
  spread(date, fargo_rating) %>% 
  arrange(desc(`2019-10-01`)) %>% 
  head(20)

df %>% 
  group_by(player) %>% 
  summarize(sdev = sd(fargo_rating)) %>% 
  arrange(desc(sdev))