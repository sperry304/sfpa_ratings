library(shiny)
library(tidyverse)
library(rvest)
library(lubridate)

team_matches_and_ratings_sliced <- readRDS("team_matches_and_ratings_sliced2019-03-15.Rdata")

plot_teams <- function(teams_of_interest) {
  team_matches_and_ratings_sliced %>% 
    ggplot(aes(x = match_date, y = new_rating)) +
    geom_hline(yintercept = 1500, color = "white", size = 2) +
    geom_line(aes(group = team), color = "gray60", size = 1, alpha = 0.6) +
    geom_point(color = "gray60", size = 1.8, alpha = 0.6) +
    geom_line(aes(color = team, group = team), size = 1, data = . %>% filter(team %in% teams_of_interest)) +
    geom_point(aes(color = team, group = team), size = 2.2, data = . %>% filter(team %in% teams_of_interest)) +
    labs(
      x = "Match Date", y = "ELO Rating",
      title = "SFPA Team ELO Ratings, 2018-2019 Seasons"
    ) +
    theme(
      legend.title = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 20),
      axis.title.y = element_text(size = 16),
      axis.title.x = element_blank(),
      axis.text = element_text(size = 14),
      legend.position = "bottom",
      legend.text = element_text(size = 14)
    )
}

ui <- bootstrapPage(
  selectInput(
    inputId = "Teams", 
    label = "Choose team(s) to highlight:",
    choices = team_matches_and_ratings_sliced %>% select(team) %>% arrange(team) %>% distinct(),
    multiple = TRUE
  ),
  plotOutput(
    outputId = "plot",
    width = "90%",
    height = "600px"
  )
)
?selectInput

server <- function(input, output) {
  output$plot <- renderPlot({
    plot_teams(input$Teams)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

