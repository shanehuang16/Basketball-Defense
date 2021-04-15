library(shiny)
library(tidyverse)
stats <- read_csv('defense_table.csv')

# Define UI for app ----
ui <- fluidPage(
  titlePanel("Defensive Metrics"),
  
  h5('The metrics displayed are the opposing team\'s metrics when the specified player is on the floor.'),

  # Create a new Row in the UI for selectInputs
  fluidRow(
    column(4,
        selectInput("team",
                    "Team:",
                    c("All",
                      unique(as.character(stats$Team))))
    ),
    column(4,
        selectInput("player",
                    "Player:",
                    c("All",
                      unique(as.character(stats$Player))))
    )
  ),
  # Create a new row for the table.
  DT::dataTableOutput("table")
)



# Define server logic ----
server <- function(input, output) {

  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data <- stats
    if (input$team != "All") { # Select Team
      data <- data[data$Team == input$team,]
    }
    if (input$player != "All") { # Select Player
      data <- data[data$Player == input$player,]
    }
    data
  }) %>% 
    DT::formatRound(columns=c(4,6,8:10), digits=3)
  )

}

shinyApp(ui, server)
