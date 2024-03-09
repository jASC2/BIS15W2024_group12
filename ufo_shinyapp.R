library(shiny)
library(shinythemes)
library(shinydashboard)

library("tidyverse")
library("naniar")
library("janitor")
library("ggmap")
library("dplyr")

ufo <- readr::read_csv("ufo_data/complete.csv", na = c("", "unkown")) %>% clean_names

ufo2 <- ufo %>%
  separate(datetime, into = c("month", "day","year"), sep="/") %>%
  separate(year, into = c("year", "time"), sep = " ") %>%
  select(-duration_hours_min, -comments, -date_posted) %>%
  filter(country=="us", latitude!=0, longitude!=0,  !is.na(latitude), !is.na(longitude)) %>%
  mutate(year=as.factor(year)) %>%
  mutate(latitude=as.numeric(latitude)) %>%
  mutate(longitude=as.numeric(longitude)) %>%
  mutate(shape=as.factor(shape)) %>%
  mutate(state=as.factor(state))

ui <- dashboardPage(
  dashboardHeader(title = "UFO Tracker"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    
    # Main content
    fluidRow(
      box(width = 3,
          selectInput("shape", "Select UFO Shape", choices = c(unique(ufo2$shape))),
          selectInput("state", "Select State", choices = c(unique(ufo2$state)))
      ),
      box(width = 8,
          plotOutput("plot", width = "600px", height = "500px"))
    )
  )
)

# Define server logic 
server <- function(input, output, session) {
  
  # Render plot based on selected UFO shape and state
  output$plot <- renderPlot({
    req(input$shape, input$state)  # Require both inputs to be selected
    
    filtered_data <- ufo2 %>%
      filter(shape == input$shape & state == input$state)
    
    if (nrow(filtered_data) == 0) {
      # If no data found, display a message
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "No data available")
    } else {
      # Otherwise, plot the data
      ggplot(filtered_data, aes(x = month)) +
        geom_bar(fill = "#43bfc7") +
        labs(title = paste("UFO Sightings for Shape:", input$shape, "and State:", input$state),
             x = "Month",
             y = "Number of Sightings") +
        theme_linedraw()
    }
  })
  session$onSessionEnded(stopApp)
}

# Start the app
shinyApp(ui, server)