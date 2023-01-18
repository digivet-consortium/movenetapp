library(shiny)
library(readr) #read_delim
library(purrr) #flatten
library(yaml) #yaml.load_file

ui <- fluidPage(
  titlePanel("Movenet"),
  navlistPanel(
    "Inputs",
    tabPanel("Movement data",
             dataInputUI("movement")),
    tabPanel("Holding data",
             dataInputUI("holding")),
    "Network analysis",
    tabPanel("Generate networks & calculate measures",
             createNetworksUI("networks")),
    "Modelling",
    widths = c(3, 9)
  )
)


server <- function(input, output) {
  movement_data <- dataInputServer("movement")
  holding_data <- dataInputServer("holding")
  createNetworksServer("networks", movement_data, holding_data)
}

# Run the application
shinyApp(ui = ui, server = server)
