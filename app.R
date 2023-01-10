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
    widths = c(3, 9)
  )
)


server <- function(input, output) {
  dataInputServer("movement")
  dataInputServer("holding")
}

# Run the application
shinyApp(ui = ui, server = server)
