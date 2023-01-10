library(shiny)
library(readr) #read_delim
library(purrr) #flatten
library(yaml) #yaml.load_file

ui <- fluidPage(
  titlePanel("Movenet"),
  navlistPanel(
    "Inputs",
    tabPanel("Movement data",
             csvFileUI("movement")),
    tabPanel("Holding data",
             csvFileUI("holding")),
    widths = c(3, 9)
  )
)


server <- function(input, output) {
  csvFileServer("movement")
  csvFileServer("holding")
}

# Run the application
shinyApp(ui = ui, server = server)
