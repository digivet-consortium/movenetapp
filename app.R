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
             parallelProcessingInput(),
             createNetworksUI("networks"),
             calculateMeasureUI("overall_measures")),
    tabPanel("Explore a single network"),
    tabPanel("Compare network measures across jitter/rounding ranges",
             plotMeasureOverGradientUI("maximum reachability")),
    tabPanel("Compare distributions of monthly measures across networks",
             plotMonthlyMeasureViolinplotUI("maximum reachability")),
    "Modelling",
    widths = c(3, 9)
  )
)


server <- function(input, output) {
  movement_data <- dataInputServer("movement")
  holding_data <- dataInputServer("holding")
  networks <- createNetworksServer("networks", movement_data, holding_data,
                                   n_threads = reactive(input$threads))
  measures <- calculateMeasureServer("overall_measures", networks,
                                     n_threads = reactive(input$threads))
  plotMeasureOverGradientServer("maximum reachability", measures)
  plotMonthlyMeasureViolinplotServer("maximum reachability", measures)
}

# Run the application
shinyApp(ui = ui, server = server)
