#' Run the embedded shiny app
#' @import shiny
#' @import readr
#' @import yaml
#'
#' @export
runMovenetApp <- function(){

  ui <- fluidPage(
    titlePanel("Movenet"),
    navlistPanel(
      "Input datasets",
      tabPanel("Input movement data",
               dataInputUI("movement")),
      #tabPanel("Holding data",
      #         dataInputUI("holding")),
      "Make data non-identifiable",
      tabPanel("Modify movement dates",# and/or weights",
               coarsenDataUI("coarsen")),
      tabPanel("Modify holding identifiers (pseudonymise)",
               anonymiseUI("anonymise")),
      tabPanel("View and/or download datasets",
               viewDataUI("view_data")),
      "Network analysis",
      tabPanel("Generate networks & calculate measures",
               parallelProcessingInput(),
               createNetworksUI("networks"),
               calculateMeasureUI("overall_measures")),
      tabPanel("Explore a single network",
               exploreNetworkUI("explore")),
      tabPanel("Compare reachability across jitter/rounding ranges",
               plotMeasureOverGradientUI("maximum reachability")),
      #tabPanel("Compare distributions of monthly measures across networks",
      #         plotMonthlyMeasureViolinplotUI("maximum reachability")),
      #"Modelling",
      widths = c(3, 9)
    )
  )


  server <- function(input, output) {
    movement_data <- dataInputServer("movement")
    #holding_data <- dataInputServer("holding")
    modified_movement_data <- coarsenDataServer("coarsen", movement_data)
    anonymised_movement_data <- anonymiseServer("anonymise", movement_data,
                                                modified_movement_data)
    viewDataServer("view_data", movement_data, modified_movement_data,
                   anonymised_movement_data)
    nw <- createNetworksServer("networks", movement_data, modified_movement_data,
                               anonymised_movement_data, #holding_data,
                               n_threads = reactive(input$threads))
    networks <- nw$networks
    #monthly_networks <- nw$monthly_networks
    exploreNetworkServer("explore", networks, n_threads = reactive(input$threads))
    measures <- calculateMeasureServer("overall_measures", networks,
                                       #monthly_networks,
                                       n_threads = reactive(input$threads))
    plotMeasureOverGradientServer("maximum reachability", measures)
    #plotMonthlyMeasureViolinplotServer("maximum reachability", measures)
  }


  # Run the application
  shinyApp(ui = ui, server = server)

}
