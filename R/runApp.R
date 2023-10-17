#' Run the embedded shiny app
#' @import movenet
#' @import shiny
#' @import shinyFeedback
#' @import readr
#' @import yaml
#'
#' @export
runMovenetApp <- function(...){

  ui <- fluidPage(
    titlePanel("Movenet"),
    navlistPanel(
      "Input datasets",
      tabPanel("Movement data",
               dataInputUI("movement")),
      tabPanel("Holding data",
               dataInputUI("holding")),
      "Make data non-identifiable",
      tabPanel("Modify movement dates",
               coarsenDatesUI("coarsen")),
      tabPanel("Modify movement weights",
               coarsenWeightsUI("weights")),
      tabPanel("Modify optional numeric movement data",
               coarsenWeightsUI("numeric_data")),
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
    holding_data <- dataInputServer("holding")
    modified_movement_data <- reactiveValues()
    modified_movement_data <- coarsenDatesServer("coarsen", movement_data,
                                                 modified_movement_data)
    modified_movement_data <- coarsenWeightsServer("weights", movement_data,
                                                   modified_movement_data)
    modified_movement_data <- coarsenWeightsServer("numeric_data", movement_data,
                                                   modified_movement_data)
    anonymised_movement_data <- anonymiseServer("anonymise", movement_data,
                                                modified_movement_data)
    viewDataServer("view_data", movement_data, modified_movement_data,
                   anonymised_movement_data)
    nw <- createNetworksServer("networks", movement_data, modified_movement_data,
                               holding_data, #modified_holding_data,
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

  # Function to make the app
  app <- shinyApp(ui = ui, server = server)

  # To ensure safe setting of multisession strategies inside the app, run app in
  # local block, with on.exit statement
  # https://github.com/rstudio/promises/issues/93#issuecomment-1557675136

  local({
    # Save the previous multisession strategy ("plan")
    oplan <- future::plan()
    # Restore the original multisession strategy upon exiting the local block,
    # for potential continued use of the R session
    on.exit({ future::plan(oplan) })

    # Run app
    shiny::runApp(app, ...)
  })

}
