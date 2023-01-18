#On progress bars: https://shiny.rstudio.com/articles/progress.html


########################################
### Create networks - User interface ###
########################################

createNetworksUI <- function(id) {
  ns <- NS(id) # `NS(id)` returns a namespace function
  tagList(
    h3("Anonymisation options"),
    # Add explanatory blurb
    fluidRow(
      column(6,
             h4("Rounding"),
             checkboxGroupInput(ns("rounding_units"),
                                label = "Unit to round dates down to",
                                choices = list("week", "month",
                                               "2 months" = "bimonth",
                                               "3 months" = "quarter",
                                               "6 months" = "halfyear", "year"),
                                selected =
                                  list("week","month","quarter","year"))),
      column(6,
             h4("Jitter"),
             checkboxGroupInput(ns("jitter_days"),
                                label = "Range of jitter (+/- n days) to apply
                                to dates",
                                choices = list(
                                  "4 (~ equivalent to range of 1 week)" = 4,
                                  "15 (~ equivalent to range of 1 month)" = 15,
                                  "30 (~ equivalent to range of 2 months)" = 30,
                                  "46 (~ equivalent to range of 3 months)" = 46,
                                  "91 (~ equivalent to range of 6 months)" = 91,
                                  "183 (~ equivalent to range of 1 year)" = 183),
                                selected = list(4, 15, 46, 183)),
             numericInput(ns("jitter_simulations"),
                          label =
                            "Number of simulations to perform per jitter range",
                          value = 5))),
    actionButton(ns("create_networks"), "Generate networks", width = "100%"),
    h3("Network measures"),
    fluidRow(
      column(6,
             h4("Measures"),
             checkboxGroupInput(ns("network_measures"),
                                label = "Select network measures of interest",
                                choices = list("Maximum reachability"),
                                selected = list("Maximum reachability"))),
      column(6,
             h4("Parallel processing"),
             numericInput(ns("threads"),
                          label = "Number of threads",
                          value = 4))),
    actionButton(ns("calculate_measures"), "Calculate measures", width = "100%"),
  )
}

######################################
### Create networks - Server logic ###
######################################

createNetworksServer <- function(id, movement_data, holding_data) {
  moduleServer(
    id,
    function(input, output, session) {


      # Change id's to numbers - use anonymise
      # Apply round and jitter functions
      # Create networkDynamic objects

      # Fig 1 prep: monthly Max reach
      ###############################
      # Extract months in dataset
      # Looping over jitter/rounding networks, then over months:
      #      extract monthly networks;
      #      identify reachable set;
      #      max()
      # (Reformatting for plotting)

      # Fig 2 prep: overall max reach
      ###############################
      # Looping over jitter/rounding networks:
      #     identify reachable set; max()

      #output$datatable <- renderDataTable({movement_data()})

})}
