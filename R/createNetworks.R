#On progress bars: https://shiny.rstudio.com/articles/progress.html

library(dplyr)
library(lubridate)
library(networkDynamic)
library(parallel)
library(shinyWidgets)
library(tsna)

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
    progressBar(ns("create_networks_pb"), value = 0),
    textOutput(ns("j")),
    #tableOutput(ns("anon_data")),
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

      networks <- reactiveValues()

      observeEvent(input$create_networks, {

# Anonymise holding ids ---------------------------------------------------

        #Change holding ids to numbers (required for temporal networks)
        anonymised_data <- simplified_anonymise(movement_data())
        movement_data(anonymised_data)

# Create temporal networks ------------------------------------------------
        n_jitter_networks <- length(input$jitter_days)*input$jitter_simulations
        n_rounded_networks <- length(input$rounding_units)
        n_networks <- 1 + n_jitter_networks + n_rounded_networks

        #Create true network
        networks$true <- movedata2networkDynamic(anonymised_data)
        updateProgressBar(session, "create_networks_pb", value = 1,
                          total = n_networks, range_value = c(0, n_networks))

        #Apply jitter
        jitter_days_reps <- rep(input$jitter_days, input$jitter_simulations)
        jitter_networks <-
          lapply(seq_along(jitter_days_reps), function(x){
            nw <- simplified_coarsen_date(anonymised_data, rounding_unit = FALSE,
                                          jitter = jitter_days_reps[x]) |>
              movedata2networkDynamic()
            updateProgressBar(session, "create_networks_pb", value = 1 + x,
                              total = n_networks, range_value = c(0, n_networks))
            return(nw)
          })
        names(jitter_networks) <-
          paste0("jitter (", rep(input$jitter_days, input$jitter_simulations)," days)")
        networks$jitter <- jitter_networks

        #Apply rounding
        week_start <- wday(min(anonymised_data[['date']]))
        rounded_networks <-
          lapply(seq_along(input$rounding_units), function(x){
            nw <- simplified_coarsen_date(anonymised_data, jitter = FALSE,
                                          rounding_unit = input$rounding_units[x],
                                          week_start = week_start) |>
              movedata2networkDynamic()
            updateProgressBar(session, "create_networks_pb",
                              value = 1 + n_jitter_networks + x,
                              total = n_networks, range_value = c(0, n_networks))
            return(nw)
          })
        names(rounded_networks) <- paste0(input$rounding_units,"ly")
        networks$rounded <- rounded_networks
      })

# Calculate temporal network measures -------------------------------------




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
