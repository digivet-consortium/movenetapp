#On progress bars: https://shiny.rstudio.com/articles/progress.html

library(dplyr)
library(lubridate)
library(networkDynamic)
library(parallel)
library(pbapply)
library(shinyWidgets)

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
                                choices = list("week", "month", "2 months",
                                               "3 months", "6 months", "year"),
                                selected =
                                  list("week","month","3 months","year"))),
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
                            "Number of simulations to perform per range",
                          min = 1,
                          value = 5))),
    actionButton(ns("create_networks"), "Generate networks", width = "100%"),
    progressBar(ns("create_networks_pb"), value = 0, display_pct = TRUE),
  )
}

######################################
### Create networks - Server logic ###
######################################

createNetworksServer <- function(id, movement_data, holding_data, n_threads){
  moduleServer(
    id,
    function(input, output, session) {

      networks <- reactiveValues()
      counts <- reactiveValues()

      observeEvent(input$create_networks, {

# Anonymise holding ids ---------------------------------------------------

        #Change holding ids to numbers (required for temporal networks)
        anonymised_data <- simplified_anonymise(movement_data())
        movement_data(anonymised_data)

# Create temporal networks ------------------------------------------------

        #Some useful variables
        counts$n_jitter_networks <-
          length(input$jitter_days)*input$jitter_simulations
        counts$n_rounded_networks <- length(input$rounding_units)
        counts$n_networks <-
          1 + counts$n_jitter_networks + counts$n_rounded_networks
        counts$months_in_data <-
          extract_periods(anonymised_data[["date"]], "month")
        counts$n_monthlynetworks <-
          counts$n_networks * length(counts$months_in_data)
        counts$n_allnetworks <- counts$n_networks + counts$n_monthlynetworks

        #Create true network
        networks$true <- list(true = movedata2networkDynamic(anonymised_data))
        updateProgressBar(session, "create_networks_pb", value = 1,
                          total = counts$n_allnetworks,
                          range_value = c(0, counts$n_allnetworks))

        #Apply jitter
        jitter_days_reps <- rep(input$jitter_days, input$jitter_simulations)
        networks$jitter <-
          lapply(seq_along(jitter_days_reps), function(x){
            nw <- simplified_coarsen_date(anonymised_data,
                                          rounding_unit = FALSE,
                                          jitter = jitter_days_reps[x]) |>
              movedata2networkDynamic()
            updateProgressBar(session, "create_networks_pb", value = 1 + x,
                              total = counts$n_allnetworks,
                              range_value = c(0, counts$n_allnetworks))
            return(nw)}) |>
          setNames(rep(input$jitter_days, input$jitter_simulations))

        #Apply rounding
        week_start <- wday(min(anonymised_data[['date']]))
        networks$rounded <-
          lapply(seq_along(input$rounding_units), function(x){
            nw <-
              simplified_coarsen_date(anonymised_data, jitter = FALSE,
                                      rounding_unit = input$rounding_units[x],
                                      week_start = week_start) |>
              movedata2networkDynamic()
            updateProgressBar(session, "create_networks_pb",
                              value = 1 + counts$n_jitter_networks + x,
                              total = counts$n_allnetworks,
                              range_value = c(0, counts$n_allnetworks))
            return(nw)}) |>
          setNames(input$rounding_units)

        #Create monthly networks
        networks$true_monthly <-
          extract_periodic_subnetworks(networks$true, n_threads(),
                                       counts$months_in_data)
        updateProgressBar(session, "create_networks_pb",
                          value =
                            counts$n_networks + length(counts$months_in_data),
                          total = counts$n_allnetworks,
                          range_value = c(0, counts$n_allnetworks))

        networks$jitter_monthly <-
          extract_periodic_subnetworks(networks$jitter, n_threads(),
                                       counts$months_in_data)
        updateProgressBar(session, "create_networks_pb",
                          value =
                            counts$n_networks + (counts$n_jitter_networks + 1)*
                            length(counts$months_in_data),
                          total = counts$n_allnetworks,
                          range_value = c(0, counts$n_allnetworks))

        networks$rounded_monthly <-
          extract_periodic_subnetworks(networks$rounded, n_threads(),
                                       counts$months_in_data)
        updateProgressBar(session, "create_networks_pb",
                          value = counts$n_allnetworks,
                          total = counts$n_allnetworks,
                          range_value = c(0, counts$n_allnetworks))
      })

      return(networks)

})}
