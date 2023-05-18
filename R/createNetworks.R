#On progress bars: https://shiny.rstudio.com/articles/progress.html

#library(dplyr)
#library(lubridate)
#library(networkDynamic)
#library(parallel)
#library(pbapply)
#library(shinyWidgets)

#' @import lubridate
#' @import dplyr
#' @import networkDynamic
#' @import parallel
#' @import pbapply
#' @import shinyWidgets


########################################
### Create networks - User interface ###
########################################

createNetworksUI <- function(id) {
  ns <- NS(id) # `NS(id)` returns a namespace function
  tagList(
    h3("Create networks from datasets"),
    p("Here, you can generate networks from one or more (reformatted) movement datasets."),
    selectInput(ns("datasets"), "Select dataset(s) to turn into networks",
                choices = NULL, multiple = TRUE),
    actionButton(ns("create_networks"), "Generate networks", width = "100%"),
    progressBar(ns("create_networks_pb"), value = 0, display_pct = TRUE),
  )
}

######################################
### Create networks - Server logic ###
######################################

createNetworksServer <- function(id, movement_data, modified_movement_data,
                                 anonymised_movement_data, n_threads){
  moduleServer(
    id,
    function(input, output, session) {

      networks <- reactiveValues()
      monthly_networks <- reactiveValues()
      counts <- reactiveValues()

# Select datasets ---------------------------------------------------------

      modified_datasets <-
        reactive({reactiveValuesToList(modified_movement_data)})
      anonymised_datasets <-
        reactive({reactiveValuesToList(anonymised_movement_data)})
      all_datasets <-
        reactive({c(reactiveValuesToList(movement_data),
                    modified_datasets()[order(names(modified_datasets()))],
                    anonymised_datasets()[order(names(anonymised_datasets()))])})

      observe({
        updateSelectInput(session, "datasets",
                          choices = names(all_datasets()),
                          selected = "original")})

      selected_datasets <- reactive({all_datasets()[input$datasets]})


# # Anonymise holding ids ---------------------------------------------------
#
#         #Change holding ids to numbers (required for temporal networks)
#         anonymised_data <- simplified_anonymise(movement_data())
#         movement_data(anonymised_data)

# Create temporal networks ------------------------------------------------

      observeEvent(input$create_networks, {

        #Some useful variables
        counts$n_networks <- length(selected_datasets())
        # counts$months_in_data <-
        #   extract_periods(movement_data$original[["date"]], "month")
        # counts$n_monthlynetworks <-
        #   counts$n_networks * length(counts$months_in_data)
        # counts$n_allnetworks <- counts$n_networks + counts$n_monthlynetworks

        #Creating overall networks
        nw <-
          lapply(seq_along(selected_datasets()), function(x) {
            net <- movedata2networkDynamic(selected_datasets()[[x]])
            updateProgressBar(session, "create_networks_pb", value = x,
                              #total = counts$n_allnetworks,  #with monthly nw
                              total = counts$n_networks,  #w/o monthly nw
                              #range_value = c(0, counts$n_allnetworks))  #with monthly nw
                              range_value = c(0, counts$n_networks))  #w/o monthly nw
            return(net)}) %>%
          setNames(names(selected_datasets()))

        ## Parallelised alternative - DOES NOT WORK (progress bar... might need to use asynchronous programming)
        ## see https://github.com/rstudio/shiny/issues/2196#issuecomment-1016858715
        # nw <- parallel_movedata2networkDynamic(selected_datasets(), input$n_threads)

        lapply(seq_along(nw), function(x){networks[[names(nw[x])]] <- nw[[x]]})


        # #Creating monthly subnetworks  - WORKS BUT TAKEN OUT FOR WORKSHOP
        # monthly_nw <- extract_periodic_subnetworks(nw, n_threads(),
        #                                            counts$months_in_data)
        # updateProgressBar(session, "create_networks_pb",
        #                   value = counts$n_allnetworks,
        #                   total = counts$n_allnetworks,
        #                   range_value = c(0, counts$n_allnetworks))
        #
        # lapply(seq_along(monthly_nw), function(x){
        #   monthly_networks[[names(monthly_nw[x])]] <- monthly_nw[[x]]})

        })

      return(list(networks = networks))#, monthly_networks = monthly_networks))

})}
