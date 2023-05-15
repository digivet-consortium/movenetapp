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
#' @import promises
#' @importFrom future plan multisession

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
    conditionalPanel(
      condition = "input.create_networks > 0",
      progressBar(ns("create_networks_pb"), value = 0, display_pct = TRUE,
                  title = "Generating networks..."),
      ns = NS(id)
    )

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

        plan("multisession", workers = n_threads())
        #or:
        #plan(multisession(workers = availableCores() - 1)) # Leave one core for Shiny itself

        nw_count <- 0 #to keep track of networks created, for progress bar
        n <- length(selected_datasets())

        updateProgressBar(session, "create_networks_pb",
                          value = 0, total = n, range_value = c(0, n),
                          title = "Generating networks...")

        seq_len(n) %>%
          lapply(FUN = function(i) {
            df <- selected_datasets()[[i]]
            future_promise({movenetapp:::movedata2networkDynamic(df)},
                           globals = c("df"),
                           envir = environment(),
                           seed = TRUE
                           ) %>%
              then(function(nw){ networks[[names(selected_datasets()[i])]] <- nw }) %>%
              finally(function(){
                nw_count <<- nw_count + 1
                updateProgressBar(session, "create_networks_pb",
                                  value = nw_count, total = n,
                                  range_value = c(0, n),
                                  title = "Generating networks...")})
          }) %>%
          promise_all(.list = .) %>%
          finally(function(){
            updateProgressBar(session, "create_networks_pb",
                              value = n, total = n, range_value = c(0, n),
                              title = "Done!")
            })
       })

      return(list(networks = networks))#, monthly_networks = monthly_networks))

})}
