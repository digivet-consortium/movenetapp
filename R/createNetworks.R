#On progress bars: https://shiny.rstudio.com/articles/progress.html

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
    selectInput(ns("datasets"), labelMandatory("Select movement dataset(s) to turn into networks"),
                choices = NULL, multiple = TRUE),
    selectInput(ns("holding_dataset"), "Select holding dataset",
                choices = NULL),
    checkboxInput(ns("incl_nonactive_holdings"), "Include non-active holdings?"),
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
                                 holding_data, #modified_holding_data,
                                 n_threads){
  moduleServer(
    id,
    function(input, output, session) {

      networks <- reactiveValues()
      monthly_networks <- reactiveValues()
      counts <- reactiveValues()

# Select datasets ---------------------------------------------------------

      modified_datasets <-
        reactive({reactiveValuesToList(modified_movement_data)})
      all_datasets <-
        reactive({c(reactiveValuesToList(movement_data),
                    modified_datasets()[order(names(modified_datasets()))])})

      # modified_holding_datasets <-
      #   reactive({reactiveValuesToList(modified_holding_data)})
      all_holding_datasets <-
        reactive({c(reactiveValuesToList(holding_data)#,
                    #modified_holding_datasets()[order(names(modified_holding_datasets()))]
                    )})

      observe({
        updateSelectInput(session, "datasets",
                          choices = names(all_datasets()),
                          selected = "original")
        updateSelectInput(session, "holding_dataset",
                          choices = names(all_holding_datasets()),
                          selected = "original")})

      selected_datasets <- reactive({all_datasets()[input$datasets]})
      selected_holding_dataset <- reactive({all_holding_datasets()[[input$holding_dataset]]})


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
            holding_data <- selected_holding_dataset()
            incl_nonactive <- input$incl_nonactive_holdings
            future_promise({movenet::movedata2networkDynamic(df,
                                                    holding_data,
                                                    incl_nonactive)},
                           globals = c("df", "holding_data", "incl_nonactive",
                                       "movedata2networkDynamic"),
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
