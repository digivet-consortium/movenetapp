# library(pbapply)
# library(tsna)

#' @import pbapply
#' @import tsna


##########################################################
### Calculate overall network measure - User interface ###
##########################################################

calculateMeasureUI <- function(id) {
  ns <- NS(id) # `NS(id)` returns a namespace function
  tagList(
    h3("Calculate network measures across all networks"),
    p("Here, you can calculate network measures across all generated networks."),
    # fluidRow(
    #   column(4,
             # h4("Overall network measures"),
             actionButton(ns("calc_max_reachability"),
                          "Maximum reachability (slow)", width = "100%"),
    conditionalPanel(
      condition = "input.calc_max_reachability > 0",
      progressBar(ns("pb_max_reachability"), value = 0, display_pct = TRUE,
                  title = "Calculating maximum reachabilities..."),
      ns = NS(id)
    ))
      # column(2),
      # column(4,
      #        h4("Monthly network measures"),
      #        actionButton(ns("calc_monthly_max_reachability"),
      #                     "Monthly maximum reachability", width = "100%"),
      #        progressBar(ns("pb_monthly_max_reachability"), value = 0,
      #                    display_pct = TRUE)))
    # )
}

########################################################
### Calculate overall network measure - Server logic ###
########################################################

calculateMeasureServer <- function(id, networks, #monthly_networks,
                                   n_threads) {
  moduleServer(
    id,
    function(input, output, session) {

      whole_nw <- reactive({reactiveValuesToList(networks)})
      n_whole_networks <- reactive({length(whole_nw())})
      #monthly_nw <- reactive({reactiveValuesToList(monthly_networks)})
      #n_monthly_networks <-
      #  reactive({length(unlist(monthly_nw(), recursive = FALSE))})

      measures <- reactiveValues()

# Maximum reachability (overall) ------------------------------------------

      observeEvent(input$calc_max_reachability, {

        plan("multisession", workers = n_threads())
        #or:
        #plan(multisession(workers = availableCores() - 1)) # Leave one core for Shiny itself

        count <- 0 #to keep track of measures calculated, for progress bar
        n <- n_whole_networks()

        updateProgressBar(session, "pb_max_reachability",
                          value = 0, total = n, range_value = c(0, n),
                          title = "Calculating maximum reachabilities...")

        seq_len(n) %>%
          lapply(FUN = function(i) {
            nw <- whole_nw()[[i]]
            future_promise({max(tsna::tReach(nw, graph.step.time = 1))},
                           globals = c("nw"),
                           envir = environment()) %>%
              finally(function(){
                count <<- count + 1
                updateProgressBar(session, "pb_max_reachability",
                                  value = count, total = n,
                                  range_value = c(0, n),
                                  title = "Calculating maximum reachabilities...")})
          }) %>%
          promise_all(.list = .) %>%
          then(function(values){
            named_values <- unlist(values) %>% setNames(names(whole_nw()))
            measures$max_reachability <- named_values}) %>%
          finally(function(){
            updateProgressBar(session, "pb_max_reachability",
                              value = n, total = n, range_value = c(0, n),
                              title = "Done!")
          })
      })



# # Maximum reachability (monthly) ------------------------------------------
#
#       observeEvent(input$calc_monthly_max_reachability, {
#         measures$monthly_max_reachability <-
#           lapply(seq_along(monthly_nw()),
#                  function(x){
#                    max_reach <-
#                      parallel_max_reachabilities(monthly_nw()[[x]],
#                                                  n_threads())
#                    updateProgressBar(session, "pb_monthly_max_reachability",
#                                      value =
#                                        length(unlist(monthly_nw()[1:x],
#                                                      recursive = FALSE)),
#                                      total = n_monthly_networks(),
#                                      range_value = c(0, n_monthly_networks()))
#                    return(max_reach)
#                  }) %>%
#           setNames(names(monthly_nw()))
#         })

      #return measures to the main app
      return(measures)

    })}
