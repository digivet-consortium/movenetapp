library(pbapply)
library(tsna)

##########################################################
### Calculate overall network measure - User interface ###
##########################################################

calculateMeasureUI <- function(id) {
  ns <- NS(id) # `NS(id)` returns a namespace function
  tagList(
    h3("Calculate network measures"),
    fluidRow(
      column(4,
             h4("Overall network measures"),
             actionButton(ns("calc_max_reachability"),
                          "Maximum reachability", width = "100%"),
             progressBar(ns("pb_max_reachability"), value = 0,
                         display_pct = TRUE)),
      column(2),
      column(4,
             h4("Monthly network measures"),
             actionButton(ns("calc_monthly_max_reachability"),
                          "Monthly maximum reachability", width = "100%"),
             progressBar(ns("pb_monthly_max_reachability"), value = 0,
                         display_pct = TRUE)))
    )
}

########################################################
### Calculate overall network measure - Server logic ###
########################################################

calculateMeasureServer <- function(id, networks, n_threads) {
  moduleServer(
    id,
    function(input, output, session) {

      whole_networks <-
        reactive({list(networks$true, networks$jitter, networks$rounded)})
      monthly_networks <- reactive({
        c(networks$true_monthly, networks$jitter_monthly,
          networks$rounded_monthly) |>
        setNames(c(names(networks$true_monthly),
                   paste0("jitter (", names(networks$jitter_monthly), " days)"),
                   paste0("rounded (", names(networks$rounded_monthly),")")))
        })

      n_whole_networks <-
        reactive({length(unlist(whole_networks(), recursive = FALSE))})
      n_monthly_networks <-
        reactive({length(unlist(monthly_networks(), recursive = FALSE))})

      measures <- reactiveValues()

# Maximum reachability (overall) ------------------------------------------

      observeEvent(input$calc_max_reachability, {

        measures$max_reachability <-
          lapply(seq_along(whole_networks()),
                 function(x){
                   max_reach <-
                     parallel_max_reachabilities(whole_networks()[[x]],
                                                 n_threads())
                   updateProgressBar(session, "pb_max_reachability",
                                     value =
                                       length(unlist(whole_networks()[1:x],
                                                     recursive = FALSE)),
                                     total = n_whole_networks(),
                                     range_value = c(0, n_whole_networks()))
                   return(max_reach)
                 }) |>
          setNames(c("true","jitter","rounded")
                   [which(c("true","jitter","rounded") %in% names(networks))])

        })

# Maximum reachability (monthly) ------------------------------------------

      observeEvent(input$calc_monthly_max_reachability, {

        measures$monthly_max_reachability <-
          lapply(seq_along(monthly_networks()),
                 function(x){
                   max_reach <-
                     parallel_max_reachabilities(monthly_networks()[[x]],
                                                 n_threads())
                   updateProgressBar(session, "pb_monthly_max_reachability",
                                     value =
                                       length(unlist(monthly_networks()[1:x],
                                                     recursive = FALSE)),
                                     total = n_monthly_networks(),
                                     range_value = c(0, n_monthly_networks()))
                   return(max_reach)
                 }) |>
          setNames(names(monthly_networks()))

        })

      #return measures to the main app
      return(measures)

    })}
