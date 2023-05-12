#####################################
### Coarsen data - User interface ###
#####################################
# library(lubridate)
#' @import lubridate

coarsenDataUI <- function(id) {
  ns <- NS(id) # `NS(id)` returns a namespace function
  tagList(
    h3("Modify movement dates"),
    p("Here, you can modify movement dates, in order to make your movement data
      less identifiable."),
    p("You can choose to jitter (add random noise to) dates and/or to round dates
      (down to the first day of a selected time unit). For each jitter range or
      rounding unit provided, a new dataset will be created. You can view these
      datasets by selecting 'View and/or download datasets' in the sidebar menu."),
    # p("As jittering dates contains an element of randomisation, you can choose to
    #   perform multiple simulations per range, resulting in the generation of multiple
    #   equivalent datasets."),
    # selectInput(ns("dataset"),
    #             "Dataset to modify movement dates for",
    #             choices = NULL),
    fluidRow(
      column(6,
             h4("Jitter dates"),
             tags$b("Range(s) of jitter (+/- n days) to apply to dates"),
             numericInput(ns("jitter_d_1"), label = NULL, min=0, value = 3),
             numericInput(ns("jitter_d_2"), label = NULL, min=0, value = NULL),
             numericInput(ns("jitter_d_3"), label = NULL, min=0, value = NULL)),
             # numericInput(ns("jitter_d_sims"),
             #              "Number of simulations to perform per range",
             #              min = 1, value = 3)),
      column(6,
             h4("Round dates"),
             #textInput(ns("rounding_units"), "Time unit to round dates down to"), #equivalent to function, may raise errors due to unacceptable time units
             selectInput(ns("rounding_units_d"), "Time unit(s) to round dates down to",
                         choices = list("No rounding" = "", "week", "month", "3 months"),
                                        #"2 months", "3 months", "4 months",
                                        #"6 months", "year"), #more restrictive than function in package, but sensible options
                         multiple = TRUE),
             tags$b("Starting day of the week (if rounding to week)"),
             selectInput(ns("week_start"), label = NULL,
                         choices = list("Monday" = 1, "Tuesday" = 2, "Wednesday"
                                        = 3, "Thursday" = 4, "Friday" = 5,
                                        "Saturday" = 6, "Sunday" = 7,
                                        "weekday of first movement in dataset" =
                                        "from_data"), #Sensible options but different from package
                         selected = 1), #in package the default is lubridate setting (Sunday by default) but for European countries I think Monday makes more sense
             #tags$b("Aggregate/summarise data for repeated movements over the selected time unit?"),
             checkboxInput(ns("sum_weight"), "Aggregate weights for repeated movements over the selected time unit",
                           value = TRUE),
             #p("Additional or alternative summary function(s)"),
             #textInput(ns("alt_summary_function"), label = NULL),
             #tags$i("These should take the form 'new_column_header =
             #function(existing_column_header)'. If providing multiple functions,
             #please separate with a comma.", tags$br(), "Example: 'mean_weight = mean(weight)'")
             )),
    # h3("Modify movement weights"),
    # fluidRow(
    #   column(6,
    #          h4("Jitter weights"),
    #          tags$b("Range(s) of jitter (+/- n) to apply to weights"),
    #          numericInput(ns("jitter_w_1"), label = NULL, min=0, value = 5),
    #          numericInput(ns("jitter_w_2"), label = NULL, min=0, value = NULL),
    #          numericInput(ns("jitter_w_3"), label = NULL, min=0, value = NULL),
    #          numericInput(ns("jitter_w_sims"),
    #                       "Number of simulations to perform per range",
    #                       min = 0, value = 3)),
    #   column(6,
    #          h4("Round weights"),
    #          tags$b("Number to round weights to"),
    #          numericInput(ns("rounding_w_1"), label = NULL, min=0, value = NULL),
    #          numericInput(ns("rounding_w_2"), label = NULL, min=0, value = NULL),
    #          numericInput(ns("rounding_w_3), label = NULL, min=0, value = NULL))),
    h4("Parallel processing"),
    numericInput(ns("n_threads"), label = "Number of threads available",
                 min = 1, value = 4),
    actionButton(ns("modify_data"), "Modify movement data", width = "100%"),
    progressBar(ns("modify_data_pb"), value = 0, display_pct = TRUE),
  )
}
#Scale up for datasets, or for different amounts of jitter or rounding?
# I had previously implemented the latter, but this assumes jitter OR rounding,
# whereas in the package we allow a user to choose jitter AND rounding

###################################
### Coarsen data - Server logic ###
###################################

coarsenDataServer <- function(id, movement_data){
  moduleServer(
    id,
    function(input, output, session) {

      counts <- reactiveValues()
      modified_data <- reactiveValues()

      observeEvent(movement_data$original, {
      #   choices <- list(names(movement_data))
      #   updateSelectInput(session, "dataset",
      #                     choices = choices,
      #                     selected = NULL)
      #   paste("input$dataset's structure:", str(input$dataset))
      #   #paste("cho's structure:", str(cho))
        updateSelectInput(session, "week_start",
                          choices = list("Monday" = 1, "Tuesday" = 2,
                                         "Wednesday" = 3, "Thursday" = 4,
                                         "Friday" = 5, "Saturday" = 6,
                                         "Sunday" = 7,
                                         "weekday of first movement in dataset"
                                         = "from_data"))
     })



      observeEvent(input$modify_data, {
        jitter_d_set <- c(input$jitter_d_1, input$jitter_d_2, input$jitter_d_3)
        # jitter_d_set <- rep(c(input$jitter_d_1, input$jitter_d_2,
        #                       input$jitter_d_3), input$jitter_d_sims)
        jitter_d_set <- jitter_d_set[which(jitter_d_set != 0 & !is.na(jitter_d_set))]
        week_start <- ifelse(input$week_start == "from_data",
                             wday(min(movement_data$original[[3]])),
                             as.integer(input$week_start))

        #alt_summ_fun <- input$alt_summary_function

        datasets_w_coarsened_dates <-
          parallel_coarsen_date(data = movement_data$original,
                                n_threads = input$n_threads,
                                jitter_set = jitter_d_set,
                                rounding_set = input$rounding_units_d,
                                week_start = week_start, #default is Monday in the app, Sunday/lubridate default in function)
                                sum_weight = input$sum_weight)
        #NO alternative summary functions, can't get this to work correctly
        updateProgressBar(session, "modify_data_pb", value = 1,
                           total = 1, range_value = c(0, 1))

        # jitter_w_set <- rep(c(input$jitter_w_1, input$jitter_w_2,
        #                       input$jitter_w_3), input$jitter_w_sims)
        # jitter_w_set <- jitter_w_set[which(jitter_w_set != 0 & !is.na(jitter_w_set))]
        #
        # rounding_w_set <- c(input$rounding_w_1, input$rounding_w_2,
        #                     input$rounding_w_3)
        # rounding_w_set <- rounding_w_set[which(rounding_w_set != 0 & !is.na(rounding_w_set))]
        #
        # ## N.B. The below doesnt work - in part because function has not actually
        # ##      been parallelised fo multiple datasets!!
        # datasets_w_coarsened_weights <-
        #   parallel_coarsen_weight(data = datasets_w_coarsened_dates,
        #                           n_threads = input$n_threads,
        #                           jitter_set = jitter_w_set,
        #                           rounding_set = rounding_w_set)
        # updateProgressBar(session, "modify_data_pb", value = 2,
        #                   total = 2, range_value = c(0, 2))

        lapply(seq_along(datasets_w_coarsened_dates),
               function(x){
                 modified_data[[names(datasets_w_coarsened_dates[x])]] <- datasets_w_coarsened_dates[[x]]})

        # lapply(seq_along(datasets_w_coarsened_weights),
        #        function(x){
        #          modified_data[[names(datasets_w_coarsened_weights[x])]] <-
        #            datasets_w_coarsened_weights[[x]]})

        })

      return(modified_data)
    })
}

