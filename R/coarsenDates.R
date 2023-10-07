######################################
### Coarsen dates - User interface ###
######################################
#' @import lubridate

coarsenDatesUI <- function(id) {
  ns <- NS(id) # `NS(id)` returns a namespace function
  tagList(
    h3("Modify movement dates"),
    p("Here, you can modify movement dates, in order to make your movement data
      less identifiable."),
    p("You can choose to jitter (add random noise to) dates and/or to round dates
      (down to the first day of a selected time unit). For each jitter range or
      rounding unit provided, a new dataset will be created. You can view these
      datasets by selecting 'View and/or download datasets' in the sidebar menu."),
    p("As jittering dates contains an element of randomisation, you can choose to
      perform multiple simulations per range, resulting in the generation of multiple
      equivalent datasets."),
    selectInput(ns("dataset"),
                "Dataset to modify movement dates for",
                choices = NULL),
    fluidRow(
      column(6,
             h4("Jitter dates"),
             tags$b("Range(s) of jitter (+/- n days) to apply"),
             numericInput(ns("jitter_1"), label = NULL, min=1, value = 3),
             numericInput(ns("jitter_2"), label = NULL, min=1, value = NULL),
             numericInput(ns("jitter_3"), label = NULL, min=1, value = NULL),
             numericInput(ns("jitter_sims"),
                          "Number of simulations to perform per range",
                          min = 1, value = 3)),
      column(6,
             h4("Round dates"),
             selectInput(ns("rounding_units"), "Time unit(s) to round dates down to",
                         choices = list("No rounding" = "", "day", "week", "month", "3 months"),
                         multiple = TRUE),
             tags$b("Starting day of the week (if rounding to week)"),
             selectInput(ns("week_start"), label = NULL,
                         choices = list("Monday" = 1, "Tuesday" = 2, "Wednesday"
                                        = 3, "Thursday" = 4, "Friday" = 5,
                                        "Saturday" = 6, "Sunday" = 7,
                                        "weekday of first movement in dataset" =
                                        "from_data"), #Sensible options but different from package
                         selected = 1), #in package the default is lubridate setting (Sunday by default) but for European countries I think Monday makes more sense
             checkboxInput(ns("sum_weight"), "Aggregate weights for repeated movements over the selected time unit",
                           value = TRUE)
             )),
    # #Parallel processing doesn't seem to be worth it, functions are fast, exporting is slow
    # h4("Parallel processing"),
    # numericInput(ns("n_threads"), label = "Number of threads available",
    #              min = 1, value = 4),
    actionButton(ns("modify"), "Modify movement dates", width = "100%"),
    conditionalPanel(
      condition = "input.modify !== 0",
      progressBar(ns("modify_pb"), value = 0, display_pct = TRUE,
                  title = "Modifying movement dates..."),
      ns = NS(id)
    )
  )
}

####################################
### Coarsen dates - Server logic ###
####################################

coarsenDatesServer <- function(id, movement_data, modified_movement_data){
  moduleServer(
    id,
    function(input, output, session) {

      modified_datasets <-
        reactive({reactiveValuesToList(modified_movement_data)})
      modified_datasets_w_no_date_changes <-
        reactive({
          modified_datasets()[grep("Dates", names(modified_datasets()),
                                   fixed = TRUE, invert = TRUE)]})
      all_datasets_w_no_date_changes <-
        reactive({c(reactiveValuesToList(movement_data),
                    modified_datasets_w_no_date_changes()[order(names(modified_datasets_w_no_date_changes()))])})

      observe({
        updateSelectInput(session, "dataset",
                          choices = names(all_datasets_w_no_date_changes()),
                          selected = "original")

        updateSelectInput(session, "week_start",
                          choices = list("Monday" = 1, "Tuesday" = 2,
                                         "Wednesday" = 3, "Thursday" = 4,
                                         "Friday" = 5, "Saturday" = 6,
                                         "Sunday" = 7,
                                         "weekday of first movement in dataset"
                                         = "from_data"))
      })

      selected_dataset <- reactive({all_datasets_w_no_date_changes()[input$dataset]})

      observeEvent(input$modify, {

        if(is.null(input$jitter_sims)){
          jitter_set <- NULL
        } else {
          jitter_set <- rep(c(input$jitter_1, input$jitter_2,
                                input$jitter_3), input$jitter_sims)
          jitter_set <- jitter_set[which(jitter_set != 0 & !is.na(jitter_set))]
        }

        dataname <- names(selected_dataset())
        n_modif <- length(jitter_set) + length(input$rounding_units)
        modif_count <- 0

        updateProgressBar(session, "modify_pb", value = 0, total = n_modif,
                          range_value = c(0, n_modif),
                          title = "Modifying movement dates...")

        jittered_datasets <- NULL
        if(length(jitter_set) > 0){
          jittered_datasets <-
            lapply(seq_along(jitter_set), function(x){
              jd <- jitter_dates(data = selected_dataset()[[dataname]],
                                 range = jitter_set[[x]])
              modif_count <<- modif_count + 1
              updateProgressBar(session, "modify_pb", value = modif_count,
                                total = n_modif, range_value = c(0, n_modif),
                                title = "Jittering movement dates...")
              return(jd)})

          if(dataname == "original"){
            names(jittered_datasets) <-
              make.unique(paste0("DatesJittered_", jitter_set, "d"), sep="_")
          } else {
            names(jittered_datasets) <-
              make.unique(paste0(dataname, "_DatesJittered_", jitter_set, "d"), sep="_")
          }
        }

        rounded_datasets <- NULL
        if(length(input$rounding_units) > 0){

          week_start <- ifelse(input$week_start == "from_data",
                               wday(min(input$dataset[[3]])),
                               as.integer(input$week_start))

          rounded_datasets <-
            lapply(seq_along(input$rounding_units), function(x){
              rd <- round_dates(data = selected_dataset()[[dataname]],
                                unit = input$rounding_units[[x]],
                                week_start = week_start,
                                sum_weight = input$sum_weight)
              modif_count <<- modif_count + 1
              updateProgressBar(session, "modify_pb", value = modif_count,
                                total = n_modif, range_value = c(0, n_modif),
                                title = "Rounding movement dates...")
              return(rd)})

          summed <- switch(input$sum_weight, "aggr")
          if(dataname == "original"){
            names(rounded_datasets) <-
              paste0("DatesRounded_", gsub(" ", "", input$rounding_units), "WS",
                     week_start, summed)
          } else {
            names(rounded_datasets) <-
              paste0(dataname, "_DatesRounded_", gsub(" ", "", input$rounding_units),
                     "WS", week_start, summed)
          }
        }

        # #Parallel processing doesn't seem to be worth it, functions are fast, exporting is slow
        # datasets_w_coarsened_dates <-
        #   parallel_coarsen_dates(data = selected_dataset(),
        #                          n_threads = input$n_threads,
        #                          jitter_set = jitter_set,
        #                          rounding_set = input$rounding_units,
        #                          week_start = week_start, #default is Monday in the app, Sunday/lubridate default in function)
        #                          sum_weight = input$sum_weight)

        #NO alternative summary functions, can't get this to work correctly
        datasets_w_coarsened_dates <- c(jittered_datasets, rounded_datasets)
        lapply(seq_along(datasets_w_coarsened_dates),
               function(x){
                 modified_movement_data[[names(datasets_w_coarsened_dates[x])]] <-
                   datasets_w_coarsened_dates[[x]]})

        updateProgressBar(session, "modify_pb", value = modif_count,
                          total = n_modif, range_value = c(0, n_modif),
                          title = "Done!")

        })

      return(modified_movement_data)
    })
}

