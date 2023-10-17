########################################
### Coarsen weights - User interface ###
########################################

coarsenWeightsUI <- function(id) {
  ns <- NS(id) # `NS(id)` returns a namespace function
  id2<-sub("_", " " , id, fixed = TRUE)
  tagList(
    h3("Modify movement", id2),
    p("Here, you can modify movement", id2, "in order to make your movement
      data less identifiable."),
    p("You can choose to jitter (add random noise to) and/or to round", id2, "to
    multiples of a specified number. For each jitter range or rounding unit
    provided, a new dataset will be created. You can view these datasets by
      selecting 'View and/or download datasets' in the sidebar menu."),
    p("As jittering dates contains an element of randomisation, you can choose to
      perform multiple simulations per range, resulting in the generation of multiple
      equivalent datasets."),
    fluidRow(
      column(6,
             selectInput(ns("dataset"),
                         label = paste("Dataset to modify movement",id2,"for"),
                         choices = NULL)),
      column(6,
             if(id == "numeric_data"){
               selectInput(ns("column"),
                           label = "Optional numeric data column to modify",
                           choices = NULL)
             })
    ),
    fluidRow(
      column(6,
             h4(paste("Jitter", id2)),
             tags$b("Range(s) of jitter (+/- n) to apply"),
             numericInput(ns("jitter_1"), label = NULL, min=1, value = 3),
             numericInput(ns("jitter_2"), label = NULL, min=1, value = NULL),
             numericInput(ns("jitter_3"), label = NULL, min=1, value = NULL),
             numericInput(ns("jitter_sims"),
                          "Number of simulations to perform per range",
                          min = 1, value = 3)),
      column(6,
             h4(paste("Round", id2)),
             tags$b("Number(s) to which to round", id2, "to"),
             numericInput(ns("rounding_unit_1"), label = NULL, min=1, value = NULL),
             numericInput(ns("rounding_unit_2"), label = NULL, min=1, value = NULL),
             numericInput(ns("rounding_unit_3"), label = NULL, min=1, value = NULL))
    ),
    # #Parallel processing doesn't seem to be worth it, functions are fast, exporting is slow
    # h4("Parallel processing"),
    # numericInput(ns("n_threads"), label = "Number of threads available",
    #              min = 1, value = 4),
    actionButton(ns("modify"), paste("Modify movement", id2), width = "100%"),
    conditionalPanel(
      condition = "input.modify !== 0",
      progressBar(ns("modify_pb"), value = 0, display_pct = TRUE,
                  title = paste0("Modifying movement ", id2,"...")),
      ns = NS(id)
    )
  )
}

######################################
### Coarsen weights - Server logic ###
######################################

coarsenWeightsServer <- function(id, movement_data, modified_movement_data){
  moduleServer(
    id,
    function(input, output, session) {

      modified_datasets <-
        reactive({reactiveValuesToList(modified_movement_data)})
      modified_datasets_w_no_weight_changes <-
        reactive({
          modified_datasets()[grep("Weights", names(modified_datasets()),
                                   fixed = TRUE, invert = TRUE)]})

      if(id == "numeric_data"){
        all_datasets <-
          reactive({c(reactiveValuesToList(movement_data),
                      modified_datasets()[order(names(modified_datasets()))])})
        observe({
          updateSelectInput(session, "dataset",
                            choices = names(all_datasets()),
                            selected = "original")
        })
        selected_dataset <- reactive({all_datasets()[input$dataset]})

        observe({
          req(movement_data$original)
          numeric_opt_cols <- names(select(movement_data$original, (!(1:4) & is.numeric)))
          updateSelectInput(session, "column",
                            choices = numeric_opt_cols,
                            selected = NULL)
        })

      } else if(id == "weights"){
        all_datasets_w_no_weight_changes <-
          reactive({c(reactiveValuesToList(movement_data),
                      modified_datasets_w_no_weight_changes()[order(names(modified_datasets_w_no_weight_changes()))])})

        observe({
          updateSelectInput(session, "dataset",
                            choices = names(all_datasets_w_no_weight_changes()),
                            selected = "original")
        })
        selected_dataset <- reactive({all_datasets_w_no_weight_changes()[input$dataset]})
      }


      observeEvent(input$modify, {

        if(is.null(input$jitter_sims)){
          jitter_set <- NULL
        } else {
          jitter_set <- rep(c(input$jitter_1, input$jitter_2,
                              input$jitter_3), input$jitter_sims)
          jitter_set <- jitter_set[which(jitter_set != 0 & !is.na(jitter_set))]
        }

        rounding_set <- c(input$rounding_unit_1, input$rounding_unit_2,
                          input$rounding_unit_3)
        rounding_set <- rounding_set[which(rounding_set != 0 & !is.na(rounding_set))]

        dataname <- names(selected_dataset())
        n_modif <- length(jitter_set) + length(rounding_set)
        modif_count <- 0
        column <- switch(id, "weights" = movenetenv$options$movedata_cols$weight,
                         "numeric_data" = input$column)
        id2<-sub("_", " " , id, fixed = TRUE)
        id_in_names <- switch(id, "weights" = "Weights", "numeric_data" = paste0("'",column,"'"))

        updateProgressBar(session, "modify_pb", value = 0, total = n_modif,
                          range_value = c(0, n_modif),
                          title = paste0("Modifying movement ", id2,"..."))

        jittered_datasets <- NULL
        if(length(jitter_set) > 0){
          jittered_datasets <-
            lapply(seq_along(jitter_set), function(x){
              jd <- jitter_weights(data = selected_dataset()[[dataname]],
                                   range = jitter_set[[x]],
                                   column = column)
              modif_count <<- modif_count + 1
              updateProgressBar(session, "modify_pb", value = modif_count,
                                total = n_modif, range_value = c(0, n_modif),
                                title = paste0("Jittering movement ", id2,"..."))
              return(jd)})

          if(dataname == "original"){
            names(jittered_datasets) <-
              make.unique(paste0(id_in_names,"Jittered_", jitter_set), sep="_")
          } else {
            names(jittered_datasets) <-
              make.unique(paste0(dataname, "_",id_in_names,"Jittered_", jitter_set), sep="_")
          }
        }

        rounded_datasets <- NULL
        if(length(rounding_set) > 0){
          rounded_datasets <-
            lapply(seq_along(rounding_set), function(x){
              rd <- round_weights(data = selected_dataset()[[dataname]],
                                  unit = rounding_set[[x]],
                                  column = column)
              modif_count <<- modif_count + 1
              updateProgressBar(session, "modify_pb", value = modif_count,
                                total = n_modif, range_value = c(0, n_modif),
                                title = paste0("Rounding movement ", id2,"..."))
              return(rd)})

          if(dataname == "original"){
            names(rounded_datasets) <- paste0(id_in_names, "Rounded_", rounding_set)
          } else {
            names(rounded_datasets) <- paste0(dataname, "_", id_in_names,
                                              "Rounded_", rounding_set)
          }
        }

        # #Parallel processing doesn't seem to be worth it, functions are fast, exporting is slow
        # datasets_w_coarsened_weights <-
        #   parallel_coarsen_weights(data = selected_dataset(),
        #                            n_threads = input$n_threads,
        #                            jitter_set = jitter_set,
        #                            rounding_set = rounding_set,
        #                            column = movenetenv$options$movedata_cols$weight)

        datasets_w_coarsened_weights <- c(jittered_datasets, rounded_datasets)
        lapply(seq_along(datasets_w_coarsened_weights),
               function(x){
                 modified_movement_data[[names(datasets_w_coarsened_weights[x])]] <-
                   datasets_w_coarsened_weights[[x]]})

        updateProgressBar(session, "modify_pb", value = modif_count,
                          total = n_modif, range_value = c(0, n_modif),
                          title = "Done!")

      })

      return(modified_movement_data)
    })
}

