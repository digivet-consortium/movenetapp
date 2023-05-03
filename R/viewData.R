######################################
### View datasets - User interface ###
######################################

viewDataUI <- function(id) {
  ns <- NS(id) # `NS(id)` returns a namespace function
  tagList(
    h3("View (modified) datasets"),
    selectInput(ns("data_to_view"), "Select dataset to view",
                choices = NULL),
    dataTableOutput(ns("datatable")),
    h3("Download selected dataset"),
    downloadButton(ns("download_rda"), "rda format (machine readable)"),
    downloadButton(ns("download_csv"), "csv format (human readable)"),
    br(),
    br(),
    p("For download in csv format, please indicate the following file options"),
    fluidRow(
      column(2,
             textInput(ns("encoding"), "Encoding", value = "UTF-8")),
      column(2,
             textInput(ns("separator"), "Separator", value = ",")),
      column(2,
             textInput(ns("decimal"), "Decimal mark", value = ".")),
      column(6)
    )

  )}


####################################
### View datasets - Server logic ###
####################################

viewDataServer <- function(id, movement_data, modified_movement_data,
                           anonymised_movement_data){
  moduleServer(
    id,
    function(input, output, session) {

      modified_datasets <-
        reactive({reactiveValuesToList(modified_movement_data)})
      anonymised_datasets <-
        reactive({reactiveValuesToList(anonymised_movement_data)})
      all_datasets <-
        reactive({c(reactiveValuesToList(movement_data),
                    modified_datasets()[order(names(modified_datasets()))],
                    anonymised_datasets()[order(names(anonymised_datasets()))])})

      observe({
        updateSelectInput(session, "data_to_view",
                          choices = names(all_datasets()))})

      selected_dataset <- reactive({all_datasets()[[input$data_to_view]]})

      output$datatable <- renderDataTable({selected_dataset()})

      observe({
        data <- selected_dataset()

        output$download_rda <- downloadHandler(
          filename = function() {paste0(input$data_to_view, ".rda")},
          content = function(file) {save(data, file = file)}
        )
        output$download_csv <- downloadHandler(
          filename = function() {paste0(input$data_to_view, ".csv")},
          content = function(file) {write.table(selected_dataset(), file = file,
                                                sep = input$separator,
                                                dec = input$decimal,
                                                row.names = FALSE,
                                                fileEncoding = input$encoding)}
        )
      })

      })}
