#On progress bars: https://shiny.rstudio.com/articles/progress.html
#library(shinyjs)

#' @importFrom utils write.table
#' @importFrom magrittr `%>%`

######################################
### Anonymisation - User interface ###
######################################

anonymiseUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Pseudonymise or anonymise holding identifiers"),
    p("Here, you can pseudonymise your movement data, by modifying holding identifiers."),
    p("You can either change holding identifiers to a number or prefix-number
    combination (allocated in random order), or upload a pseudonymisation key that
    you wish to apply."),
    selectInput(ns("data"), "Select dataset to pseudonymise", choices = NULL),
    textInput(ns("prefix"), "Prefix (optional)"),
    fileInput(ns("key"), "Provide a pseudonymisation key to apply to the data (optional)"),
    #useShinyjs(),
    actionButton(ns("anonymise"), "Modify holding identifiers",
                 width = "100%"),
    progressBar(ns("anonymise_pb"), value = 0, display_pct = TRUE),
    # conditionalPanel("false", # always hide the download button
    #                  downloadButton(ns("download_key")))
    #br(),
    tags$b("Download pseudonymisation key"),
    br(),
    downloadButton(ns("download_rda"), "rda format (machine readable)"),
    downloadButton(ns("download_csv"), "csv format (human readable)"),
    br(),
    br(),
    p("For upload or download in csv format, please indicate the following file options"),
    fluidRow(
      column(2,
             textInput(ns("encoding"), "Encoding", value = "UTF-8")),
      column(2,
             textInput(ns("separator"), "Separator", value = ",")),
      column(2,
             textInput(ns("decimal"), "Decimal mark", value = ".")),
      column(6)
    )
  )
}

####################################
### Anonymisation - Server logic ###
####################################

anonymiseServer <- function(id, movement_data, modified_movement_data){
  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
      anonymised_data <- reactiveValues()
      ano_key <- reactiveValues()

      modified_datasets <-
        reactive({reactiveValuesToList(modified_movement_data)})
      all_datasets <-
        reactive({c(reactiveValuesToList(movement_data),
                    modified_datasets()[order(names(modified_datasets()))])})

      observe({
        updateSelectInput(session, "data", choices = names(all_datasets()))})

      selected_dataset <- reactive({all_datasets()[[input$data]]})

    # Pseudonymise / anonymise holding identifiers

      observeEvent(input$anonymise, {
        if(!is.null(input$key)){
          ext <- strsplit(input$key$name, ".", fixed=T)[[1]][-1]
          if(ext == "rda"){
            load(input$key$datapath, envir = environment())
          } else if(ext == "csv"){
            df <- read_delim(input$key$datapath,
                             delim = input$separator,
                             locale = locale(decimal_mark = input$decimal,
                                             encoding = input$encoding),
                             col_names = FALSE,
                             show_col_types = FALSE)
            key <- as.character(df[[2]])
            names(key) <- df[[1]]
          }
        } else {
          key <- NULL
        }
        ano_data_and_key <- internal_anonymise(selected_dataset(),
                                               col_to_anonymise = c(1,2),
                                               prefix = input$prefix,
                                               key = environment()$key)
        ano_data <- ano_data_and_key[[1]]
        ano_data_name <- paste0(input$data,"_Pseudonymised")
        anonymised_data[[ano_data_name]] <- ano_data

        ano_key$key <- ano_data_and_key[[2]]
        ano_key$name <- paste0(input$data,"_PseudonymisationKey")

        #runjs(paste0("$('#", ns("download_key"),"')[0].click();")) # click hidden download button

        updateProgressBar(session, "anonymise_pb", value = 1,
                          total = 1, range_value = c(0, 1))
        })

      observe({
        key <- ano_key$key

        output$download_rda <- downloadHandler(
          filename = function() {paste0(ano_key$name, ".rda")},
          content = function(file) {save(key, file = file)})
        #If two separate buttons, there is a chance that the "anonymise data"
        #button is pressed again accidentally (and without noticing), resulting
        #in the downloaded key accidentally no longer applying to the data.
        #If one button, key is downloaded automatically - is this acceptable behaviour?

        output$download_csv <- downloadHandler(
          filename = function() {paste0(ano_key$name, ".csv")},
          content = function(file) {write.table(key, file = file,
                                                sep = input$separator,
                                                dec = input$decimal,
                                                row.names = TRUE,
                                                col.names = FALSE,
                                                fileEncoding = input$encoding)})
      })



      return(anonymised_data)

})}
