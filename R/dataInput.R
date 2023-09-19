
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

appCSS <- ".mandatory_star { color: red; }"

selectInputs_empty <- c("", "Select appropriate column header",
                        "Click 'Populate' to fill with column headers")

movenetenv <- movenet:::movenetenv

###################################
### Data Input - User interface ###
###################################
dataInputUI <- function(id) {

  ns <- NS(id) # `NS(id)` returns a namespace function
  tagList(
    shinyFeedback::useShinyFeedback(),
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(appCSS),

# Data file upload --------------------------------------------------------

    h3("Upload", id, "data file"),
    p("Here, upload a livestock", id, "data file (in csv format) that
    you would like to make non-identifiable, or explore as a network."),
    fileInput(ns("data_file"), labelMandatory(paste(stringr::str_to_title(id)," data file")),
              accept = c("text/csv", "text/comma-separated-values",
                         "text/plain", ".csv")),

# Configurations ----------------------------------------------------------

    h3("Configurations"),
    p("Please provide some configurations relating to the", id, "data file,
      so that it can be read in and reformatted in a correct manner."),
    p("We have made these configurations explicit, to avoid reliance on
    assumptions inherent in underlying software, and to allow a broader range
      of file formats to be read in correctly."),
    fluidRow(
      column(4,
             h4("Config file"),
             br(),
             p("EITHER upload an appropriate Movenet", id, "config file OR
               complete the configurations to the right."),
             br(),
             fileInput(ns("config_file"), label = paste(stringr::str_to_title(id)," or combined config file"),
                       accept = c("text/yaml", "text/yml", "text/x-yaml",
                                  "text/plain", "application/x-yaml", ".yml")),
             uiOutput(ns("config_file_progress"))
             ),
      column(8,
             style = "border-left: 1px solid",
             fluidRow(
               column(6,
                      h4("File and data options"),
                      textInput(ns("encoding"), labelMandatory("Encoding"), value = "UTF-8"),
                      textInput(ns("separator"), labelMandatory("Separator"), value = ","),
                      textInput(ns("decimal"), labelMandatory("Decimal mark"), value = "."),
                      create_data_options(id, ns)),
               column(6,
                      splitLayout(
                        h4("Data fields (column headers)"),
                        actionButton(ns("read_headers"), "Populate*"),
                        cellWidths = c("67%","33%")),
                      p("*Requires uploaded data file and completed file options"),
                      create_data_fields(id, ns))
             ),
             fluidRow(
               actionButton(ns("submit_config"), "Submit configurations",
                            width = "100%"),
               conditionalPanel(
                 condition = "input.submit_config > 0",
                 progressBar(ns("submit_config_pb"), value = 0, display_pct = TRUE,
                             title = "Saving configurations..."),
                 ns = NS(id)
               )
             ))),


# Extract & reformat data -------------------------------------------------

    br(),
    shinyjs::disabled(
      actionButton(ns("extract"), "Extract and reformat relevant columns",
                   width = "100%")
    ),
    conditionalPanel(
      condition = "input.extract > 0",
      progressBar(ns("extract_pb"), value = 0, display_pct = TRUE,
                  title = "Extracting and reformatting data..."),
      ns = NS(id)
    ),
    conditionalPanel(
      condition = "input.extract > 0",
      actionButton(ns("view_extracted_data"), "View extracted data",
                   width = "100%"),
      dataTableOutput(ns("datatable")),
      ns = NS(id)
    )
  )
}

#################################
### Data Input - Server logic ###
#################################

dataInputServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

# Setting up some useful handles ------------------------------------------

      # Input names for all textInputs
      text_input_names <- reactive({
        names(input)[which(names(input) %in% c("encoding", "separator",
                                               "decimal", "date_format",
                                               "coord_EPSG_code", "country_code"))]})

      # Input names for all selectInputs (preserving order of data fields)
      select_input_names <- reactive({
        data_fields <- c("from","to","date","weight","id","coord_x","coord_y")
        data_fields[which(data_fields %in% names(input))]})

      # Reactive value to store input data in
      input_data <- reactiveValues()


# Helper button -----------------------------------------------------------

      observeEvent(input$date_format_helper, {
        showModal(modalDialog(
          title = "Date formats",
          "Movenetapp uses readr date formats, except that empty strings are not
          accepted. For a flexible YMD parser, use '%AD'. See",
          tags$a(href='https://readr.tidyverse.org/reference/parse_datetime.html',
          "readr documentation", target="_blank"),"for further guidance.",
          easyClose = TRUE,
          footer = NULL
        ))
      })

# Datafield validation ----------------------------------------------------

      observe({
        if(id == "movement"){
          required_fileopts <- c("encoding","separator","decimal","date_format")
        } else if(id == "holding"){
          required_fileopts <- c("encoding","separator","decimal")}
        fields_for_populate <- c("data_file", required_fileopts)

        fields_for_populate_filled <-
          vapply(fields_for_populate,
                 function(x){!is.null(input[[x]]) && input[[x]] != ""},
                 logical(1))
        fields_for_populate_filled <- all(fields_for_populate_filled)
        shinyjs::toggleState(id = "read_headers",
                             condition = fields_for_populate_filled)

      })

      observeEvent(input$submit_config, {

        updateProgressBar(session, "submit_config_pb", value = 0,
                          title = "Saving configurations...")

        #Validate file options and save as configs
        if(input$encoding == ""){
          showFeedbackDanger(inputId = "encoding", text = paste("Required"))
          encoding_valid <- FALSE
        } else {
          hideFeedback("encoding"); encoding_valid <- TRUE
          if(id == "movement"){
            movenetenv$options$movedata_fileopts$encoding <- input$encoding
          } else if(id == "holding") {
            movenetenv$options$holdingdata_fileopts$encoding <- input$encoding
        }}

        if(input$separator == ""){
          hideFeedback("separator")
          showFeedbackDanger(inputId = "separator", text = paste("Required"))
          separator_valid <- FALSE
        } else if(nchar(input$separator) > 1){
          hideFeedback("separator")
          showFeedbackDanger(inputId = "separator",
                             text = paste("Must be a single character"))
          separator_valid <- FALSE
        } else {
          hideFeedback("separator"); separator_valid <- TRUE
          if(id == "movement"){
            movenetenv$options$movedata_fileopts$separator <- input$separator
          } else if(id == "holding") {
            movenetenv$options$holdingdata_fileopts$separator <- input$separator
        }}

        if(input$decimal == ""){
          hideFeedback("decimal")
          showFeedbackDanger(inputId = "decimal", text = paste("Required"))
          decimal_valid <- FALSE
        } else if(nchar(input$decimal) > 1){
          hideFeedback("decimal")
          showFeedbackDanger(inputId = "decimal",
                             text = paste("Must be a single character"))
          decimal_valid <- FALSE
        } else {
          hideFeedback("decimal"); decimal_valid <- TRUE
          if(id == "movement"){
            movenetenv$options$movedata_fileopts$decimal <- input$decimal
          } else if(id == "holding") {
            movenetenv$options$holdingdata_fileopts$decimal <- input$decimal
        }}

        if(id == "movement"){
          if(input$date_format == ""){
            hideFeedback("date_format")
            showFeedbackDanger(inputId = "date_format", text = paste("Required"))
            date_format_valid <- FALSE
          } else if(isFALSE(movenet:::is_valid_date_format(input$date_format))){
            hideFeedback("date_format")
            showFeedbackDanger(inputId = "date_format",
                               text = paste("Must be a valid readr date format"))
            date_format_valid <- FALSE
          } else {
            hideFeedback("date_format"); date_format_valid <- TRUE
            movenetenv$options$movedata_fileopts$date_format <- input$date_format}
        }

        if(id == "holding"){
          movenetenv$options$holdingdata_fileopts$country_code <- NULL
          if(input$country_code == "" &&
             any(!(c(input$coord_x, input$coord_y) %in% selectInputs_empty))){
            hideFeedback("country_code")
            showFeedbackDanger(inputId = "country_code",
                               text = paste("Required for inclusion of geographical coordinates"))
            country_code_valid <- FALSE
          } else if(input$country_code != "" &&
                    isFALSE(movenet:::is_valid_country_code(input$country_code))){
            showFeedbackDanger(inputId = "country_code",
                               text = paste("Must be two uppercase letters"))
            country_code_valid <- FALSE
          } else {
            hideFeedback("country_code"); country_code_valid <- TRUE
            if(input$country_code != ""){
              movenetenv$options$holdingdata_fileopts$country_code <- input$country_code}}

          movenetenv$options$holdingdata_fileopts$coord_EPSG_code <- NULL
          if(input$coord_EPSG_code == "" &&
             any(!(c(input$coord_x, input$coord_y) %in% selectInputs_empty))){
            hideFeedback("coord_EPSG_code")
            showFeedbackDanger(inputId = "coord_EPSG_code",
                               text = paste("Required for inclusion of geographical coordinates"))
            coord_EPSG_code_valid <- FALSE
          } else if(input$coord_EPSG_code != "" &&
                    isFALSE(test_integerish(input$coord_EPSG_code))){
            showFeedbackDanger(inputId = "coord_EPSG_code",
                               text = paste("Must be an integer"))
            coord_EPSG_code_valid <- FALSE
          } else {
            hideFeedback("coord_EPSG_code"); coord_EPSG_code_valid <- TRUE
            if(input$coord_EPSG_code != ""){
              movenetenv$options$holdingdata_fileopts$coord_EPSG_code <- input$coord_EPSG_code}}
        }

        #Validate columns and save as configs
        if(id == "movement"){
          if(input$from %in% selectInputs_empty){
            showFeedbackDanger(inputId = "from", text = paste("Required"))
            from_valid <- FALSE
          } else {
            hideFeedback("from"); from_valid <- TRUE
            movenetenv$options$movedata_cols$from <- input$from }

          if(input$to %in% selectInputs_empty){
            showFeedbackDanger(inputId = "to", text = paste("Required"))
            to_valid <- FALSE
          } else {
            hideFeedback("to"); to_valid <- TRUE
            movenetenv$options$movedata_cols$to <- input$to }

          if(input$weight %in% selectInputs_empty){
            showFeedbackDanger(inputId = "weight", text = paste("Required"))
            weight_valid <- FALSE
          } else {
            hideFeedback("weight"); weight_valid <- TRUE
            movenetenv$options$movedata_cols$weight <- input$weight }

          if(input$date %in% selectInputs_empty){
            showFeedbackDanger(inputId = "date", text = paste("Required"))
            date_valid <- FALSE
          } else {
            hideFeedback("date"); date_valid <- TRUE
            movenetenv$options$movedata_cols$date <- input$date }

          movenetenv$options$movedata_cols[
            which(!(names(movenetenv$options$movedata_cols) %in% select_input_names()))] <- NULL
          if(!is.null(input$additional_cols) && !(input$additional_cols %in% selectInputs_empty)){
            mapply(function(x){
              movenetenv$options$movedata_cols[[x]] <- x},
              input$additional_cols)
          }
        }

        if(id == "holding"){
          if(input$id %in% selectInputs_empty){
            showFeedbackDanger(inputId = "id", text = paste("Required"))
            id_valid <- FALSE
          } else {
            hideFeedback("id"); id_valid <- TRUE
            movenetenv$options$holdingdata_cols$id <- input$id }

          movenetenv$options$holdingdata_cols$coord_x <- NULL
          if(input$coord_x %in% selectInputs_empty &&
             !(input$coord_y %in% selectInputs_empty)){
            showFeedbackDanger(inputId = "coord_x",
                               text =
                                 paste("Required for inclusion of geographical coordinates"))
            coord_x_valid <- FALSE
          } else {
            hideFeedback("coord_x"); coord_x_valid <- TRUE
            if(!(input$coord_x %in% selectInputs_empty)){
              movenetenv$options$holdingdata_cols$coord_x <- input$coord_x }}

          movenetenv$options$holdingdata_cols$coord_y <- NULL
          if(input$coord_y %in% selectInputs_empty &&
             !(input$coord_x %in% selectInputs_empty)){
            showFeedbackDanger(inputId = "coord_y",
                               text =
                                 paste("Required for inclusion of geographical coordinates"))
            coord_y_valid <- FALSE
          } else {
            hideFeedback("coord_y"); coord_y_valid <- TRUE
            if(!(input$coord_y %in% selectInputs_empty)){
              movenetenv$options$holdingdata_cols$coord_y <- input$coord_y }}


          movenetenv$options$holdingdata_cols[
            which(!(names(movenetenv$options$holdingdata_cols) %in% select_input_names()))] <- NULL
          if(!is.null(input$additional_cols) && !(input$additional_cols %in% selectInputs_empty)){
            mapply(function(x){
              movenetenv$options$holdingdata_cols[[x]] <- x},
              input$additional_cols)
          }
        }

        #Test for and highlight any duplicate columns
        inputs_to_test_for_duplicates <-
          lapply(select_input_names(), function(x) input[[x]])
        duplicates <- inputs_to_test_for_duplicates[anyDuplicated(inputs_to_test_for_duplicates)]
        mapply(function(x){
          feedbackDanger(inputId = x,
                         show = (input[[x]] %in% duplicates || input[[x]] %in% input$additional_cols),
                         text = paste("Duplicate. Each column can only be selected once"))},
          select_input_names())

        dupl_additional_col <- any(input$additional_cols %in% inputs_to_test_for_duplicates)
        feedbackDanger(inputId = "additional_cols",
                       show = dupl_additional_col,
                       text = paste("Contains duplicate. Each column can only be selected once"))

        # Make "Extract" button active only when all inputs are valid
        if(id == "movement"){
          validity_criteria <- c(encoding_valid, separator_valid, decimal_valid,
                                 date_format_valid, from_valid, to_valid,
                                 date_valid, weight_valid,
                                 length(duplicates) == 0,
                                 isFALSE(dupl_additional_col))
        } else if(id == "holding") {
          validity_criteria <- c(encoding_valid, separator_valid, decimal_valid,
                                 country_code_valid, coord_EPSG_code_valid, id_valid,
                                 coord_x_valid, coord_y_valid,
                                 length(duplicates) == 0,
                                 isFALSE(dupl_additional_col))
        }
        updateProgressBar(session, "submit_config_pb", value = 100,
                          title = "The provided configurations have been saved.")
        shinyjs::toggleState(id = "extract", condition = all(validity_criteria))
    })

# Use config file ---------------------------------------------------------

      observeEvent(input$config_file, {

        # If no file is selected, don't do anything
        req(input$config_file)

        # Validate config file
        failed_validation_messages <-
          movenet:::internal_validate_config(input$config_file$datapath, config_type = id)
        if (!is.null(failed_validation_messages)){
          showModal(modalDialog(title = "config file validation error",
                                HTML("The uploaded file is not a valid config file:<ul><li>",
                                paste0(failed_validation_messages, collapse = "</li><li>"),"</li></ul>"),
                                easyClose = TRUE))
          showFeedbackDanger(inputId = "config_file",
                             text = paste("Invalid",id,"config file, please upload another file."))
        } else {
          hideFeedback("config_file")

          # Load config into movenetenv
          load_config(input$config_file$datapath)

          # Update inputs
          fileopts <- switch(id,
                             movement = movenetenv$options$movedata_fileopts,
                             holding = movenetenv$options$holdingdata_fileopts)
          cols <- switch(id,
                         movement = movenetenv$options$movedata_cols,
                         holding = movenetenv$options$holdingdata_cols)

          sapply(text_input_names()
                 [which(text_input_names() %in% names(fileopts))],
                 function(x){updateTextInput(session, x,
                                             value = fileopts[[x]])})

          if (movenetenv$options$movedata_fileopts$date_format == ""){
            updateTextInput(session, "date_format", value = "%AD")
          }

          sapply(select_input_names()
                 [which(select_input_names() %in% names(cols))],
                 function(x){updateSelectInput(session, x,
                                               choices = cols[[x]],
                                               selected = cols[[x]])})
          additional_col_names <-
            unname(cols[which(!(names(cols) %in% select_input_names()))])
          updateSelectInput(session, "additional_cols",
                            choices = additional_col_names,
                            selected = additional_col_names)

          shinyjs::toggleState(id = "extract", condition = !is.null(input$data_file))
          output$config_file_progress <- renderUI({
            p("The provided configurations have been saved.")
          })

          }
      })

# Populate column header selectInputs -------------------------------------

      observeEvent(input$read_headers, {

        dataframe <-
          read_delim(
            input$data_file$datapath,
            delim = input$separator,
            locale = locale(decimal_mark = input$decimal,
                            encoding = input$encoding),
            n_max = 1,
            name_repair = movenet::asciify,
            show_col_types = FALSE,
            lazy = TRUE
          )

        sapply(c(select_input_names(),"additional_cols"),
               function(x){
                 updateSelectInput(session, x,
                                   choices =
                                     c("Select appropriate column header",
                                       colnames(dataframe)))})
      })

# Extract & reformat data -------------------------------------------------
      observeEvent(input$extract, {

        updateProgressBar(session, "extract_pb", value = 0,
                          title = "Extracting and reformatting data...")

        dataframe <- reformat_data(input$data_file$datapath, type = id)

        # Update the input_data reactiveValues to contain the extracted dataframe
        input_data$original <- dataframe

        updateProgressBar(session, "extract_pb", value = 100, title = "Done!")
      })

      # View the extracted data upon request via actionButton
      observeEvent(input$view_extracted_data, {
        output$datatable <- renderDataTable({input_data$original})
      })

      # Pass the extracted data to the main app
      return(input_data)

    }
  )
}
