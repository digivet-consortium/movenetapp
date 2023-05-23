###################################
### Data Input - User interface ###
###################################

dataInputUI <- function(id) {
  ns <- NS(id) # `NS(id)` returns a namespace function
  tagList(

# Data file upload --------------------------------------------------------

    h3("Upload", id, "data file"),
    p("Here, you can upload a livestock movement data file (in csv format), that
    you would like to make non-identifiable, or explore as a network."),
    fileInput(ns("data_file"), label = NULL,
              accept = c("text/csv", "text/comma-separated-values",
                         "text/plain", ".csv")),

# Configurations ----------------------------------------------------------

    h3("Configurations"),
    p("Please provide some configurations relating to your movement data file,
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
             fileInput(ns("config_file"), label = NULL,
                       accept = c("text/yaml", "text/yml", "text/x-yaml",
                                  "text/plain", "application/x-yaml", ".yml"))),
      column(4,
             style = "border-left: 1px solid",
             h4("File and data options"),
             textInput(ns("encoding"), "Encoding", value = "UTF-8"),
             textInput(ns("separator"), "Separator", value = ","),
             textInput(ns("decimal"), "Decimal mark", value = "."),
             create_data_options(id, ns)),
      column(4,
             splitLayout(
               h4("Data fields (column headers)"),
               actionButton(ns("read_headers"), "Populate*"),
               cellWidths = c("67%","33%")),
             p("*Requires uploaded data file and completed file options"),
             create_data_fields(id, ns))
    ),

# Extract & reformat data -------------------------------------------------

    actionButton(ns("extract"), "Extract and reformat relevant columns",
                 width = "100%"),
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
                                               "coord_EPSG", "country_code"))]})

      # Input names for all selectInputs (preserving order of data fields)
      select_input_names <- reactive({
        data_fields <- c("from","to","date","weight","id","coord_x","coord_y")
        data_fields[which(data_fields %in% names(input))]})

      # Reactive value to store input data in
      input_data <- reactiveValues()

# Use config file ---------------------------------------------------------

      config_file <- reactive({
        # If no file is selected, don't do anything
        validate(need(input$config_file, message = FALSE))
        input$config_file
      })

      observeEvent(config_file(), {
        # Read in yaml file (suppressW to avoid flatten outer name warning)
        config_list <-
          suppressWarnings(flatten(yaml.load_file(config_file()$datapath)))

        # Update inputs
        sapply(text_input_names()
               [which(text_input_names() %in% names(config_list))],
               function(x){updateTextInput(session, x,
                                           value = config_list[[x]])})
        sapply(select_input_names()
               [which(select_input_names() %in% names(config_list))],
               function(x){updateSelectInput(session, x,
                                             choices = config_list[[x]],
                                             selected = config_list[[x]])})
      })

# Populate column header selectInputs -------------------------------------

      observeEvent(input$read_headers, {

        dataframe <-
          read_delim(
            input$data_file$datapath,
            delim = input$separator,
            locale = locale(decimal_mark = input$decimal,
                            encoding = input$encoding,
                            asciify = TRUE), #check that same as in Movenet
            n_max = 1,
            show_col_types = FALSE,
            lazy = TRUE
          )

        sapply(select_input_names(),
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

        dataframe <-
          read_delim(
            input$data_file$datapath,
            delim = input$separator,
            col_select = sapply(select_input_names(), function(x) input[[x]]),
            locale = locale(decimal_mark = input$decimal,
                            encoding = input$encoding,
                            asciify = TRUE), #check that same as in Movenet
            col_types = cols(.default = col_character()),
            show_col_types = FALSE,
            lazy = TRUE) %>%
          type_convert(col_types = cols(date = col_date(),
                                        weight = col_double(),
                                        coord_x = col_double(),
                                        coord_y = col_double()),
                       locale = locale(decimal_mark = input$decimal,
                                       date_format =
                                         ifelse("date_format" %in% names(input),
                                                            input$date_format,
                                                            "%AD"),
                                       encoding = input$encoding))

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
