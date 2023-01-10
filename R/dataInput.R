# Module UI function
dataInputUI <- function(id) {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  tagList(
    h3("Upload", id, "data file"),
    fileInput(ns("data_file"), label = NULL,
              accept = c("text/csv", "text/comma-separated-values",
                         "text/plain", ".csv")),
    h3("Configurations"),
    fluidRow(
      column(4,
             h4("Config file"),
             br(),
             p("Either upload an appropriate Movenet", id, "config file OR
               complete the configurations to the right"),
             br(),
             fileInput(ns("config_file"), label = NULL,
                       accept = c("text/yaml", "text/yml", "text/x-yaml",
                                  "text/plain", "application/x-yaml", ".yml"))),
      column(4,
             style = "border-left: 1px solid",
             h4("File and data options"),
             textInput(ns("encoding"), "Encoding",
                       value = "UTF-8"),
             textInput(ns("separator"), "Separator",
                       value = ","),
             textInput(ns("decimal"), "Decimal mark",
                       value = "."),
             create_data_options(id, ns)),
      column(4,
             splitLayout(
               h4("Data fields (column headers)"),
               actionButton(ns("read_headers"),
                            "Populate*"),
               cellWidths = c("67%","33%")),
             p("*Requires uploaded data file and completed file options"),
             create_data_fields(id, ns))
    ),
    fluidRow(tableOutput(ns("datatable")))

    #actionButton(ns("read_file"), "Read file",
    #             width = "100%"),

    #conditionalPanel(
    #  condition = "input.read_file > 0",
    #  actionButton(ns("show_head"), "Show first 3 rows",
    #               width = "49.5%"),
    #  actionButton(ns("show_full"), "Show full table",
    #               width = "49.5%"),
    #  #selectInput(),
    #  ns = ns)
    #conditionalPanel()

  )
}

dataInputServer <- function(id, stringsAsFactors = FALSE) {
  moduleServer(
    id,
    function(input, output, session) {

# Setting up some useful handles ------------------------------------------

      # Input names for all textInputs
      text_input_names <- reactive({
        names(input)[which(names(input) %in% c("encoding", "separator",
                                               "decimal", "date_format",
                                               "coord_EPSG", "country_code"))]})

      # Input names for all selectInputs
      select_input_names <- reactive({
        names(input)[which(names(input) %in% c("from", "to", "date", "weight",
                                               "id", "coord_x", "coord_y"))]})

# Use config file ---------------------------------------------------------

      # The selected file, if any
      config_file <- reactive({
        # If no file is selected, don't do anything
        validate(need(input$config_file, message = FALSE))
        input$config_file
      })

      observeEvent(config_file(), {
        # Read in yaml file
        config_list <- flatten(yaml.load_file(config_file()$datapath))

        # Update inputs
        sapply(text_input_names()[which(text_input_names() %in% names(config_list))],
               function(x){updateTextInput(session, x,
                                           value = config_list[[x]])})
        sapply(select_input_names()[which(select_input_names() %in% names(config_list))],
               function(x){updateSelectInput(session, x,
                                             choices = config_list[[x]],
                                             selected = config_list[[x]])})
      })


# Populate column headers -------------------------------------------------

      observeEvent(input$read_headers, {

        dataframe <-
          read_delim(
            input$data_file$datapath,
            lazy = TRUE,
            delim = input$separator,
            locale = locale(decimal_mark = input$decimal,
                            #date_format = input$date_format, #if found in inputs, else set to "%AD" !!
                            encoding = input$encoding),
            #col_types = cols(.default = col_character()), #only if checking data properties
            #name_repair = asciify) #copy from movenet
            show_col_types = FALSE
          )

        sapply(select_input_names,
               function(x){
                 updateSelectInput(session, x,
                                   choices = c("Select appropriate column header",
                                               colnames(dataframe)))})
      })

    }
  )
}
