create_data_options <- function(data_type, ns){
  if (data_type == "movement") {
     data_options <-
     #  textInput(ns("date_format"), labelMandatory("Date format"), value = "%Y%m%d") #%>%
     #  shinyhelper::helper(type = "inline", title = "Date formats", content = c(
     #  "Movenetapp uses readr date formats, except that empty strings are not
     #  accepted. For a flexible YMD parser, use '%AD'. For further guidance, see
     #  <a href='https://readr.tidyverse.org/reference/parse_datetime.html'>
     #  readr documentation</a>."))
       div(
         class = "input-group",
         tags$span(
           style = "display: inline-block",
           textInput(ns("date_format"), labelMandatory("Date format"), value = "%Y%m%d")
         ),
         tags$span(
           style = "vertical-align: bottom;",
           actionButton(ns("date_format_helper"), "", icon = icon("question-circle"))
         )
       )
  } else if (data_type == "holding") {
    data_options <-
      tagList(
        textInput(ns("coord_EPSG_code"), "EPSG code (numeric part)"),
        textInput(ns("country_code"), "Country code (two-letter)"))
  } else {
    stop("data_type must be either 'movement' or 'holding'")
  }
  return(data_options)
}

create_data_fields <- function(data_type, ns){
  if (data_type == "movement") {
    data_fields <-
      tagList(
        selectInput(ns("from"), labelMandatory("From (id of originating holding)"),
                    choices = "Click 'Populate' to fill with column headers"),
        selectInput(ns("to"), labelMandatory("To (id of destination holding)"),
                    choices = "Click 'Populate' to fill with column headers"),
        selectInput(ns("date"), labelMandatory("Date"),
                    choices = "Click 'Populate' to fill with column headers"),
        selectInput(ns("weight"), labelMandatory("Weight"),
                    choices = "Click 'Populate' to fill with column headers"),
        selectInput(ns("additional_cols"), "Additional columns",
                    choices = "Click 'Populate' to fill with column headers",
                    multiple = TRUE)
      )
  } else if (data_type == "holding") {
    data_fields <-
      tagList(
        selectInput(ns("id"), labelMandatory("Holding id"),
                    choices = "No choices here yet"),
        selectInput(ns("coord_x"), "x coordinate (easting or longitude)",
                    choices = "No choices here yet"),
        selectInput(ns("coord_y"), "y coordinate (northing or latitude)",
                    choices = "No choices here yet"),
        selectInput(ns("additional_cols"), "Additional columns",
                    choices = "Click 'Populate' to fill with column headers",
                    multiple = TRUE)
      )
  } else {
    stop("data_type must be either 'movement' or 'holding'")
  }
  return(data_fields)
}


