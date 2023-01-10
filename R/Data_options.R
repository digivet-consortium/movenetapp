create_data_options <- function(data_type, ns){
  if (data_type == "movement") {
    data_options <-
      textInput(ns("date_format"), "Date format",
                value = "%Y%m%d")
  } else if (data_type == "holding") {
    data_options <-
      tagList(
        textInput(ns("EPSG_code"), "EPSG code (numeric part)"),
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
        selectInput(ns("from"), "From (id of originating holding)",
                    choices = "No choices here yet"),
        selectInput(ns("to"), "To (id of destination holding)",
                    choices = "No choices here yet"),
        selectInput(ns("date"), "Date",
                    choices = "No choices here yet"),
        selectInput(ns("weight"), "Weight",
                    choices = "No choices here yet"),
      )
  } else if (data_type == "holding") {
    data_fields <-
      tagList(
        selectInput(ns("id"), "Holding id",
                    choices = "No choices here yet"),
        selectInput(ns("coord_x"), "x coordinate (easting or longitude)",
                    choices = "No choices here yet"),
        selectInput(ns("coord_y"), "y coordinate (northing or latitude)",
                    choices = "No choices here yet"),
      )
  } else {
    stop("data_type must be either 'movement' or 'holding'")
  }
  return(data_fields)
}

