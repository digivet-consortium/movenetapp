#On progress bars: https://shiny.rstudio.com/articles/progress.html


######################################
### Anonymisation - User interface ###
######################################

anonymisationUI <- function(id) {
  ns <- NS(id) # `NS(id)` returns a namespace function
  tagList(

    # Add explanatory blurb

    fluidRow(
      column(6,
             h3("Rounding"),
             checkboxGroupInput(ns("rounding_units"),
                                label = "Unit to round dates down to",
                                choices = list("week", "month",
                                               "2 months" = "bimonth",
                                               "3 months" = "quarter",
                                               "6 months" = "halfyear", "year"),
                                selected =
                                  list("week","month","quarter","year"))),
      column(6,
             h3("Jitter"),
             checkboxGroupInput(ns("jitter_days"),
                                label = "Range of jitter (+/- n days) to apply
                                to dates",
                                choices = list(
                                  "4 (~ equivalent to range of 1 week)" = 4,
                                  "15 (~ equivalent to range of 1 month)" = 15,
                                  "30 (~ equivalent to range of 2 months)" = 30,
                                  "46 (~ equivalent to range of 3 months)" = 46,
                                  "91 (~ equivalent to range of 6 months)" = 91,
                                  "183 (~ equivalent to range of 1 year)" = 183),
                                selected = list(4, 15, 46, 183)),
             numericInput(ns("jitter_simulations"),
                          label =
                            "Number of jitter simulations to perform per range",
                          value = 5))),
  actionButton(ns("create_networks"), "Generate networks", width = "100%"),
  )
}

####################################
### Anonymisation - Server logic ###
####################################

anonymisationServer <- function(id, movement_data, holding_data) {
  moduleServer(
    id,
    function(input, output, session) {


# Change id's to numbers --------------------------------------------------


})}
