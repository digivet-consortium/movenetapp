#On progress bars: https://shiny.rstudio.com/articles/progress.html


######################################
### Anonymisation - User interface ###
######################################

anonymisationUI <- function(id) {
  tagList(

    # Add explanatory blurb

    fluidRow(
      column(6,
             h3("Rounding"),
             checkboxGroupInput("rounding_units",
                                label = "Unit to round dates down to",
                                choices = list("week", "month",
                                               "2 months" = "bimonth",
                                               "3 months" = "quarter",
                                               "6 months" = "halfyear", "year"),
                                selected =
                                  list("week","month","quarter","year"))),
      column(6,
             h3("Jitter"),
             checkboxGroupInput("jitter_days",
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
             numericInput("jitter_simulations",
                          label =
                            "Number of simulations to perform per jitter range",
                          value = 5)))
  )
}
