# library(ggplot2)
#' @import ggplot2


###########################################################################
### Violin plot comparing distribs of monthly measures - User interface ###
###########################################################################

plotMonthlyMeasureViolinplotUI <- function(id) {
  ns <- NS(id) # `NS(id)` returns a namespace function
  tagList(
    # Add explanatory blurb
    plotOutput(ns("plot")),
  )
}

##########################################################################
### Violin plot comparing distribs of monthly measures  - Server logic ###
##########################################################################

plotMonthlyMeasureViolinplotServer <- function(id, measures) {
  moduleServer(
    id,
    function(input, output, session) {

      output$plot <-
        renderPlot(
          violinplot_monthly_measures(
            data.frame(measures$monthly_max_reachability, check.names = FALSE),
            id))


    })}
