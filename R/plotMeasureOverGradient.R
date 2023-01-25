library(ggplot2)
library(stringr)
library(tidyr)

###############################################################
### Plot measure over uncertainty gradient - User interface ###
###############################################################

plotMeasureOverGradientUI <- function(id) {
  ns <- NS(id) # `NS(id)` returns a namespace function
  tagList(
    # Add explanatory blurb
    h3(paste("Overall network", id, "for various units of rounding")),
    plotOutput(ns("plot_rounding")),
    h3(paste("Overall network", id, "for various ranges of jitter")),
    plotOutput(ns("plot_jitter"))
  )
}

#############################################################
### Plot measure over uncertainty gradient - Server logic ###
#############################################################

plotMeasureOverGradientServer <- function(id, measures) {
  moduleServer(
    id,
    function(input, output, session) {

      output$plot_rounding <-
        renderPlot(
          plot_measure_over_anonymisation_gradient(
            data.frame(
              sapply(names(measures$max_reachability$rounded),
                     function(x){as.numeric(duration(x),"days")}),
              measures$max_reachability$rounded),
            id, "rounding"))

      output$plot_jitter <-
        renderPlot(
          plot_measure_over_anonymisation_gradient(
            data.frame(as.numeric(names(measures$max_reachability$jitter)),
                       measures$max_reachability$jitter),
            id, "jitter"))

    })}
