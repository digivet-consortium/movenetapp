#library(ggplot2)
#library(stringr)
#library(tidyr)

#' @import ggplot2
#' @import stringr


###############################################################
### Plot measure over uncertainty gradient - User interface ###
###############################################################

plotMeasureOverGradientUI <- function(id) {
  ns <- NS(id) # `NS(id)` returns a namespace function
  tagList(
    # Add explanatory blurb
    h3(paste("Comparison of", id, "across ranges of date modifications")),
    p(paste0("Here, you can explore the effect of date modifications on ", id,".")),
    p("This may assist you in deciding how much jitter, or which rounding unit,
      would be appropriate to apply to your dataset."),
    h4(paste("Overall", id, "for various ranges of jitter")),
    plotOutput(ns("plot_jitter")),
    h4(paste("Overall", id, "for various units of rounding")),
    plotOutput(ns("plot_rounding"))
  )
}

#############################################################
### Plot measure over uncertainty gradient - Server logic ###
#############################################################

plotMeasureOverGradientServer <- function(id, measures) {
  moduleServer(
    id,
    function(input, output, session) {

      #Create useful data handles
      non_pseudonymised_data_names <- reactive({
        names(measures$max_reachability)[which(!str_detect(names(measures$max_reachability),fixed("Pseudonymised")))]})


      jittered_data_names <- reactive({
        non_pseudonymised_data_names()[str_which(non_pseudonymised_data_names(), fixed("DatesJittered"))]})
      jittered_data_days <- reactive({
        as.numeric(str_extract(jittered_data_names(), pattern = "\\d+"))})

      rounded_data_names <- reactive({
        non_pseudonymised_data_names()[str_which(non_pseudonymised_data_names(), fixed("DatesRounded"))]})
      rounded_data_days <- reactive({
        as.numeric(duration(str_match(rounded_data_names(), pattern = "_([a-zA-Z0-9]*)WS")[,2]), "days")})

      output$plot_jitter <-
        renderPlot(
          plot_measure_over_anonymisation_gradient(
            data.frame(c(0, jittered_data_days()), #data.frame with measure values for original data & jitter ranges
                       c(measures$max_reachability["original"],
                         measures$max_reachability[jittered_data_names()])),
            id, "jitter"))

      output$plot_rounding <-
        renderPlot(
          plot_measure_over_anonymisation_gradient(
            data.frame(c(0, rounded_data_days()), #data.frame with measure values for original data & rounding units (equiv in days)
                         c(measures$max_reachability["original"],
                           measures$max_reachability[rounded_data_names()])),
            id, "rounding"))

    })}
