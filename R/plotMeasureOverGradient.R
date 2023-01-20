###############################################################
### Plot measure over uncertainty gradient - User interface ###
###############################################################

plotMeasureOverGradientUI <- function(id) {
  ns <- NS(id) # `NS(id)` returns a namespace function
  tagList(
    # Add explanatory blurb
  )
}

#############################################################
### Plot measure over uncertainty gradient - Server logic ###
#############################################################

plotMeasureOverGradientServer <- function(id, measures) {
  moduleServer(
    id,
    function(input, output, session) {

    })}
