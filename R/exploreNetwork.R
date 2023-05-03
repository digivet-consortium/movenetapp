library(network)
library(networkDynamic)
library(tsna)

########################################
### Explore network - User interface ###
########################################

exploreNetworkUI <- function(id) {
  ns <- NS(id) # `NS(id)` returns a namespace function
  tagList(
    h3("Explore a movement network"),
    selectInput(ns("network"), "Select network to explore",
                choices = NULL),

# Basic measures ----------------------------------------------------------
    h3("Basic measures"),
    p("Network size (nr of holdings active during time covered by data):"),
    textOutput(ns("network_size")),
    p("Network density:")
  )
}

######################################
### Explore network - Server logic ###
######################################

exploreNetworkServer <- function(id, networks, n_threads){
  moduleServer(
    id,
    function(input, output, session) {

      all_networks <-
        reactive({reactiveValuesToList(networks)[order(names(networks))]})

      observe({
        updateSelectInput(session, "network",
                          choices = names(all_networks()))})

      selected_network <- reactive({all_networks()[[input$network]]})

# Basic measures ----------------------------------------------------------
      output$network_size <- renderText({network.size(selected_network())})
})}

