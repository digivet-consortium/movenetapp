#library(network)
#library(networkDynamic)
#library(tsna)

#' @import network
#' @import networkDynamic
#' @import tsna
#' @importFrom sna component.dist
#' @importFrom tidyr unnest

########################################
### Explore network - User interface ###
########################################

exploreNetworkUI <- function(id) {
  ns <- NS(id) # `NS(id)` returns a namespace function
  tagList(
    h3("Explore a movement network"),
    p("Here, you can explore a selected network via basic data summaries and a
    range of static and temporal network analyses."),
    p("This page follows analyses and recreates some plots from Schulz J et al.
      (2017) PLOS ONE 12(6): e0179915.",
      tags$a(href="https://doi.org/10.1371/journal.pone.0179915",
             "https://doi.org/10.1371/journal.pone.0179915", target="_blank")),
    selectInput(ns("network"), "Select network to explore", choices = NULL),

# Data summary measures ---------------------------------------------------

    h3("Basic network summary"),
    h4("Network size"),
    p(tags$b("Overall network size:"), textOutput(ns("network_size"), inline = TRUE),
      "holdings active over the whole time period"),
    fluidRow(
      column(6, dataTableOutput(ns("size_summary"))),
      column(6, align = "center",
             radioGroupButtons(ns("size_n_or_prop"), label = NULL,
                               choices = c("Holdings active (n)" = "number",
                                           "Holdings active (proportion)" = "proportion"),
                               selected = "number"),
             plotOutput(ns("size_plot")))),
    br(),
    h4("Movements and edges (links)"),
    p(tags$b("Overall number of movements:"), textOutput(ns("n_moves"), inline = TRUE),
      "movements over the whole time period"),
    p(tags$b("Overall number of edges:"), textOutput(ns("edge_count"), inline = TRUE),
      "different links between holdings over the whole time period"),
    p(tags$b("Overall edge density:"), textOutput(ns("edge_density"), inline = TRUE),
      "of all possible links between holdings are observed over the whole time period"),
    fluidRow(
      column(6, dataTableOutput(ns("moves_summary"))),
      column(6, align = "center",
             radioGroupButtons(ns("moves_n_or_prop"), label = NULL,
                               choices = c("Movements (n)" = "moves_number",
                                           "Movements (prop.)" = "moves_proportion",
                                           "Edges (n)" = "edges_number",
                                           "Edges (prop.)" = "edges_proportion",
                                           "Density" = "edges_density"),
                               selected = "moves_number"),
             plotOutput(ns("moves_plot")))),
    br(),
    h4("Movement weights (batch sizes)"),
    fluidRow(
      column(6, dataTableOutput(ns("weights_summary"))),
      column(6, align = "center", plotOutput(ns("weights_plot")))),
    br(),

# Static network analysis (snapshots) -------------------------------------

    h3("Static network analysis (monthly snapshots)"),
    h4("Component analysis"),
    fluidRow(
      column(6, plotOutput(ns("GSCC_size_plot"))),
      column(6, tags$b("Section under development:"),
             p("Similar plots for giant in component (GIC) and giant out component
               (GOC) will be added shortly."))),
    br(),
    h4("Loyalty"),
    fluidRow(
      column(6, plotOutput(ns("common_links_heatmap"))),
      column(6, tags$b("Section under development"))),
    br(),

# Temporal network analysis -----------------------------------------------

    h3("Temporal network analysis"),

    h4("Reachability (in- & out-going contact chains)"),
    numericInput(ns("n_threads"), label = "Parallel processing: number of threads available",
                 min = 1, value = 4),
    actionButton(ns("calc_reachabilities"),
                 "Calculate reachabilities (slow)", width = "100%"),
    progressBar(ns("pb_reachabilities"), value = 0,
                display_pct = TRUE),
    fluidRow(
      column(6, plotOutput(ns("fwd_reachability_plot"))),
      column(6, plotOutput(ns("bkwd_reachability_plot"))))
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

# Setting up useful handles -----------------------------------------------

      selected_network <- reactive({all_networks()[[input$network]]})

      dates_data <- reactiveValues()
      size_data <- reactiveValues()
      moves_data <- reactiveValues()
      edges_data <- reactiveValues()
      component_size_data <- reactiveValues()

      observeEvent(selected_network(), ignoreInit = TRUE, {
        edge_spell_list <- get.edge.activity(selected_network(), as.spellList = TRUE)

        #Extracting dates for study periods & subperiods (7d, 14d, 28d, 84d, month, quarter)
        first_day <- min(edge_spell_list[[1]]) #date in int format
        last_day <- max(edge_spell_list[[1]]) #date in int format

        dates_data$daily_int <- c(first_day:last_day)
        dates_data$daily <- as_date(dates_data$daily_int)
        mapply(function(n_days){
          dates_data[[paste0(n_days,"days_int")]] <- seq(first_day, last_day, by = n_days)
          dates_data[[paste0(n_days,"days")]] <- as_date(dates_data[[paste0(n_days,"days_int")]])},
          c(7, 14, 28, 84))
        mapply(function(period){
          dates_data[[paste0(period,"ly")]] <-
            seq(floor_date(as_date(first_day), period), as_date(last_day), by = period)
          dates_data[[paste0(period,"ly_int")]] <-
            as.integer(dates_data[[paste0(period,"ly")]])},
          c("month", "quarter"))

# Data summary measures ---------------------------------------------------

        #Extracting network sizes for study periods & subperiods
        n_nodes <- network.size(selected_network())
        output$network_size <- renderText({n_nodes})

        size_data$daily <-
          sapply(dates_data$daily_int,
                 function(t){network.size.active(selected_network(), at = t)})
        mapply(function(n_days){size_data[[paste0(n_days,"days")]] <-
          sapply(dates_data[[paste0(n_days,"days_int")]], function(t){
            network.size.active(selected_network(), onset = t, length = n_days)})},
          c(7, 14, 28, 84))
        size_data$monthly <- sapply(dates_data$monthly_int, function(t){
          network.size.active(selected_network(), onset = t,
                              terminus = as.integer(as_date(t)+months(1)))})
        size_data$quarterly <- sapply(dates_data$quarterly_int, function(t){
          network.size.active(selected_network(), onset = t,
                              terminus = as.integer(as_date(t)+months(3)))})

        #Turning network sizes into proportions
        mapply(
          function(x){size_data[[paste0(x,"_prop")]] <- size_data[[x]]/n_nodes},
          c("daily","7days","14days","28days","84days","monthly","quarterly"))

        #Table with monthly network sizes and proportions
        output$size_summary <- renderDataTable({
          tibble(month = format(dates_data$monthly, "%b %Y"),
                 "holdings active (n)" = size_data$monthly,
                 "holdings active (prop.)" = num(size_data$monthly_prop,
                                                      digits = 3))})

        #Plotting network sizes
        output$size_plot <- renderPlot({
          if(input$size_n_or_prop == "number"){
            snapshots_diffperiods_dotplot1(
              data.frame(dates_data$daily, size_data$daily),
              data.frame(dates_data$"7days", size_data$"7days"),
              data.frame(dates_data$"14days", size_data$"14days"),
              data.frame(dates_data$"28days", size_data$"28days"),
              data.frame(dates_data$"84days", size_data$"84days"),
              "Holdings active during each time period",
              "Number of holdings")
          } else {
            snapshots_diffperiods_dotplot1(
              data.frame(dates_data$daily, size_data$daily_prop),
              data.frame(dates_data$"7days", size_data$"7days_prop"),
              data.frame(dates_data$"14days", size_data$"14days_prop"),
              data.frame(dates_data$"28days", size_data$"28days_prop"),
              data.frame(dates_data$"84days", size_data$"84days_prop"),
              "Holdings active during each time period",
              "Proportion of holdings")}})

        #Extracting numbers of movements for study periods & subperiods
        n_moves <- nrow(edge_spell_list)
        output$n_moves <- renderText({n_moves})

        moves_data$daily <-
          sapply(dates_data$daily_int, function(t){
            nrow(edge_spell_list[which(edge_spell_list$onset == t),])})
        mapply(function(n_days){moves_data[[paste0(n_days,"days")]] <-
          sapply(dates_data[[paste0(n_days,"days_int")]], function(t){
            nrow(edge_spell_list[which(between(edge_spell_list$onset, t, t+(n_days-1))),])})},
          c(7, 14, 28, 84))
        moves_data$monthly <- sapply(dates_data$monthly_int, function(t){
          nrow(edge_spell_list[
            which(between(edge_spell_list$onset, t, as.integer(as_date(t)+months(1))-1)),])})
        moves_data$quarterly <- sapply(dates_data$quarterly_int, function(t){
          nrow(edge_spell_list[
            which(between(edge_spell_list$onset, t, as.integer(as_date(t)+months(3))-1)),])})

        #Turning movement numbers into proportions
        mapply(
          function(x){moves_data[[paste0(x,"_prop")]] <- moves_data[[x]]/n_moves},
          c("daily","7days","14days","28days","84days","monthly","quarterly"))

        #Extracting numbers of edges for study periods & subperiods
        edge_count <- network.edgecount(selected_network())
        output$edge_count <- renderText({edge_count})

        edges_data$daily <- sapply(dates_data$daily_int, function(t){
            network.edgecount.active(selected_network(), at = t)})
        mapply(function(n_days){edges_data[[paste0(n_days,"days")]] <-
          sapply(dates_data[[paste0(n_days,"days_int")]], function(t){
            network.edgecount.active(selected_network(), onset = t, length = n_days)})},
          c(7, 14, 28, 84))
        edges_data$monthly <- sapply(dates_data$monthly_int, function(t){
          network.edgecount.active(selected_network(), onset = t,
                                   terminus = as.integer(as_date(t)+months(1)))})
        edges_data$quarterly <- sapply(dates_data$quarterly_int, function(t){
          network.edgecount.active(selected_network(), onset = t,
                                   terminus = as.integer(as_date(t)+months(3)))})

        #Turning edge numbers into proportions [of actual edge count]
        mapply(
          function(x){edges_data[[paste0(x,"_prop")]] <- edges_data[[x]]/edge_count},
          c("daily","7days","14days","28days","84days","monthly","quarterly"))

        #Turning edge numbers into densities (proportion of all *possible* edges)
        possible_edges <- n_nodes*(n_nodes-1)
        output$edge_density <- renderText({round(edge_count/possible_edges, digits = 3)})
        mapply(
          function(x){edges_data[[paste0(x,"_density")]] <- edges_data[[x]]/possible_edges},
          c("daily","7days","14days","28days","84days","monthly","quarterly"))

        #Table with monthly movements / edges and proportions
        output$moves_summary <- renderDataTable({
          tibble(month = format(dates_data$monthly, "%b %Y"),
                 "movements (n)" = moves_data$monthly,
                 "movements (prop.)" = num(moves_data$monthly_prop, digits = 3),
                 "edges (n)" = edges_data$monthly,
                 "edges (prop.)" = num(edges_data$monthly_prop, digits = 3),
                 "edge density (prop.)" = num(edges_data$monthly_density, digits = 3))})

        #Plotting movements / edges
        output$moves_plot <- renderPlot({
          if(input$moves_n_or_prop == "moves_number"){
            snapshots_diffperiods_dotplot1(
              data.frame(dates_data$daily, moves_data$daily),
              data.frame(dates_data$"7days", moves_data$"7days"),
              data.frame(dates_data$"14days", moves_data$"14days"),
              data.frame(dates_data$"28days", moves_data$"28days"),
              data.frame(dates_data$"84days", moves_data$"84days"),
              "Movements during each time period",
              "Number of movements")
          } else if(input$moves_n_or_prop == "moves_proportion") {
            snapshots_diffperiods_dotplot1(
              data.frame(dates_data$daily, moves_data$daily_prop),
              data.frame(dates_data$"7days", moves_data$"7days_prop"),
              data.frame(dates_data$"14days", moves_data$"14days_prop"),
              data.frame(dates_data$"28days", moves_data$"28days_prop"),
              data.frame(dates_data$"84days", moves_data$"84days_prop"),
              "Movements during each time period",
              "Proportion of movements")
          } else if(input$moves_n_or_prop == "edges_number"){
            snapshots_diffperiods_dotplot1(
              data.frame(dates_data$daily, edges_data$daily),
              data.frame(dates_data$"7days", edges_data$"7days"),
              data.frame(dates_data$"14days", edges_data$"14days"),
              data.frame(dates_data$"28days", edges_data$"28days"),
              data.frame(dates_data$"84days", edges_data$"84days"),
              "Edges active during each time period",
              "Number of edges")
          } else if(input$moves_n_or_prop == "edges_proportion") {
            snapshots_diffperiods_dotplot1(
              data.frame(dates_data$daily, edges_data$daily_prop),
              data.frame(dates_data$"7days", edges_data$"7days_prop"),
              data.frame(dates_data$"14days", edges_data$"14days_prop"),
              data.frame(dates_data$"28days", edges_data$"28days_prop"),
              data.frame(dates_data$"84days", edges_data$"84days_prop"),
              "Edges active during each time period",
              "Proportion of edges")
          } else if(input$moves_n_or_prop == "edges_density") {
            snapshots_diffperiods_dotplot1(
              data.frame(dates_data$daily, edges_data$daily_density),
              data.frame(dates_data$"7days", edges_data$"7days_density"),
              data.frame(dates_data$"14days", edges_data$"14days_density"),
              data.frame(dates_data$"28days", edges_data$"28days_density"),
              data.frame(dates_data$"84days", edges_data$"84days_density"),
              "Edge density during each time period",
              "Proportion of all possible edges")}})

        #Extracting monthly summary stats for movement weights
        monthly_weights_summary_stats <-
          lapply(dates_data$monthly_int, FUN = function(t) {
            sapply(get.edge.attribute.active(selected_network(), "weight", onset = t, #list of edges during period
                                             terminus = as.integer(as_date(t)+months(1)),
                                             return.tea = TRUE, require.active = TRUE),
                   function(x) x[[1]]) |> #extract edge weights
              unlist() |>
              na.omit()}) |>
          periodic_data2summary_stats_df(dates_data$monthly, "month")

        output$weights_summary <-
          renderDataTable({monthly_weights_summary_stats |>
              mutate(month = format(month, "%b %Y"))})

        #Plotting monthly summary stats for movement weights
        output$weights_plot <- renderPlot({
          snapshot_summary_stats_linechart(monthly_weights_summary_stats,
                                           "Development of movement weights over time",
                                           "Movement weight (batch size)")})

# Static network analysis (snapshots) -------------------------------------

        #Creating monthly snapshots  - these only keep 1st of repeated movements within a month
        monthly_snapshots <-
          lapply(dates_data$monthly_int, function(t){
            network.collapse(selected_network(), onset = t,
                             terminus = as.integer(as_date(t)+months(1)),
                             active.default = FALSE,
                             retain.all.vertices = TRUE)})
        monthly_adj_matrices <- lapply(monthly_snapshots, as.matrix)
        monthly_lists_of_moves <- lapply(monthly_adj_matrices, function(m){
          as.data.frame(which(m == 1, arr.ind = TRUE, useNames = FALSE))})

        #Component analysis

        #Calculating monthly GSCC sizes (proportion of active farms included in GSCC)
        component_size_data$GSCC_prop_monthly <-
          sapply(component.dist(monthly_snapshots, connected = "strong"),
                 function(x) max(x$csize))/size_data$monthly

        #Plotting monthly GSCC sizes
        output$GSCC_size_plot <- renderPlot({
          snapshot_barchart(
            data.frame(dates_data$monthly, component_size_data$GSCC_prop_monthly),
            "Size of giant strongly connected component (GSCC)",
            "Component size (proportion of active holdings)")})

        #Loyalty

        #Creating df with fractions of common directed links between all monthly networks
        month_combinations <-
          expand.grid(m1 = 1:length(dates_data$monthly), m2 = 1:length(dates_data$monthly))
        month_combinations$loyalty <-
          sapply(1:nrow(month_combinations), function(x){
            sum(do.call(paste, monthly_lists_of_moves[[month_combinations[x,1]]]) %in%
                  do.call(paste, monthly_lists_of_moves[[month_combinations[x,2]]])) /
              edges_data$monthly[[month_combinations[x,1]]]
          })
        month_combinations <- mutate(month_combinations,
                                     m1 = dates_data$monthly[m1],
                                     m2 = dates_data$monthly[m2])

        #Plotting heatmap with fractions of common directed links
        output$common_links_heatmap <-
          renderPlot({monthxmonth_heatmap(month_combinations,
                                          "Fraction of common directed links")})
      })

# Temporal network analysis -----------------------------------------------

      observeEvent(input$calc_reachabilities, {

        monthly_tempnw <-
          extract_periodic_subnetworks(
            list(selected_network()), n_threads(),
            Map(c,dates_data$monthly_int,as.integer(dates_data$monthly+months(1)))) |>
          unlist(recursive = FALSE)
        updateProgressBar(session, "pb_reachabilities",
                          value = 1, total = 3,
                          range_value = c(0, 3))
        fwd_reachability <-
          parallel_reachabilities(monthly_tempnw, n_threads(), direction = "fwd")
        updateProgressBar(session, "pb_reachabilities",
                          value = 2, total = 3,
                          range_value = c(0, 3))
        bkwd_reachability <-
          parallel_reachabilities(monthly_tempnw, n_threads(), direction = "bkwd")
        updateProgressBar(session, "pb_reachabilities",
                          value = 3, total = 3,
                          range_value = c(0, 3))

        output$fwd_reachability_plot <- renderPlot({ snapshot_boxplot(
          unnest(tibble(dates_data$monthly, fwd_reachability), fwd_reachability),
          "Forward reachable set (out-going contact chain) sizes per month",
          "Size")})
        output$bkwd_reachability_plot <- renderPlot({snapshot_boxplot(
          unnest(tibble(dates_data$monthly, bkwd_reachability), bkwd_reachability),
          "Backward reachable set (in-going contact chain) sizes per month",
          "Size")})

      })


})}

