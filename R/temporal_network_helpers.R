#' @import rlang
#' @import checkmate
#' @import network
#' @import networkDynamic
#' @import tibble
#' @import tsna
#' @import dplyr
#' @importFrom magrittr %>%


parallel_max_reachabilities <- function(networks, n_threads){
  cl <- makeCluster(n_threads)
  on.exit(stopCluster(cl))

  clusterEvalQ(cl, {
    library("tsna")
  })

  max_reachabilities <-
    pbsapply(networks,
             function(x){max(tReach(x, graph.step.time = 1))}, cl=cl)
  return(max_reachabilities)
}

parallel_reachabilities <- function(networks, n_threads, direction = c("fwd","bkwd")){
  cl <- makeCluster(n_threads)
  on.exit(stopCluster(cl))

  clusterEvalQ(cl, {
    library("tsna")
  })

  reachabilities <-
    pblapply(networks,
             function(x){tReach(x, direction, graph.step.time = 1)}, cl=cl)
  return(reachabilities)
}


extract_periods <- function(data, period){
  if (isTRUE(grepl("(\\d+\\sdays)|((\\d+\\s)?week(\\s)?)",period))){
    start_dates <- seq(min(data), max(data), by = period)
  } else {
    start_dates <- seq(floor_date(min(data), period),
                       max(data),
                       by = period)
  }
  end_dates <- as.integer(start_dates + period(period))
  start_dates <- as.integer(start_dates)
  Map(c,start_dates,end_dates)
}


extract_periodic_subnetworks <- function(networks, n_threads, periods_in_data){

  cl <- makeCluster(n_threads)
  on.exit(stopCluster(cl))
  clusterExport(cl, c("network.extract","periods_in_data"), envir = environment())
  periodic_networks <-
    pblapply(networks,
              function(nw){
                lapply(periods_in_data, function(p) {
                  network.extract(nw, onset = p[[1]], terminus = p[[2]],
                                  rule = "any",
                                  #retain.all.vertices = TRUE,
                                  trim.spells = TRUE)})}, cl=cl)
  names(periodic_networks) <- names(networks)
  return(periodic_networks)
}



violinplot_monthly_measures <- function(monthly_data, measure_name){

  #Reformat to long tibble for plotting
  monthly_measures <-
    monthly_data %>%
    pivot_longer(everything(),
                 names_to = "network",
                 values_to = measure_name)

  #Set network as a factor with specific order (to avoid default alphabetic order)
  monthly_measures$network <-
    factor(monthly_measures$network,
           levels = unique(names(monthly_data)))

  p <-
    ggplot(data = monthly_measures,
           aes(x = network, y = .data[[measure_name]])) +
    xlab("Movement network") +
    ylab(paste("Monthly", measure_name)) +
    ylim(0, NA) +
    theme_bw() +
    scale_x_discrete(labels = function(x) str_wrap(as.character(x), width = 9))+
    geom_violin(trim = FALSE)

  p <- p + geom_boxplot(width = 0.1)

  plot(p)
}


plot_measure_over_anonymisation_gradient <-
  function(data, measure_name, anonymisation){
    datacols <- colnames(data)
    anon_amount <- colnames(data)[1]
    measure <- colnames(data)[2]
    p <-
      ggplot(data = data,
             aes(x = .data[[anon_amount]], y = .data[[measure]],
                 group = .data[[anon_amount]])) +
      theme_bw() +
      xlab(ifelse(anonymisation == "jitter", "Jitter (days)",
                  "Rounding unit equivalent (days)")) +
      ylab(measure_name) +
      ylim(0, NA)

    # if(anonymisation == "jitter"){
    #   p <- p + geom_boxplot()
    # } else {
      p <- p + geom_point()#}

    plot(p)
  }

