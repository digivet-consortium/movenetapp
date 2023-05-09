#library(rlang)
#library(checkmate)
#library(network)
#library(networkDynamic)
#library(tibble)
#library(tsna)
#library(dplyr)
#library(magrittr)

#' @import rlang
#' @import checkmate
#' @import network
#' @import networkDynamic
#' @import tibble
#' @import tsna
#' @import dplyr
#' @import magrittr

movedata2networkDynamic <- function(movement_data, holding_data = NULL,
                                    incl_nonactive_holdings = FALSE){

  #######################
  ### Argument checks ###
  #######################

  assert_data_frame(movement_data, min.cols = 4, null.ok = FALSE)
  assert_character(movement_data[[1]], any.missing = FALSE)
  assert_character(movement_data[[2]], any.missing = FALSE)
  assert_date(movement_data[[3]], any.missing = FALSE)
  assert_numeric(movement_data[[4]], any.missing = TRUE)

  node_ids <- unique(c(movement_data[[1]],movement_data[[2]]))

  assert_data_frame(holding_data, min.cols = 1, null.ok = TRUE)
  if(!is.null(holding_data)){
    #check that holding_ids are unique - should this be done in "reformat_data"?!
    assert_character(holding_data[[1]], unique = TRUE, any.missing = FALSE)

    holding_ids <- holding_data[[1]]
  }

  assert_logical(incl_nonactive_holdings, len = 1, any.missing = FALSE,
                 null.ok = FALSE)

  ############################################################
  ### Ensure consistency between movement and holding data ###
  ############################################################

  if(!is.null(holding_data)){

    #If there are any holding ids present in movement_data, but missing from
    #holding_data, add these ids to holding_data with NAs for other columns
    missing_holding_ids <- !(node_ids %in% holding_ids)
    if(any(missing_holding_ids)){
      holding_data <-
        holding_data %>%
        add_row("{names(holding_data)[1]}" :=
                  node_ids[which(missing_holding_ids)])
      holding_ids <- holding_data[[1]]
    }

    #If there are any holding ids present in holding_data but missing from
    #movement_data, either delete these ids (if incl_nonactive_holdings == FALSE),
    #or split these to form a new additional_holding_data dataframe, to be
    #added as (non-active) vertices after network creation (if
    #incl_nonactive_holdings == TRUE)
    additional_holding_ids <- !(holding_ids %in% node_ids)
    if(any(additional_holding_ids)){
      if(isTRUE(incl_nonactive_holdings)){
        additional_holding_data <-
          holding_data %>%
          dplyr::filter(!(.data[[names(holding_data)[1]]] %in% node_ids))
      }
      holding_data <-
        holding_data %>%
        dplyr::filter(.data[[names(holding_data)[1]]] %in% node_ids)
    }
  }

  #############################################
  ### Ensure correct node identifier format ###
  #############################################

  #networkDynamic needs vertex.ids to be consecutive integers from 1 to n_nodes.
  #Check if holding ids are consecutive "integers" (in character format is ok),
  #and if this is not the case, renumber and save the key.
  #(The key is later used to generate a "true_id" vertex attribute, and to set
  #vertex.pids (persistent identifiers))

  #  if(!all(grepl("^\\d+$",node_ids)) ||
  #     !identical(sort(as.integer(node_ids)), 1:max(as.integer(node_ids)))){
  ##I think this test makes code slow so just removing it

  key <- generate_anonymisation_key(node_ids, prefix = "", n_start = 1)
  #using this instead of 'anonymise' avoids the loaded config file requirement

  movement_data[c(1,2)] <-
    lapply(c(1,2), function(x){unname(key[as.character(movement_data[[x]])])})

  if(!is.null(holding_data)){
    holding_data[1] <- unname(key[as.character(holding_data[[1]])])
  }
  #  }

  ########################################
  ### Reformat data and create network ###
  ########################################

  #Reformat data to the specific column order, and integer vertex.ids and dates,
  #required by networkDynamic. Then create the network.

  movement_data[1:3] <- movement_data[1:3] |> lapply(as.numeric)
  movement_data <-
    movement_data[,c(3,3,1,2,4:length(movement_data))] |>
    data.frame(stringsAsFactors = FALSE)

  net <- networkDynamic(edge.spells = movement_data, verbose = FALSE,
                        create.TEAs = TRUE,
                        edge.TEA.names = names(movement_data)[-c(1:4)])
  #Allow multiplex graphs? (Default = FALSE)
  #This may cause trouble with certain measures. Edge spells over time will
  #cover most cases, but what if multiple moves betw same farms on 1 day?
  #Allow loops? (Default = FALSE)

  #######################################
  ### Set node persistent identifiers ###
  #######################################

  #vertex.pids (persistent identifiers) are needed to reliably identify nodes
  #when extracting subnetworks, as vertex.ids are re-numbered to match the
  #network size (i.e. vertex.ids are always 1:n_nodes; whereas vertex.pids
  #remain the same throughout extractions and can be non-int/non-consecutive).

  #if have key, add names (original holding ids) as vertex attrib "true_id"
  # if(exists("key", where = environment(), inherits = FALSE)){
  set.vertex.attribute(net, 'true_id', names(key))
  warning(str_wrap("Node identifiers (vertex.id) have been changed to
    consecutive integers. Original identifiers have been set as persistent
    identifiers (vertex.pid) and can be identified for each node by running
    `get.vertex.pid(network_name, vertex.id(s))`."))
  # } else {
  # #else, set convert vertex.names (original holding ids if consecutive ints) to
  # #character and set these as vertex attrib "true_id" [for consistency]
  #   set.vertex.attribute(net, 'true_id',
  #                        as.character(get.vertex.attribute(net,'vertex.names')))
  # }
  #set true_id attribute as vertex.pid
  set.network.attribute(net,'vertex.pid','true_id')

  ###########################
  ### Set node attributes ###
  ###########################

  if(!is.null(holding_data)){
    holding_data <- holding_data[order(as.integer(holding_data[[1]])),]
    set.vertex.attribute(net, names(holding_data)[-1], holding_data[-1])
  }

  ###########################################
  ### Add any non-active nodes to network ###
  ###########################################

  #If incl_nonactive_holdings == TRUE, add any non-active holdings to network,
  #creating (automatic) new numeric identifiers, setting their original
  #identifiers as true_id vertex.attribute and as vertex.pids, and adding
  #other vertex.attributes from additional_holding_data

  if(exists("additional_holding_data", where = environment(),
            inherits = FALSE)){
    names(additional_holding_data)[1]<-"true_id"
    add.vertices(net, nv = sum(additional_holding_ids),
                 vattr = lapply(split(additional_holding_data,
                                      1:nrow(additional_holding_data)),
                                as.list),
                 vertex.pid = additional_holding_data[[1]])
    deactivate.vertices(net, v = c((nrow(holding_data)+1):network.size(net)))
  }

  ###################################################################
  ### Reconcile node activity with edge activity & Return network ###
  ###################################################################

  #set nodes to active only during edge spells
  reconcile.vertex.activity(net, mode = "match.to.edges")

  #return network w/ true_id and vertex.pid containing original holding ids in
  #character format
  return(net)
}

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
# N.B. This step is super-slow without parallel processing.
# See https://bookdown.org/rdpeng/rprogdatascience/parallel-computation.html#building-a-socket-cluster
# Using a socket cluster, as mclapply doesn't work on Windows

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
    monthly_data |>
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

    if(anonymisation == "jitter"){
      p <- p + geom_boxplot()
    } else {
      p <- p + geom_point()}

    plot(p)
  }

