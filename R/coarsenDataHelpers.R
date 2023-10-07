parallel_coarsen_dates <- function(data, n_threads, jitter_set, rounding_set,
                                   week_start, sum_weight){

  cl <- makeCluster(n_threads)
  on.exit(stopCluster(cl))

  clusterExport(cl, c("data", "jitter_set", "rounding_set", "week_start",
                      "sum_weight"), envir = environment())
  clusterExport(cl, "movenetenv", envir = parent.env(movenetenv))

  clusterEvalQ(cl, {
    library("dplyr")
    library("movenet")
    for(n in ls(movenetenv, all.names = TRUE)){ #copies movenetenv to movenet:::movenetenv
      assign(n, get(n, movenetenv), movenet:::movenetenv)
    }
    })

  jittered_datasets <- NULL
  rounded_datasets <- NULL
  dataname <- names(data)

  if(length(jitter_set) > 0){
    jittered_datasets <-
      parLapply(cl = cl, seq_along(jitter_set),
                function(x){jitter_dates(data = data[[names(data)]],
                                         range = jitter_set[[x]])})

    if(dataname == "original"){
      names(jittered_datasets) <-
        make.unique(paste0("DatesJittered_", jitter_set, "d"), sep="_")
    } else {
      names(jittered_datasets) <-
        make.unique(paste0(dataname, "_DatesJittered_", jitter_set, "d"), sep="_")
    }
  }

  if(length(rounding_set) > 0){
    rounded_datasets <-
      parLapply(cl = cl, seq_along(rounding_set),
                function(x){round_dates(data = data[[names(data)]],
                                        unit = rounding_set[[x]],
                                        week_start = week_start,
                                        sum_weight = sum_weight)})
    summed <- switch(sum_weight, "aggr")
    if(dataname == "original"){
      names(rounded_datasets) <-
        paste0("DatesRounded_", gsub(" ", "", rounding_set), "WS", week_start,
               summed)
    } else {
      names(rounded_datasets) <-
        paste0(dataname, "_DatesRounded_", gsub(" ", "", rounding_set), "WS", week_start,
               summed)
    }
  }

  return(c(jittered_datasets,rounded_datasets))
}

parallel_coarsen_weights <- function(data, n_threads, jitter_set, rounding_set,
                                     column){

  cl <- makeCluster(n_threads)
  on.exit(stopCluster(cl))

  clusterExport(cl, c("data", "jitter_set", "rounding_set"),
                envir = environment())
  clusterExport(cl, "movenetenv", envir = parent.env(movenetenv))

  clusterEvalQ(cl, {
    library("dplyr")
    library("movenet")
    for(n in ls(movenetenv, all.names = TRUE)){ #copies movenetenv to movenet:::movenetenv
      assign(n, get(n, movenetenv), movenet:::movenetenv)
    }
  })

  jittered_datasets <- NULL
  rounded_datasets <- NULL
  dataname <- names(data)

  if(length(jitter_set) > 0){
    jittered_datasets <-
      parLapply(cl = cl, seq_along(jitter_set),
                function(x){jitter_weights(data = data[[names(data)]],
                                           range = jitter_set[[x]],
                                           column = column)})
    if(dataname == "original"){
      names(jittered_datasets) <-
        make.unique(paste0("WeightsJittered_", jitter_set), sep="_")
    } else {
      names(jittered_datasets) <-
        make.unique(paste0(dataname, "_WeightsJittered_", jitter_set), sep="_")
    }
  }

  if(length(rounding_set) > 0){
    rounded_datasets <-
      parLapply(cl = cl, seq_along(rounding_set),
                function(x){round_weights(data = data[[names(data)]],
                                          unit = rounding_set[[x]],
                                          column = column)})
    if(dataname == "original"){
      names(rounded_datasets) <- paste0("WeightsRounded_", rounding_set)
    } else {
      names(rounded_datasets) <- paste0(dataname, "_WeightsRounded_", rounding_set)
    }
  }

  return(c(jittered_datasets, rounded_datasets))
}
