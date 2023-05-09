#library(lubridate)
#library(dplyr)
#library(rlang)

#' @import lubridate
#' @import dplyr
#' @importFrom rlang .data
#'

simplified_coarsen_date <- function(data, jitter,
                                    rounding_unit,
                                    week_start = getOption("lubridate.week.start", 7),
                                    sum_weight = TRUE, ...){

  # Removed all argument checks and config checks from the movenet version;
  # changed all references to movenetenv variables -> from/to/date/weight , as
  # in webapp the data cols were renamed

  dates <- data[["date"]]

  #####################
  ### Adding jitter ###
  #####################

  if (!isFALSE(jitter) | jitter != 0){

    jitter <- as.integer(jitter)

    replacement_data <-
      dates +
      sample(c(-jitter:jitter),
             length(dates),
             replace = TRUE)

    while (any(replacement_data < min(dates) | replacement_data > max(dates))){

      #resampling for dates beyond boundaries
      replacement_data[which(replacement_data < min(dates) |
                               replacement_data > max(dates))] <-
        dates[which(replacement_data < min(dates) |
                      replacement_data > max(dates))] +
        sample(c(-jitter:jitter),
               sum(any(replacement_data < min(dates) |
                         replacement_data > max(dates))),
               replace = TRUE)

    }

    data["date"] <- replacement_data
  }

  # Currently adds uniform ints between -jitter and +jitter , including 0 !


  ###########################
  ### Rounding down dates ###
  ###########################

  if (!isFALSE(rounding_unit)){
    # round down each date to the first date of the rounding_unit (e.g. month)
    rounded_data <-
      data |>
      mutate(date =
             floor_date(.data$date,
                        unit = rounding_unit,
                        week_start = week_start))

    #what to do to allow user to coarsen other date fields?
    #  build in column argument like for coarsen_weight?

    #Alternatives:
    # format_ISO8601(date_object, precision) from lubridate ->
    #      character vector matching ISO format to certain precision
    # format(date_object, format_string) from base R ->
    #      character vector matching format_string (e.g %Y-%m)
    # as.yearmon(date_object) from zoo ->
    #      yearmon object

    # Allow for aggregation over consistent periods of x number of days?
    # Lentz uses 1d, 7d, 14d, 28d (monthly), 84d (quarterly)


    ###########################
    ### Aggregating by date ###   (only if !isFALSE(rounding_unit))
    ###########################

    if (isTRUE(sum_weight)){

      aggregated_data <-
        rounded_data |>
        group_by(.data$from, .data$to, .data$date) |>
        summarise(summed_weight = sum(.data$weight),  ...) |>
        ungroup() |>
        rename(weight = .data$summed_weight)
      #using "summed_weight" and then renaming to the data-specific weight
      #variable, to avoid problems with additional weight summarising
      #functions. If the original name is kept, any additional functions are
      #performed on the summed weights rather than individual weights

      return(aggregated_data)

    } else if(...length() == 0){

      return(rounded_data)

    } else {

      aggregated_data <-
        rounded_data |>
        group_by(.data$from, .data$to, .data$date) |>
        summarise(...) |>
        ungroup()

      return(aggregated_data)
    }

    ###############################################
    ### Returning jittered data, if no rounding ###
    ###############################################

  } else return(data)
}

parallel_coarsen_date <- function(data, n_threads, jitter_set, rounding_set,
                                  week_start = getOption("lubridate.week.start", 7),
                                  sum_weight = TRUE, ...){

  jitter_reps <- c(jitter_set, rep(FALSE, length(rounding_set)))
  rounding_reps <- c(rep(list(FALSE),length(jitter_set)), rounding_set)

  cl <- makeCluster(n_threads)
  on.exit(stopCluster(cl))

  clusterExport(cl, c("data", "jitter_reps", "rounding_reps", "week_start",
                      "sum_weight", "floor_date", ":="), envir = environment())
  clusterEvalQ(cl, {
    library("dplyr")
    })

  modified_datasets <-
    parLapply(cl = cl, seq_along(jitter_reps),
              function(x){simplified_coarsen_date(
                data = data,
                jitter = jitter_reps[[x]],
                rounding_unit = rounding_reps[[x]],
                week_start = week_start,
                sum_weight = sum_weight,
                ...)})

  jitter_names <-
    if(length(jitter_set) == 0){NULL
      } else {paste0("Jittered_", jitter_set,"days")}

  rounded_names <-
    if(length(rounding_set) == 0){NULL
      } else {paste0("Rounded_", gsub(" ", "", rounding_set), "WS", week_start)}
  #Names miss reference to summary functions - would get too complicated

  names(modified_datasets) <-
    make.unique(paste0("Dates", c(jitter_names, rounded_names)), sep="_")

  return(modified_datasets)
}

#' @importFrom plyr round_any
#' @importFrom purrr has_element
#' @importFrom stats runif
#' @export
simplified_coarsen_weight <- function(data, column, jitter, round){

  #####################
  ### Adding jitter ###
  #####################

  if (!isFALSE(jitter) | jitter != 0){

    replacement_data <-
      data[[column]] +
      runif(length(data[[column]]), -jitter, +jitter)

    while (any(replacement_data <= 0)){ #resampling if result of jitter is <= 0

      replacement_data[which(replacement_data <= 0)] <-
        data[[column]][which(replacement_data <= 0)] +
        runif(sum(replacement_data <= 0), -jitter, +jitter)

    }

    data[column] <- replacement_data
  }

  # Most suitable distribution? Currently uniform, but can change


  ################
  ### Rounding ###
  ################

  if (!isFALSE(round) | round != 0){

    data[column] <- round_any(data[[column]], accuracy = round)

    data[column][which(data[column] < round),] <- round #set round as minimum

    #is it more efficient to filter first & only round if data[column] > round,
    #or to just round everything?
  }

  return(data)
}

## N.B. The below doesnt work - in part because function has not actually
##      been parallelised for multiple datasets!!
parallel_coarsen_weight <- function(data, n_threads, jitter_set, rounding_set){

  jitter_reps <- c(jitter_set, rep(FALSE, length(rounding_set)))
  rounding_reps <- c(rep(FALSE, length(jitter_set)), rounding_set)

  cl <- makeCluster(n_threads)
  on.exit(stopCluster(cl))

  clusterExport(cl, c("data", "jitter_reps", "rounding_reps"),
                envir = environment())
  clusterEvalQ(cl, {
    library("dplyr")
  })

  modified_datasets <-
    parLapply(cl = cl, seq_along(jitter_reps),
              function(x){simplified_coarsen_weight(
                data = data,
                column = "weight",
                jitter = jitter_reps[[x]],
                round = rounding_reps[[x]])})

  jitter_names <-
    if(length(jitter_set) == 0){NULL
    } else {paste0("Jittered_", jitter_set)}

  rounded_names <-
    if(length(rounding_set) == 0){NULL
    } else {paste0("Rounded_", rounding_set)}

  names(modified_datasets) <-
    make.unique(paste0("Weights", c(jitter_names, rounded_names)), sep="_")

  return(modified_datasets)
}
