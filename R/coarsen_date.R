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
             floor_date(date,
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
        group_by(from, to, date) |>
        summarise(summed_weight = sum(weight),  ...) |>
        ungroup() |>
        rename(weight = summed_weight)
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
        group_by(from, to, date) |>
        summarise(...) |>
        ungroup()

      return(aggregated_data)
    }

    ###############################################
    ### Returning jittered data, if no rounding ###
    ###############################################

  } else return(data)
}
