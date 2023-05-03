simplified_anonymise <- function(data = movement_data, prefix = ""){

  #Simplified from movenet anonymise() - currently for movement data only
  #This will need to be changed to allow for co-anonymisation of movement and
  #holding data - so that can work with geographical data
  #Ideally use anonymise() from movenet, but this currently does not work (we're
  #not using movenetenv in the app, so -> error). Simplify function in movenet?
  #Or keep using a simplified function in webapp only?

  col_to_anonymise <- c("from","to") #different for holding data
  unique_ids <- unique(unlist(data[col_to_anonymise]))
  key <- generate_anonymisation_key(unique_ids, prefix, n_start = 1)
  data[col_to_anonymise] <-
    lapply(col_to_anonymise, function(x){unname(key[data[[x]]])})
  return(data)
}

#helper literally copied across from movenet
generate_anonymisation_key <- function(ids, prefix, n_start){

  ids_in_random_order <-
    if(length(ids) == 1) ids else sample(ids, size=length(ids), replace=FALSE)

  key <-
    setNames(paste0(prefix,
                    seq(n_start, length.out = length(ids_in_random_order))),
             ids_in_random_order)

  return(key)
}


#Copied over from movenet commit ea4c803 on 27/04/23, then edited for testing
anonymise <- function(data, prefix, key = NULL){

  #Either set function up to run in interactive (Shiny) vs non-interactive mode,
  #or just set up an internal function to run in Shiny in combination with other
  #check in Shiny code. Better to have all associated checks in 1 place, but
  #"interactive mode" does capture more modes than shiny...?

  #How can be made clear which key is rightly associated with which data? need
  #some key identifier string (random, or very specific timestamp) to link key
  #and data?

  #Ensure new reactive/reactiveVal/... for anon data instead of modification in place?
  #Or need to be able to provide fully anonymised (irretrievable) mode within 1 app session?

  if(!interactive()){ #in Shiny mode, do not do arg checks and config checks (but need to provide replacements)

  #######################
  ### Argument checks ###
  #######################

    assert_data_frame(data)
    assert(
      check_names(names(data),
                  must.include = c(movenetenv$options$movedata_cols$from,
                                   movenetenv$options$movedata_cols$to)),
      check_names(names(data),
                  must.include = movenetenv$options$holdingdata_cols$id)
    )
    assert_string(prefix, null.ok = TRUE)
    assert_character(key, any.missing = FALSE, names = "unique", null.ok = TRUE,
                     unique = TRUE)

  #########################
  ### Config file check ###
  #########################

    if (has_element(names(movenetenv$options), "movedata_cols") &
        has_element(names(data), movenetenv$options$movedata_cols$from)){
      col_to_anonymise <- c(movenetenv$options$movedata_cols$from,
                            movenetenv$options$movedata_cols$to)
    } else if (has_element(names(movenetenv$options), "holdingdata_cols") &
               has_element(names(data), movenetenv$options$holdingdata_cols$id)){
      col_to_anonymise <- movenetenv$options$holdingdata_cols$id
    } else {
      stop(
        "The loaded config file and the type of data (movement or holding data)
      do not correspond. Please ensure the appropriate config file is loaded.")
    }
  }

  anon_data_and_key <-
    internal_anonymise(data, col_to_anonymise = col_to_anonymise,
                       prefix = prefix, key = key)

  return(anon_data_and_key)
}


############################################
### Main internal anonymisation function ###
############################################

internal_anonymise <- function(data, col_to_anonymise, prefix, key = NULL){

  ###########################
  ### Create / expand key ###
  ###########################

  unique_ids <- unique(unlist(data[col_to_anonymise]))

  if(is.null(key)){
    key <- generate_anonymisation_key(unique_ids, prefix, n_start = 1)
  }

  ids_not_in_key <- !(unique_ids %in% names(key))
  if(any(ids_not_in_key)){
    warning(
      "The key has been expanded to include identifiers that were not found in
       the (provided) original key")

    key <-
      c(key,
        generate_anonymisation_key(unique_ids[which(ids_not_in_key)],
                                   prefix,
                                   n_start = length(key)+1))
  }


  #############################
  ### Replace ids using key ###
  #############################

  data[col_to_anonymise] <-
    lapply(col_to_anonymise, function(x){unname(key[data[[x]]])})

  return(list(data = data,
              key = key))

}

#####################################
### Helper function to create key ###
#####################################
#' @importFrom stats setNames
generate_anonymisation_key <- function(ids, prefix, n_start){

  ids_in_random_order <-
    if(length(ids) == 1) ids else sample(ids, size=length(ids), replace=FALSE)

  key <-
    setNames(paste0(prefix,
                    seq(n_start, length.out = length(ids_in_random_order))),
             ids_in_random_order)

  return(key)
}
