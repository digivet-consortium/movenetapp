#Copied over from movenet commit ea4c803 on 27/04/23, then edited for testing

  #Either set function up to run in interactive (Shiny) vs non-interactive mode,
  #or just set up an internal function to run in Shiny in combination with other
  #check in Shiny code. Better to have all associated checks in 1 place, but
  #"interactive mode" does capture more modes than shiny...?

  #How can be made clear which key is rightly associated with which data? need
  #some key identifier string (random, or very specific timestamp) to link key
  #and data?

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
