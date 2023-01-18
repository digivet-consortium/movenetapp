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
