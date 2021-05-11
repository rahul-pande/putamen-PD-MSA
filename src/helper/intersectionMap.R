getIntersectionMap <- function(set_list){
  all_elements <- unique(unlist(set_list))
  
  intersections <- lapply(set_list, function(current_set){
    return(as.numeric(is.element(all_elements, current_set)))
  })
  
  df <- do.call(cbind, intersections)
  
  row.names(df) <- all_elements
  
  return(df)
}
