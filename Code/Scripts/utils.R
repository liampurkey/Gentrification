#Readme: This file contains functions for my research on gentrification and the racial composition of employment

##Cleaning Functions

balance.panel = function(group_id, panel_length, data) {
    
  #The balance.panel function takes in a balanced panel where missing unit-time pairs are not explicit.
  #It returns a balanced panel by dropping units with missing time observations.
  
  out_data = data %>%
    dplyr::group_by(!!as.name(group_id)) %>%
    dplyr::filter(dplyr::n() == panel_length) %>%
    dplyr::ungroup()
    
  return(out_data)
  
}

##Spatial Functions

st.erase <- function(x, y) {
  sf::st_difference(x, sf::st_union(y))
}

clean.units <- function(x){
  attr(x,"units") <- NULL
  class(x) <- setdiff(class(x),"units")
  x
}

