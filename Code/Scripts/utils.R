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

create.gentrification = function(income_var, college_var, pre_var, income_pctile, college_pctile_lower, college_pctile_upper, pre_pctile, data) {
  
  out_data = data %>%
    dplyr::filter(ifelse(!!as.name(income_var) <= quantile(!!as.name(income_var), probs = income_pctile), 1, 0) == 1) %>%
    dplyr::mutate(gentrify = case_when(!!as.name(college_var) <= quantile(!!as.name(college_var), probs = college_pctile_lower) ~ 0,
                                       !!as.name(college_var) >= quantile(!!as.name(college_var), probs = college_pctile_upper) ~ 1)) %>%
    filter(!is.na(gentrify)) %>%
    filter(ifelse(!!as.name(pre_var) >= quantile(!!as.name(pre_var), probs = pre_pctile), 1, 0) == 0) 
  
  return(out_data)
  
}

##Output Functions


##Spatial Functions

st.erase <- function(x, y) {
  sf::st_difference(x, sf::st_union(y))
}

clean.units <- function(x){
  attr(x,"units") <- NULL
  class(x) <- setdiff(class(x),"units")
  x
}

aggregate_lines = function(tract, tracts_shp, id_var, lines_shp, line_var) {
  
  tract_shp = tracts_shp %>%
    dplyr::filter(!!as.name(id_var) == tract)
  
  nearby_lines = cbind(lines_shp, distance = clean.units(st_distance(lines_shp, tract_shp))*0.000621371) %>%
    dplyr::filter(distance <= 0.5) %>%
    sf::st_drop_geometry() %>%
    dplyr::distinct(!!as.name(line_var)) %>%
    dplyr::summarize(n_lines = n()) %>%
    mutate(GEOID = as.character(tract))
  
  return(nearby_lines)
  
}

aggregate_lanes = function(tract, tracts_shp, id_var, lanes_shp) {
  
  tract_shp = tracts_shp %>%
    dplyr::filter(!!as.name(id_var) == tract)
  
  tract_lanes = st_intersection(tract_shp, lanes_shp) %>%
    sf::st_drop_geometry() %>%
    dplyr::summarize(n_lanes = n()) %>%
    mutate(GEOID = as.character(tract))
  
  return(tract_lanes)
  
}

