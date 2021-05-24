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
    dplyr::filter(!is.na(gentrify)) %>%
    dplyr::filter(ifelse(!!as.name(pre_var) >= quantile(!!as.name(pre_var), probs = pre_pctile), 1, 0) == 0) 
  
  return(out_data)
  
}

clean.zoning = function(zoning_var, overlay_var, lot_area_var, boro_code, id_var, data) {
  
  out_data = data %>%
    dplyr::select(tidyselect::all_of(c(id_var, zoning_var, overlay_var, lot_area_var))) %>% #Select variables
    dplyr::mutate(zoning = dplyr::case_when(grepl("R1|R2|R3|R4|R5", !!as.name(zoning_var)) & !grepl("/", !!as.name(zoning_var)) ~ "R1_R5", #Low density residential
                            grepl("R6|R7|R8|R9|R10", !!as.name(zoning_var)) & !grepl("/", !!as.name(zoning_var)) ~ "R6_R10", #High density residential
                            grepl("C1|C2", !!as.name(overlay_var)) ~ "C1_C2", #Low density commercial
                            grepl("C4", !!as.name(zoning_var)) ~ "C4", #High density commercial
                            grepl("/", !!as.name(zoning_var)) ~ "MR")) %>% #Manufacturing and residential
    dplyr::mutate(zoning = ifelse(is.na(zoning), "other", zoning)) %>% #Create other zoning districts variable
    dplyr::group_by(!!as.name(id_var)) %>%
    dplyr::mutate(total_area = sum(as.numeric(!!as.name(lot_area_var)))) %>% #Calculate total lot area in tract
    dplyr::group_by(!!as.name(id_var), zoning) %>%
    dplyr::summarise(zoning_area = sum(as.numeric(LotArea)/total_area)) %>% #Calculate share of tract area by aggregate zoning district
    dplyr::ungroup() %>%
    tidyr::spread(key = zoning, value = zoning_area) %>% #Create lot share variables for each aggregate zoning district
    replace(is.na(.), 0) %>%
    dplyr::rename(GEOID = !!as.name(id_var)) %>%
    dplyr::select(-other) %>%
    tract.to.geoid(boro_code = boro_code, tract_var = "GEOID", data = .) #Map tract numbers to geoids
  
  return(out_data)
  
}

clean.districts = function(lot_area_var, grad_var, boro_code, district_var, id_var, district_data, zoning_data) {
  
  out_data = zoning_data %>%
    dplyr::select(tidyselect::all_of(c(lot_area_var, district_var, id_var))) %>% #Select variables
    dplyr::filter(!is.na(!!as.name(district_var))) %>% #Drop lots missing district assignments
    dplyr::group_by(!!as.name(id_var)) %>%
    dplyr::mutate(total_area = sum(as.numeric(!!as.name(lot_area_var)))) %>% #Calculate total lot area in tract
    dplyr::group_by(!!as.name(id_var), !!as.name(district_var)) %>%
    dplyr::summarise(dist_area = sum(as.numeric(!!as.name(lot_area_var))/total_area)) %>% #Calculate share of tract area by school district
    dplyr::ungroup() %>%
    dplyr::inner_join(district_data, by = district_var) %>% #Join residential data with school district data
    dplyr::group_by(!!as.name(id_var)) %>% 
    dplyr::summarize(grad_rate = sum(!!as.name(grad_var)/100 * dist_area)) %>% #Caclulate weighted averge of graduation rates by tract
    tract.to.geoid(boro_code = boro_code, tract_var = "GEOID", data = .) #Map tract numbers to geoids
  
  return(out_data)
  
}
  
tract.to.geoid = function(boro_code, tract_var, data) {
  
  #Test that the given boro_code is valid
  if (!boro_code %in% c("005", "047", "061", "081", "085")) {
    
    stop(paste(boro_code, "is not valid", sep = " "), .call = FALSE)
    
  } 
  
  geoid_data = data %>%
    dplyr::mutate(!!as.name(tract_var) := as.character(!!as.name(tract_var)*100)) %>%
    dplyr::mutate(!!as.name(tract_var) := dplyr::case_when(stringr::str_length(!!as.name(tract_var)) == 3 ~ paste("36", boro_code, "000", !!as.name(tract_var), sep = ""),
                                                           stringr::str_length(!!as.name(tract_var)) == 4 ~ paste("36", boro_code, "00", !!as.name(tract_var), sep = ""),
                                                           stringr::str_length(!!as.name(tract_var)) == 5 ~ paste("36", boro_code, "0", !!as.name(tract_var), sep = ""),
                                                           stringr::str_length(!!as.name(tract_var)) == 6 ~ paste("36", boro_code, !!as.name(tract_var), sep = "")))
  
  #Test that tract.to.geoid returns valid geoid
  if (isFALSE(all(str_length(as.vector(as.matrix(geoid_data[,tract_var]))) == 11))) {
    
    stop("some geoids do not have length 11", .call = FALSE)
    
  } 
  
  return(geoid_data)
  
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





