#Readme: This file contains functions for my research on gentrification and the racial composition of employment

##Cleaning Functions

normalize = function(data, variables, total, prefix = NULL, drop = FALSE) {
  
  #Normalize takes in a dataset, a set of variables to normalize, and a variable to normalize by,
  #and returns a dataset with normalized variables. The user has the option to provide their own
  #prefix with the prefix argument and can also drop the original variables with the drop argument.
  
  out_data = data 
  
  if (is.null(newname)) {
    
    for (i in 1:length(variables)) {
      
      var = variables[i]
      varname = paste("p", var, sep = "_")
      out_data = out_data %>% 
        dplyr::mutate(!!varname := !!as.name(var)/!!as.name(total)) 
      
      if (isTRUE(drop)) {
        out_data = out_data %>%
          dyplr::select(-!!as.name(var))
      }
      
    }
    
  } else {
    
    for (i in 1:length(variables)) {
      
      var = variables[i]
      varname = paste(prefix, var, sep = "_")
      out_data = out_data %>% 
        dplyr::mutate(!!varname := !!as.name(var)/!!as.name(total)) 
      
      if (isTRUE(drop)) {
        out_data = out_data %>%
          dplyr::select(-!!as.name(var))
      }
      
    }
    
  }
  
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

