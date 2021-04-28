#Readme: This file contains functions for my research on gentrification and the racial composition of employment

##Cleaning Functions

##Spatial Functions

st_erase <- function(x, y) {
  sf::st_difference(x, sf::st_union(y))
}
