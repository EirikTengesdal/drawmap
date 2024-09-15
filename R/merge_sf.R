#' `merge_sf`: Merge `sf` objects from a specified folder
#'
#' @param folder
#' @param layer
#' @param check_layers
#' @param wgs84
#' @param other_data
#' @param ...
#'
#' @return An `sf` object containing data from all Geopackage files in a specified folder. The function is intended to be used for the \href{https://gadm.org/}{GADM spatial data}.
#' @export
#'
#' @examples
#' europe <- merge_sf("data/raw")
merge_sf <- function(folder,
                     layer = "ADM_ADM_0",
                     check_layers = FALSE,
                     wgs84 = TRUE,
                     other_data = NULL,
                     ...){
  # Find all Geopackage files from GADM to read in a specified folder
  files <- list.files(folder, pattern = ".gpkg", full.names = TRUE)

  # If check_layers is TRUE: For each file in the list, run and return the `sf::st_layers` output
  if(check_layers){return(lapply(files, sf::st_layers))}

  # Read the files into separate `sf` objects
  data <- lapply(files, function(file){
    sf::st_read(file, layer = layer, ...)
  })

  # Transform to WGS84 if wgs84 is TRUE
  if(wgs84){data <- lapply(data, sf::st_transform, crs = "EPSG:4326")}

  # Combine the data into a single `sf` object
  data <- do.call(rbind, data)

  # Add any external sf object as an optional function argument and do this before combining the data into a single `sf` object (must be WGS 84)
  if(!is.null(other_data)){
    data <- rbind(data, other_data)
  }
  return(data)
}
