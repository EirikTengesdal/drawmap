#' Merge \code{sf} objects from a specified folder
#'
#' \code{merge_sf()}: Read all Geopackage files in a specified folder and
#' combine them into a single \code{sf} object. It is tailored for importing
#' downloaded \href{https://gadm.org/}{GADM spatial data}.
#'
#' @param folder A character string specifying the folder containing the data
#' @param layer A character string specifying the layer to read from the
#'   Geopackage file
#' @param check_layers A logical value specifying whether to return the
#'   \code{sf::st_layers} output for each file in the folder
#' @param wgs84 A logical value specifying whether to transform the data to WGS
#'   84 (EPSG:4326) coördinate reference system
#' @param other_data An optional \code{sf} object to add to the final \code{sf}
#'   object
#' @param ... Additional arguments to pass to \code{sf::st_read()}
#'
#' @return An \code{sf} object
#' @export
#'
#' @examples
#' europe <- merge_sf(folder = here::here("data", "raw"))
merge_sf <- function(folder,
                     layer = "ADM_ADM_0",
                     check_layers = FALSE,
                     wgs84 = TRUE,
                     other_data = NULL,
                     ...) {
  files <- list.files(folder, pattern = ".gpkg", full.names = TRUE)

  if (check_layers) {
    return(lapply(files, sf::st_layers))
  }

  data <- lapply(files, function(file) {
    sf::st_read(file, layer = layer, ...)
  })

  if (wgs84) {
    data <- lapply(data, sf::st_transform, crs = "EPSG:4326")
  }

  data <- do.call(rbind, data)

  if (!is.null(other_data)) {
    data <- rbind(data, other_data)
  }

  return(data)
}
