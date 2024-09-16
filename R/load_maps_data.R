#' Load map data using \code{maps::map()}
#'
#' \code{load_maps_data()}: Wrapper function for \code{maps::map()} that loads
#' map data for a specified country or countries, optionally transforms it to
#' WGS 84 format, and returns an \code{sf} object.
#'
#' @param countries A character vector of countries for which to load data,
#'   default is \code{c("Sweden", "Denmark", "Russia", "Estonia", "Latvia",
#'   "Lithuania", "Finland")} intended for a map of Norway
#' @param wgs84 Logical, whether to transform the data to WGS 84 (EPSG:4326),
#'   default is \code{TRUE}
#'
#' @return An \code{sf} object
#' @export
#'
#' @seealso \code{browse_wfsdata()}, \code{load_wfsdata()}
#' @examples
#' norway <- load_maps_data(countries = "Norway")
#' europe <- load_maps_data()
load_maps_data <- function(countries = c("Sweden",
                                         "Denmark",
                                         "Russia",
                                         "Estonia",
                                         "Latvia",
                                         "Lithuania",
                                         "Finland"),
                           wgs84 = TRUE) {
  datalist <- list()

  if (length(countries) == 1) {
    data <- maps::map(region = countries,
                      plot = FALSE,
                      fill = TRUE)
    if (wgs84) {
      data <- sf::st_as_sf(data)
      data <- sf::st_transform(x = data, crs = "EPSG:4326")
    }
    return(data)
  } else {
    for (country in countries) {
      country_data <- maps::map(region = country,
                                plot = FALSE,
                                fill = TRUE)
      if (wgs84) {
        country_data <- sf::st_as_sf(country_data)
        country_data <- sf::st_transform(x = country_data, crs = "EPSG:4326")
      }
      datalist[[country]] <- country_data
    }
  }
  data <- do.call(rbind, datalist)
  return(data)
}
