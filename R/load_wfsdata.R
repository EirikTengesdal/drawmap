#' Load WFS data
#'
#' \code{load_wfsdata()}: Wrapper function for \code{get_map_data()} that
#' retrieves WFS data from a WFSClient URL, optionally transforms it to WGS 84
#' format, and optionally dissolves the data.
#'
#' @param url URL to a service from which to retrieve geospatial data
#' @param typename Name of the layer to retrieve from the service
#' @param wgs84 Logical indicating whether to transform the data to WGS 84
#'   format, default is \code{TRUE}
#' @param dissolve Logical indicating whether to dissolve the data, default is
#'   \code{TRUE}
#'
#' @return An \code{sf} object
#' @export
#'
#' @seealso [browse_wfsdata()], [get_map_data()]
#' @examples
#' load_wfsdata(
#' url = "https://geo.stat.fi/geoserver/tilastointialueet/wfs",
#' typename = "tilastointialueet:kunta1000k")
load_wfsdata <- function(url = NULL,
                         typename = NULL,
                         wgs84 = TRUE,
                         dissolve = TRUE) {
  data <- get_map_data(url = url, typename = typename)

  if (wgs84) {
    data <- sf::st_transform(x = data, crs = "EPSG:4326")
  }

  if (dissolve) {
    data <- rmapshaper::ms_dissolve(data)
  }

  return(data)
}
