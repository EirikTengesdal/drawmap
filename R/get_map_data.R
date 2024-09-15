#' `get_map_data`: Retrieve data via URL service
#'
#' @param url URL to a service from which to retrieve data
#' @param service `service` list attribute for `httr2::url_parse`, default is `wfs`
#' @param version `version` list attribute for `httr2::url_parse`, default is `NULL`
#' @param request `request` list attribute for `httr2::url_parse`, default is `GetFeature`
#' @param typename `typename` list attribute for `httr2::url_parse`, default is `NULL`
#' @param outname Name of an optional output file with `sf::st_write`, default is `NULL`
#' @param append Logical, should the output layer be appended to an existing file (`append = TRUE`) or overwrite layer (`append = FALSE`), default is `FALSE`
#'
#' @return An `sf` object
#' @export
#'
#' @examples
#' get_map_data(url = "https://geo.stat.fi/geoserver/tilastointialueet/wfs",
#'             typename = "tilastointialueet:kunta1000k",
#'             outname = "finland_municipalities.shp") -> finland_municipalities
get_map_data <- function(url,
                         service = "wfs",
                         version = NULL,
                         request = "GetFeature",
                         typename = NULL,
                         outname = NULL,
                         append = FALSE) {
  url <- httr2::url_parse(url)
  url$query <- list(
    service = service,
    version = version,
    request = request,
    typename = typename
  )
  request <- httr2::url_build(url)
  data <- sf::read_sf(request)

  if(!is.null(outname)) {
    sf::st_write(data, outname, append = append)
  }

  return(data)
}
