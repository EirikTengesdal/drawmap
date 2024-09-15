#' Retrieve geospatial data via URL service
#'
#' \code{get_map_data()}: Retrieve data via URL service as an \code{sf} object.
#' Optionally choose to save a local copy of the data drawing on the
#' \code{sf::st_write()} function.
#'
#' @param url URL to a service from which to retrieve geospatial data
#' @param service \code{service} list attribute for \code{httr2::url_parse},
#'   default is \code{wfs}
#' @param version \code{version} list attribute for \code{httr2::url_parse},
#'   default is \code{NULL}
#' @param request \code{request} list attribute for \code{httr2::url_parse},
#'   default is \code{GetFeature}
#' @param typename \code{typename} list attribute for \code{httr2::url_parse},
#'   default is \code{NULL}
#' @param outname Name of an optional output file with \code{sf::st_write},
#'   default is \code{NULL}
#' @param append Logical, should the output layer be appended to an existing
#'   file (\code{append = TRUE}) or overwrite layer (\code{append = FALSE}),
#'   default is \code{FALSE}
#'
#' @return An \code{sf} object
#' @export
#'
#' @seealso \code{browse_wfsdata()}, \code{load_wfsdata()}
#' @examples
#' finland_municipalities <- get_map_data(
#' url = "https://geo.stat.fi/geoserver/tilastointialueet/wfs",
#' typename = "tilastointialueet:kunta1000k")
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

  if (!is.null(outname)) {
    sf::st_write(data, outname, append = append)
  }

  return(data)
}
