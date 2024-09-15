#' `browse_owsdata`: Browse data from URL using `OWS4R::WFSClient`
#'
#' @param url URL to a service from which to retrieve data
#' @param service_version Version of the service; it might be necessary to check serviceVersion manually at time of request
#'
#' @return A data.frame (cf. `pretty = TRUE`) containing available feature types
#' @export
#'
#' @examples
#' finland <- browse_owsdata(url = "https://geo.stat.fi/geoserver/tilastointialueet/wfs",
#'                           service_version = "2.0.0")
#' Locate desired municipality layer (unspecified year = same year as at the time of request, i.e. 2024 today?): "title" == "Kunnat (1:1 000 000)" -> "name" == "tilastointialueet:kunta1000k"
browse_owsdata <- function(url, service_version) {
  data_client <- ows4R::WFSClient$new(url, serviceVersion = service_version)
  return(data_client$getFeatureTypes(pretty = TRUE))
}
