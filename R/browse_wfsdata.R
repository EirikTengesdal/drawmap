#' Browse WFS data from URL using \code{OWS4R::WFSClient()}
#'
#' \code{browse_wfsdata()}: Browse WFS data from a URL using
#' \code{OWS4R::WFSClient()}.
#'
#' @param url URL to a service from which to retrieve data
#' @param service_version Version of the service; it might be necessary to check
#'   \code{serviceVersion} manually at time of request
#'
#' @return A data.frame (\code{pretty = TRUE}) containing available feature
#'   types
#' @export
#'
#' @seealso \code{load_wfsdata()}, \code{load_maps_data()}
browse_wfsdata <- function(url, service_version) {
  data_client <- ows4R::WFSClient$new(url, serviceVersion = service_version)

  return(data_client$getFeatureTypes(pretty = TRUE))
}
