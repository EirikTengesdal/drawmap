% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_maps_data.R
\name{load_maps_data}
\alias{load_maps_data}
\title{Load map data using \code{maps::map()}}
\usage{
load_maps_data(
  countries = c("Sweden", "Denmark", "Russia", "Estonia", "Latvia", "Lithuania",
    "Finland"),
  wgs84 = TRUE
)
}
\arguments{
\item{countries}{A character vector of countries for which to load data,
default is \code{c("Sweden", "Denmark", "Russia", "Estonia", "Latvia",
"Lithuania", "Finland")} intended for a map of Norway}

\item{wgs84}{Logical, whether to transform the data to WGS 84 (EPSG:4326),
default is \code{TRUE}}
}
\value{
An \code{sf} object
}
\description{
\code{load_maps_data()}: Wrapper function for \code{maps::map()} that loads
map data for a specified country or countries, optionally transforms it to
WGS 84 format, and returns an \code{sf} object.
}
\examples{
norway <- load_maps_data(countries = "Norway")
europe <- load_maps_data()
}
\seealso{
\code{browse_wfsdata()}, \code{load_wfsdata()}
}
