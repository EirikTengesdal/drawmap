#' Load map location coördinates from file
#'
#' \code{load_coordinates()}: Load a CSV file containing named coördinates and
#' return an \code{sf} object. It uses \code{utils::read.csv()} to read the
#' data. In the function definition, it is assumed that the CSV file contains a
#' column named "Coordinates_WGS84" that contains the coördinates in the format
#' "Latitude, Longitude". The function uses \code{tidyr::separate()} to split
#' the coördinates into two separate columns, "Latitude" and "Longitude". The
#' function also assumes that the coördinates are in the WGS 84 format, and it
#' uses \code{sf::st_as_sf()} to convert the data to an \code{sf} object with
#' the coördinates as the geometry column.
#'
#' @param data An optional data frame containing coördinates, default is
#'   \code{NULL}
#' @param path A list of character strings specifying the path to the CSV file,
#'   default is \code{here::here("data", "raw", "NDC_datapoints.csv")}
#' @param sep The separator used in the CSV file, default is \code{";"}
#' @param sep_col The name of the column containing the coördinates, default is
#'   \code{"Coordinates_WGS84"}
#' @param into A character vector of length 2 specifying the names of the
#'   columns, default is \code{c("Latitude", "Longitude")}
#' @param col_order A character vector of length 2 specifying the order of the
#'   columns, default is \code{c("Longitude", "Latitude")}
#' @param separate_sep The separator used to separate the coördinates into two
#'   columns, default is \code{","}
#' @param factor_cols A character vector of column names to convert to factors,
#'   default is \code{c("County", "Place")}
#' @param mutate_col The name of the column to mutate, default is \code{Type}
#' @param mutate_val The value to assign to the mutated column, default is
#'   \code{"Participant home town/self-defined dialect"}
#' @param mutate_after The name of the column after which to insert the mutated
#'   column, default is \code{"geometry"}
#'
#' @return An \code{sf} object with the coördinates as the geometry column
#' @importFrom dplyr %>%
#' @importFrom rlang :=
#' @export
load_coordinates <- function(
    data = NULL,
    path = list("data", "raw", "NDC_datapoints.csv"),
    sep = ";",
    sep_col = "Coordinates_WGS84",
    into = c("Latitude", "Longitude"),
    col_order = c("Longitude", "Latitude"),
    separate_sep = ",",
    factor_cols = c("County", "Place"),
    mutate_col = "Type",
    mutate_val = "Participant home town/self-defined dialect",
    mutate_after = "geometry") {
  if (!is.null(data)) {
    coordinates <- data
  } else {
    coordinates <- utils::read.csv(
      here::here(path),
      sep = sep
    )
  }
  coordinates <- tidyr::separate(
    coordinates,
    sep_col,
    into = into,
    sep = separate_sep
  )
  coordinates[col_order] <- lapply(coordinates[col_order], as.numeric)
  coordinates <- sf::st_as_sf(
    coordinates,
    coords = col_order,
    remove = FALSE,
    crs = 4326,
    agr = "constant"
  )

  coordinates <- coordinates %>%
    dplyr::mutate(dplyr::across({{factor_cols}}, as.factor))

  if (!is.null(mutate_col)) {
    coordinates <- coordinates %>%
      dplyr::mutate({{mutate_col}} := mutate_val, .after = mutate_after)
  }

  return(coordinates)
}
