#' Harmonise variables for subsequent merging of \code{sf} objects
#'
#' \code{harmonise_vars()}: Harmonise variables in an \code{sf} object for
#' subsequent merging with other \code{sf} object(-s) with compatible variable
#' names. This could be relevant for example when working with GADM map data and
#' merging with map data from other sources.
#'
#' @param data An \code{sf} object
#' @param rename_list A named list of variables to rename
#' @param new_var A new variable to create
#' @param new_val A value for the new variable
#' @param position Position of the new variable
#' @param mutate_col A variable to mutate
#' @param mutate_suffix A suffix to add to the mutated variable
#' @param replace_var A variable to replace
#' @param replace_val A value with which to replace the variable
#'
#' @return An \code{sf} object
#' @importFrom dplyr %>%
#' @importFrom rlang :=
#' @export
harmonise_vars <- function(data,
                           rename_list = NULL,
                           new_var = NULL,
                           new_val = NULL,
                           position = "after",
                           mutate_col = NULL,
                           mutate_suffix = NULL,
                           replace_var = NULL,
                           replace_val = NULL) {
  if (!is.null(rename_list)) {
    data <- data %>%
      dplyr::rename(!!!rename_list)
  }

  if (!is.null(new_var)) {
    data <- data %>%
      dplyr::mutate(!!new_var := new_val, .after = position)
  }

  if (!is.null(mutate_col)) {
    data <- data %>%
      dplyr::mutate({{mutate_col}} := paste0({{mutate_col}}, mutate_suffix))
  }

  if (!is.null(replace_var)) {
    data <- data %>%
      dplyr::mutate(!!replace_var := replace_val)
  }

  return(data)
}
