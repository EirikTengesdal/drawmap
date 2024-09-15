#' `harmonise_vars`: Harmonise variables for subsequent merging of `sf` objects
#'
#' @param data
#' @param rename_list
#' @param new_var
#' @param new_val
#' @param position
#' @param mutate_col
#' @param mutate_suffix
#' @param replace_var
#' @param replace_val
#'
#' @return An `sf` object whose variables have been harmonised for subsequent merging with other sf object(-s) with compatible variable names
#' @export
#'
#' @examples norway <- harmonise_vars(data = norway,
#'                                    rename_list = list("GID_0" = "rmapshaperid", "geom" = "geometry"),
#'                                    new_var = "COUNTRY",
#'                                    new_val = "Norway",
#'                                    position = "GID_0")
harmonise_vars <- function(data,
                           rename_list = NULL,
                           new_var = NULL,
                           new_val = NULL,
                           position = "after",
                           mutate_col = NULL,
                           mutate_suffix = NULL,
                           replace_var = NULL,
                           replace_val = NULL) {

  if(!is.null(rename_list)) {
    data <- data %>%
      dplyr::rename(!!!rename_list)
  }

  if(!is.null(new_var)) {
    data <- data %>%
      dplyr::mutate(!!new_var := new_val, .after = position)
  }

  if (!is.null(mutate_col)) {
    data <- data %>%
      dplyr::mutate({{mutate_col}} := paste0({{mutate_col}}, mutate_suffix))
  }

  if(!is.null(replace_var)) {
    data <- data %>%
      dplyr::mutate(!!replace_var := replace_val)
  }

  return(data)
}
