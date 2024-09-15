#' Specify the font size and family for the text in the map plot
#'
#' \code{theme_map()}: Specify the font size and family for the text in the map
#' plot drawn with \code{draw()}.
#'
#' @param font A font name, default is \code{""}.
#' @param size A number, default is \code{12}.
#' @importFrom ggplot2 %+replace%
#' @export
#'
#' @seealso [draw()]
theme_map <- function(font = "", size = 12) {
  ggplot2::theme_void() %+replace%

    ggplot2::theme(
      text = ggplot2::element_text(family = font, size = size),

      legend.text = ggplot2::element_text(family = font, size = size)
    )
}
