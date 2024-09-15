#' Draw maps with \code{ggplot2} functions and \code{sf} objects
#'
#' \code{draw()}: Draw maps with \code{ggplot2} functions and \code{sf} objects.
#' It is based on previous versions of the
#' \href{https://github.com/EirikTengesdal/MapDrawingScriptforR}{\code{Map
#' Drawing Script for R}}, offering a consolidated method of creating maps with
#' a uniform style.
#'
#' @param area_data An \code{sf} object to be drawn with \code{ggplot2}
#' @param area_col An optional string for the object's \code{colour} argument
#' @param area_fill An optional string for the object's \code{fill} argument
#' @param other_areas_data An optional \code{sf} object to be drawn with
#'   \code{ggplot2}, default is \code{NULL}
#' @param other_areas_col An optional string for the object's \code{colour}
#'   argument, default is \code{NULL}
#' @param other_areas_fill An optional string for the object's \code{fill}
#'   argument, default is \code{NULL}
#' @param subdivision_data An optional \code{sf} object to be drawn with
#'   \code{ggplot2}, default is \code{NULL}
#' @param subdivision_colfill An optional vector of strings for the object's
#'   \code{colour} and \code{fill} arguments, default is \code{NULL}
#' @param font An optional argument for specifying the font family, default is
#'   \code{""}
#' @param size An optional argument for specifying the font size, default is
#'   \code{12}
#' @param coordinates An optional vector with object to be drawn with
#'   \code{ggplot2}, default is \code{NULL}
#' @param annotation_scale An optional logical argument for specifying whether
#'   to add a \code{ggspatial} annotation scale, default is \code{TRUE}
#' @param as_location An optional argument for specifying the location of the
#'   annotation scale, default is \code{"br"}
#' @param as_width_hint An optional argument for specifying the width hint of
#'   the annotation scale, default is \code{0.3}
#' @param as_text_family An optional argument for specifying the font family of
#'   the annotation scale, default is \code{font}
#' @param subdivision_col_values An optional vector of strings for the object's
#'   \code{colour} argument, default is \code{NULL}
#' @param subdivision_fill_values An optional vector of strings for the object's
#'   \code{fill} argument, default is \code{NULL}
#' @param subdivision_legend_order An optional integer for specifying the
#'   object's order in the legend, default is \code{2}
#' @param coord_size An optional integer for specifying the coordinate object's
#'   size, default is \code{4}
#' @param coord_shape An optional integer for specifying the coordinate object's
#'   shape, default is \code{21}
#' @param coord_colfill An optional argument for specifying the
#'   \code{coordinates} column to be passed as \code{colour} argument, default
#'   is \code{coordinates$Type}
#' @param coord_col An optional string for the object's \code{colour} argument,
#'   default is \code{"darkred"}
#' @param coord_fillcol An optional string for the object's \code{fill}
#'   argument, default is \code{"red"}
#' @param label_repel An optional logical argument for specifying whether to add
#'   a \code{ggrepel} repel label, default is \code{FALSE}
#' @param label_repel_x An optional argument for specifying the
#'   \code{coordinates} column to be passed as \code{x} argument, default is
#'   \code{coordinates$Longitude}
#' @param label_repel_y An optional argument for specifying the
#'   \code{coordinates} column to be passed as \code{y} argument, default is
#'   `coordinates$Latitude`
#' @param label_repel_label An optional argument for specifying the
#'   \code{coordinates} column to be passed as \code{label} argument, default is
#'   \code{coordinates$Place}
#' @param label_size An optional argument for specifying the label size, default
#'   is \code{size / ggplot2::.pt}. This is due to the conversion that happens
#'   for \code{geom_text} objects
#' @param coord_sf An optional logical argument for specifying whether to use
#'   \code{coord_sf}, default is \code{TRUE}
#' @param coord_sf_xlim An optional vector of integers for specifying the x-axis
#'   limits, default is \code{c(2.5, 32.5)}, which works fine for Norway
#' @param coord_sf_ylim An optional vector of integers for specifying the y-axis
#'   limits, default is \code{c(57.3, 72)}, which works fine for Norway
#' @param coord_sf_expand An optional logical argument for specifying whether to
#'   expand the \code{coord_sf} object, default is \code{FALSE}
#' @param coord_legend_order An optional integer for specifying the coordinate
#'   object's order in the legend, default is \code{1}
#' @param margin_t An optional integer for specifying the legend.box.margin top
#'   margin, default is \code{0}
#' @param margin_r An optional integer for specifying the legend.box.margin
#'   right margin, default is \code{0}
#' @param margin_b An optional integer for specifying the legend.box.margin
#'   bottom margin, default is \code{0}
#' @param margin_l An optional integer for specifying the legend.box.margin left
#'   margin, default is \code{0}
#' @param margin_unit An optional string for specifying the legend.box.margin
#'   unit, default is \code{"mm"}
#' @param panel_background An optional string for specifying the panel
#'   background colour, default is \code{"#E4F4FF"}
#' @param output_filename An optional string for specifying the output filename,
#'   default is \code{NULL}. If it is not null, the function will save the plot
#'   as a file
#' @param output_device An optional function for specifying the output device,
#'   default is \code{ragg::agg_png}
#' @param output_width An optional integer for specifying the output width,
#'   default is \code{5.68}, which works fine for Norway
#' @param output_height An optional integer for specifying the output height,
#'   default is \code{6.50}, which works fine for Norway
#' @param output_resolution An optional integer for specifying the output
#'   resolution, default is \code{1200} DPI
#' @param attribution An optional string for specifying any data source or
#'   copyright information, default is \code{NULL}
#' @param attribution_size_factor An optional integer for specifying the size
#'   factor to be applied to the font size of the attribution text, default is
#'   \code{0.5}
#' @param attribution_colour An optional string for specifying the colour of the
#'   attribution text, default is \code{"#636363"} (a shade of dark grey)
#'
#' @return A \code{ggplot2} object. If \code{output_filename} is not
#'   \code{NULL}, the function will save the plot as a file.
#' @export
#'
#' @seealso [browse_wfsdata()], [get_map_data()], [harmonise_vars()],
#'   [load_coordinates()], [load_maps_data()], [load_wfsdata()], [merge_sf()],
#'   [theme_map()]
#' @examples
#' norway <- load_maps_data(countries = "Norway")
#'
#' draw(area_data = norway,
#'      area_col = "#512888",
#'      area_fill = "#D4C2ED")
draw <- function(area_data,
                 area_col,
                 area_fill,
                 other_areas_data = NULL,
                 other_areas_col = NULL,
                 other_areas_fill = NULL,
                 subdivision_data = NULL,
                 subdivision_colfill = NULL,
                 subdivision_col_values = NULL,
                 subdivision_fill_values = NULL,
                 subdivision_legend_order = 2,
                 font = "",
                 size = 12,
                 coordinates = NULL,
                 coord_size = 4,
                 coord_shape = 21,
                 coord_colfill = coordinates$Type,
                 coord_col = "darkred",
                 coord_fillcol = "red",
                 label_repel = FALSE,
                 label_repel_x = coordinates$Longitude,
                 label_repel_y = coordinates$Latitude,
                 label_repel_label = coordinates$Place,
                 label_size = size / ggplot2::.pt,
                 coord_sf = TRUE,
                 coord_sf_xlim = c(2.5, 32.5),
                 coord_sf_ylim = c(57.3, 72),
                 coord_sf_expand = FALSE,
                 coord_legend_order = 1,
                 annotation_scale = TRUE,
                 as_location = "br",
                 as_width_hint = 0.3,
                 as_text_family = font,
                 margin_t = 0,
                 margin_r = 0,
                 margin_b = 0,
                 margin_l = 0,
                 margin_unit = "mm",
                 panel_background = "#E4F4FF",
                 output_filename = NULL,
                 output_device = ragg::agg_png,
                 output_width = 5.68,
                 output_height = 6.50,
                 output_resolution = 1200,
                 attribution = NULL,
                 attribution_size_factor = 0.5,
                 attribution_colour = "#636363") {
  if (is.null(other_areas_col)) {
    other_areas_col <- area_col
  }
  if (is.null(other_areas_fill)) {
    other_areas_fill <- area_fill
  }

  if (is.null(subdivision_col_values)) {
    subdivision_col_values <- c()
    for (county in subdivision_colfill) {
      subdivision_col_values[county] <- c(county = area_col)
    }
  }

  if (is.null(subdivision_fill_values)) {
    subdivision_fill_values <- c()
    i <- 1
    for (county in subdivision_colfill) {
      subdivision_fill_values[county] <- c(county = grDevices::rainbow(length(subdivision_colfill))[i])
      i <- i + 1
    }
  }

  if (annotation_scale && as_location == "br") {
    attribution_offset <- 0.3
  } else {
    attribution_offset <- 0
  }

  p <- ggplot2::ggplot() + {
    if (!is.null(other_areas_data))
      ggplot2::geom_sf(data = other_areas_data,
                       colour = other_areas_col,
                       fill = other_areas_fill)
  } +

    ggplot2::geom_sf(data = area_data,
                     colour = area_col,
                     fill = area_fill) +
    {
      if (!is.null(subdivision_data))
        ggnewscale::new_scale_colour()
    } +

    {
      if (!is.null(subdivision_data))
        ggnewscale::new_scale_fill()
    } +

    {
      if (!is.null(subdivision_data))
        ggplot2::geom_sf(data = subdivision_data, ggplot2::aes(
          colour = as.factor(subdivision_colfill),
          fill = as.factor(subdivision_colfill)
        ))
    } +

    {
      if (!is.null(subdivision_data))
        ggplot2::scale_colour_manual(
          values = subdivision_col_values,
          limits = names(subdivision_col_values),
          guide = ggplot2::guide_legend(title = "", order = subdivision_legend_order)
        )
    } +

    {
      if (!is.null(subdivision_data))
        ggplot2::scale_fill_manual(
          values = subdivision_fill_values,
          limits = names(subdivision_fill_values),
          guide = ggplot2::guide_legend(title = "", order = subdivision_legend_order)
        )
    } +

    {
      if (!is.null(coordinates))
        ggnewscale::new_scale_colour()
    } +

    {
      if (!is.null(coordinates))
        ggnewscale::new_scale_fill()
    } +

    {
      if (!is.null(coordinates))
        ggplot2::geom_sf(
          data = coordinates,
          size = coord_size,
          shape = coord_shape,
          ggplot2::aes(
            colour = as.factor(coord_colfill),
            fill = as.factor(coord_colfill)
          )
        )
    } +

    {
      if (label_repel)
        ggrepel::geom_label_repel(
          data = coordinates,
          ggplot2::aes(x = label_repel_x, y = label_repel_y, label = label_repel_label),
          family = font,
          size = label_size
        )
    } +

    {
      if (!is.null(coordinates))
        ggplot2::scale_colour_manual(values = coord_col,
                                     guide = ggplot2::guide_legend(order = coord_legend_order))
    } +

    {
      if (!is.null(coordinates))
        ggplot2::scale_fill_manual(values = coord_fillcol,
                                   guide = ggplot2::guide_legend(order = coord_legend_order))
    } +

    {
      if (coord_sf)
        ggplot2::coord_sf(xlim = coord_sf_xlim,
                          ylim = coord_sf_ylim,
                          expand = coord_sf_expand)
    } +

    theme_map(font = font, size = size) +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      legend.position = "inside",
      legend.justification = c(0, 1),
      legend.box.margin = ggplot2::margin(margin_t, margin_r, margin_b, margin_l, unit = margin_unit),
      panel.background = ggplot2::element_rect(panel_background),
      panel.border = ggplot2::element_rect(fill = NA)
    ) +

    {
      if (annotation_scale)
        ggspatial::annotation_scale(location = as_location,
                                    width_hint = as_width_hint,
                                    text_family = as_text_family)
    } +

    {
      if (!is.null(attribution))
        ggplot2::annotate(
          geom = "richtext",
          x = coord_sf_xlim[2],
          y = coord_sf_ylim[1],
          hjust = 1,
          vjust = 0,
          fill = NA,
          label.color = NA,
          label.padding = grid::unit(rep(0, 4), "pt"),
          label.margin = grid::unit(c(0, 0.25, (
            0.25 + attribution_offset
          ), 0), "cm"),
          label = attribution,
          family = font,
          size = label_size * attribution_size_factor,
          colour = attribution_colour
        )
    }

  if (!is.null(output_filename)) {
    ggplot2::ggsave(
      output_filename,
      plot = p,
      device = output_device,
      width = output_width,
      height = output_height,
      dpi = output_resolution
    )
  }

  return(p)
}
