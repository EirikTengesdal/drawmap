#' Draw Maps with `ggplot2`
#'
#' @param area_data An sf object to be drawn with `ggplot2`.
#' @param area_col An optional string for the object's `colour` argument.
#' @param area_fill An optional string for the object's `fill` argument.
#' @param other_areas_data An optional sf object to be drawn with `ggplot2`, default is `NULL`.
#' @param other_areas_col An optional string for the object's `colour` argument, default is `NULL`.
#' @param other_areas_fill An optional string for the object's `fill` argument, default is `NULL`.
#' @param subdivision_data An optional sf object to be drawn with `ggplot2`, default is `NULL`.
#' @param subdivision_colfill An optional vector of strings for the object's `colour` and `fill` arguments, default is `NULL`.
#' @param font An optional argument for specifying the font family, default is `""`.
#' @param size An optional argument for specifying the font size, default is `12`.
#' @param coordinates An optional vector with object to be drawn with `ggplot2`, default is `NULL`.
#' @param annotation_scale An optional logical argument for specifying whether to add a `ggspatial` annotation scale, default is `TRUE`.
#' @param as_location An optional argument for specifying the location of the annotation scale, default is `"br"`.
#' @param as_width_hint An optional argument for specifying the width hint of the annotation scale, default is `0.3`.
#' @param as_text_family An optional argument for specifying the font family of the annotation scale, default is `font`.
#' @param subdivision_col_values An optional vector of strings for the object's `colour` argument, default is `NULL`.
#' @param subdivision_fill_values An optional vector of strings for the object's `fill` argument, default is `NULL`.
#' @param subdivision_legend_order An optional integer for specifying the object's order in the legend, default is `2`.
#' @param coord_size An optional integer for specifying the coordinate object's size, default is `4`.
#' @param coord_shape An optional integer for specifying the coordinate object's shape, default is `21`.
#' @param coord_colfill An optional argument for specifying the `coordinates` column to be passed as `colour` argument, default is `coordinates$Type`.
#' @param coord_col An optional string for the object's `colour` argument, default is `"darkred"`.
#' @param coord_fillcol An optional string for the object's `fill` argument, default is `"red"`.
#' @param label_repel An optional logical argument for specifying whether to add a `ggrepel` repel label, default is `FALSE`.
#' @param label_repel_x An optional argument for specifying the `coordinates` column to be passed as `x` argument, default is `coordinates$Longitude`.
#' @param label_repel_y An optional argument for specifying the `coordinates` column to be passed as `y` argument, default is `coordinates$Latitude`.
#' @param label_repel_label An optional argument for specifying the `coordinates` column to be passed as `label` argument, default is `coordinates$Place`.
#' @param label_size An optional argument for specifying the label size, default is `size / ggplot2:::.pt`. This is due to the conversion that happens for `geom_text` objects.
#' @param coord_sf An optional logical argument for specifying whether to use `coord_sf`, default is `TRUE`.
#' @param coord_sf_xlim An optional vector of integers for specifying the x-axis limits, default is `c(2.5, 32.5)`, which works fine for Norway.
#' @param coord_sf_ylim An optional vector of integers for specifying the y-axis limits, default is `c(57.3, 72)`, which works fine for Norway.
#' @param coord_sf_expand An optional logical argument for specifying whether to expand the `coord_sf` object, default is `FALSE`.
#' @param coord_legend_order An optional integer for specifying the coordinate object's order in the legend, default is `1`.
#' @param margin_t An optional integer for specifying the legend.box.margin top margin, default is `0`.
#' @param margin_r An optional integer for specifying the legend.box.margin right margin, default is `0`.
#' @param margin_b An optional integer for specifying the legend.box.margin bottom margin, default is `0`.
#' @param margin_l An optional integer for specifying the legend.box.margin left margin, default is `0`.
#' @param margin_unit An optional string for specifying the legend.box.margin unit, default is `"mm"`.
#' @param panel_background An optional string for specifying the panel background colour, default is `"#E4F4FF"`.
#' @param output_filename An optional string for specifying the output filename, default is `NULL`. If it is not null, the function will save the plot as a file.
#' @param output_device An optional function for specifying the output device, default is `ragg::agg_png`.
#' @param output_width An optional integer for specifying the output width, default is `5.68`.
#' @param output_height An optional integer for specifying the output height, default is `6.50`.
#' @param output_resolution An optional integer for specifying the output resolution, default is `1200`.
#'
#' @return A `ggplot2` object.
#' @export
#'
#' @examples
#' draw(area_data = norway, area_col = "#512888", area_fill = "#D4C2ED", other_areas_data = europe_excl_norway)
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
                 label_size = size / ggplot2:::.pt,
                 coord_sf = TRUE,
                 coord_sf_xlim = c(2.5, 32.5),
                 # Works fine for Norway
                 coord_sf_ylim = c(57.3, 72),
                 # Works fine for Norway
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
                 output_resolution = 1200) {
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
      subdivision_fill_values[county] <- c(county = rainbow(length(subdivision_colfill))[i])
      i <- i + 1
    }
  }

  p <- ggplot2::ggplot() +
    {
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
