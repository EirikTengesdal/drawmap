
<!-- README.md is generated from README.Rmd. Please edit that file -->

# drawmap <a href="https://eiriktengesdal.github.io/drawmap/"><img src="man/figures/logo.png" align="right" height="150" alt="drawmap website"/></a>

<!-- badges: start -->
[![R-CMD-check](https://github.com/EirikTengesdal/drawmap/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/EirikTengesdal/drawmap/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.13768420.svg)](https://zenodo.org/doi/10.5281/zenodo.13768420)
<!-- badges: end -->

The goal of `drawmap` is to draw maps in R easily. The package provides
a simple interface to draw maps with a uniform style, and is
particularly useful for drawing maps with multiple layers, such as
countries, regions, and points of interest. The package provides the
following functions:

- `draw()`: Draw a map.
- `load_maps_data()`: Load map data using `maps::map()`.
- `load_coordinates()`: Load points of interest coördinates from a CSV
  file.
- `browse_wfsdata()`: Browse data from a Web Feature Service (WFS)
  endpoint.
- `load_wfsdata()`: Load data from a Web Feature Service (WFS) endpoint.
- `get_map_data()`: Retrieve geospatial data via a URL service (used in
  `load_wfsdata()`).
- `merge_sf()`: Merge `sf` objects from a specified folder.
- `harmonise_vars()`: Harmonise specified variables across `sf` objects.
- `theme_map()`: Specify `ggplot2` custom theme for `drawmap::draw()`
  (font and size).

The package is built on top of the `ggplot2` and `sf` packages. It
accepts different types of spatial data, such as `sf` objects and
`data.frame` objects with longitude and latitude columns.

## Installation

You can install the development version of `drawmap` from
[GitHub](https://github.com/) with `pak::pak()` or
`remotes::install_github()`:

``` r
# Using `pak`:
# install.packages("pak")
pak::pak("EirikTengesdal/drawmap")
# Using `remotes`:
# install.packages("remotes")
remotes::install_github("EirikTengesdal/drawmap")
```

## Example

``` r
library(drawmap)
norway <- load_maps_data(countries = "Norway")
europe <- load_maps_data()
p <- draw(
  area_data = norway,
  area_col = "#512888",
  area_fill = "#D4C2ED",
  other_areas_data = europe,
  other_areas_col = "#ADB0B8",
  other_areas_fill = "#D9DCE6",
  annotation_scale = FALSE
)
```

This yields a map like the one used in the `drawmap` package logo:

<img src="man/figures/README-map-1.png" alt="Map depicting Norway and the surrounding countries." width="100%" style="display: block; margin: auto;" />

## License and copyright
© 2024 Eirik Tengesdal

Licensed under the [MIT License](LICENSE.md).

## Author
[Eirik Tengesdal](https://eiriktengesdal.no/)  
Assistant Professor of Norwegian  
[Department of Early Childhood Education (BLU)](https://www.oslomet.no/en/about/employee/eiten9710/)  
OsloMet – Oslo Metropolitan University  
eirik.tengesdal@oslomet.no

Guest Researcher and PhD Student  
[Department of Linguistics and Scandinavian Studies (ILN)](https://www.hf.uio.no/iln/english/people/aca/scandinavian-languages/temporary/eirikten/)  
University of Oslo  
eirik.tengesdal@iln.uio.no  
ORCID iD: [0000-0003-0599-8925](https://orcid.org/0000-0003-0599-8925)
