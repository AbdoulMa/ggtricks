
draw_panel_function <- function(data, panel_scales, coord, r = .5) {
  max <- max(data$group, data$id)
  data$circle_id <- generate_id(data$group, data$id, max)
  coords <- coord$transform(data, panel_scales)

  first_row <- coords[1, ]
  # Polygons of first 10 groups are
  # drawn first
  first_plotted <- (coords$group < 10) # & (coords$id <= 10)
  fills_1 <- coords[first_plotted, ] # filter(group < 10)
  fills_1 <- fills_1[order(fills_1$group, fills_1$id), ] # arrange(id, group)
  fills_1 <- unique(fills_1[c("id", "group", "fill")]) # distinct(id, group, fill )
  fills_1 <- fills_1$fill

  fills_2 <- coords[!first_plotted, ]
  fills_2 <- fills_2[order(fills_2$group, fills_2$id), ]
  fills_2 <- unique(fills_2[c("id", "group", "fill")])
  fills_2 <- fills_2$fill

  fills <- c(fills_1, fills_2)
  grid::polygonGrob(
    x = coords$x,
    y = coords$y,
    default.units = "native",
    id = coords$circle_id,
    gp = grid::gpar(
      fill = fills,
      lwd = first_row$linewidth,
      col = first_row$col
    )
  )
}

#' See [ggplot2::Geom]
#' @format NULL
#' @usage NULL
#' @export
GeomSeriesCircles <- ggplot2::ggproto("GeomSeriesCircles", ggplot2::GeomPolygon,
  draw_panel = draw_panel_function,
  required_aes = c("x", "y"),
  default_aes = ggplot2::aes(
    colour = NA, fill = "black", linewidth = 0.5,
    alpha = NA, subgroup = NULL
  ),
  rename_size = TRUE
)

#' @title Create a series of circles plot
#'
#' @description
#' `geom_series_circles()` can be used as an alternative for
#' single or multiple bar charts. It consists of using
#' whole and fragments of circles to represent numerical values.
#' As it draws circles, the geom should use with [ggplot2::coord_equal()]
#' to maintain the "aspect ratio".
#'
#' There are two are arguments absolutely needed in `aes()` mappings:
#' - `x` A vector mapping the abscissa  axis `x`,  i.e. a character vector
#' when `x` is a numerical vector, or  a numerical vector when `y` is a character
#' vector.
#' - `y` A vector mapping the ordinate axis `y`, i.e. a numerical vector
#' when `x` is a character vector or vice versa.
#' There is a default mapping `fill` with  value `black` to fill  circles/fragments
#' of circles with. It can be used in  `aes` mapping or as  a global argument
#'  for all the circles.
#'
#' @param mapping Set of aesthetic mappings created by `aes()` or
#'   `aes_()`. If specified and `inherit.aes = TRUE` (the
#'   default), it is combined with the default mapping at the top level of the
#'   plot. You must supply `mapping` if there is no plot mapping.
#' @param data to be displayed in this layer
#' @param init_angle Circle drawing starting angle.
#' @param r Circle radius, should be <= 0.5.
#' @param color Color of circles/fragments of circles borders.
#'
#' @param linewidth Size of circles/fragments of circles borders.
#' @param na.rm If `FALSE`, the default, missing values are removed with
#'   a warning. If `TRUE`, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends?
#'   `NA`, the default, includes if any aesthetics are mapped.
#'   `FALSE` never includes, and `TRUE` always includes.
#'   It can also be a named logical vector to finely select the aesthetics to
#'   display.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics,
#'   rather than combining with them.
#' @param ... other arguments passed on to `layer()`.
#'
#' @examples
#' my_df <- data.frame(cat = c("Apple", "Banana", "Pineapple"), val = c(2.65, 4.5, 6.25))
#' my_df |>
#'   ggplot2::ggplot() +
#'   geom_series_circles(ggplot2::aes(cat, val)) +
#'   ggplot2::coord_equal()
#'
#' @returns A ggplot2 layer.
#' @export
geom_series_circles <- function(mapping = NULL, data = NULL,
                                show.legend = NA,
                                na.rm = FALSE, inherit.aes = TRUE,
                                init_angle = 0, r = .5, color = NA, linewidth = 0.5, ...) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatSeriesCircles,
    geom = GeomSeriesCircles,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, init_angle = init_angle, r = r, color = color, linewidth = linewidth, ...)
  )
}

# Serie Text ----

#' @rdname geom_series_text
#' @export
GeomSeriesText <- ggplot2::ggproto("GeomSeriesText", ggplot2::GeomText)

#' Create series of circles labels text
#'
#' @description
#' `geom_series_text` is designed to be used in concert with [geom_series_circles].
#' It renders the `label` mapping to the final position of the series of circles sequence.
#'
#' There are three arguments absolutely needed in `aes()` mappings:
#' - `x` A vector mapping the abscissa  axis `x`,  i.e. a character vector
#' when `x` is a numerical vector, or  a numerical vector when `y` is a character
#' vector.
#' - `y` A vector mapping the ordinate axis `y`, i.e. a numerical vector
#' when `x` is a character vector or vice versa.
#' - `label` Labels.
#'
#' @inheritParams ggplot2::geom_text
#'
#' @examples
#' my_df <- data.frame(cat = c("Apple", "Banana", "Pineapple"), val = c(2.65, 4.5, 6.25))
#' my_df |>
#'   ggplot2::ggplot() +
#'   geom_series_circles(ggplot2::aes(cat, val)) +
#'   geom_series_text(ggplot2::aes(cat, val, label = cat)) +
#'   ggplot2::coord_equal()
#' @returns A ggplot2 layer.
#' @export
geom_series_text <- function(mapping = NULL, data = NULL,
                             position = "identity", show.legend = NA,
                             na.rm = FALSE, inherit.aes = TRUE,
                             ...) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatSeriesText,
    geom = GeomSeriesText,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
