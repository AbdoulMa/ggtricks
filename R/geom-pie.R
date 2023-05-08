#' @param x0 Init position x
#' @param y0 Init position y
#' @param init_angle Starting angle
#' @param color Plot border colour
#' @param alpha Filling colour transparency \[0,1\]
#' @param linewidth Plot border size
#' @param spotlight_max `TRUE` if we want the max value category
#' to drive the positions of all categories
#' @param spotlight_cat Should be a value inside categories vector.
#' When it is provided, it is this category position which drives the
#' positions of all categories
#' @param spotlight_position It is used to position the category spotlighted.
#' Value should be in `c("top","right", "bottom", "left")`. When a valid `spotlight_cat` is
#' provided  or `spotlight_max` is set to `TRUE`, the default `spotlight_position` value is set
#' to `TRUE`
#' @param labels_with_tick `TRUE` if we want tick when labelling categories
#' @param labels_family Labels font family
#' @param labels_size Labels font size
#' @param labels_col Labels colour
#' @param labels_hjust Labels horizontal adjusting
#' @param labels_vjust Labels vertical adjusting
#' @param labels_fontface Labels font face
#' @param labels_lineheight Labels line height
#' @param tick_lwd Ticks Size
#' @param mapping Set of aesthetic mappings created by `aes()` or
#'   `aes_()`. If specified and `inherit.aes = TRUE` (the
#'   default), it is combined with the default mapping at the top level of the
#'   plot. You must supply `mapping` if there is no plot mapping.
#' @param data to be displayed in this layer
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
#' @name common_doc
# common_doc <- function()
NULL

draw_panel_function <- function(data, panel_scales, coord,
                                # params to use in draw function
                                # values are changed when changed in geom
                                x0 = 0, y0 = 0,
                                colour = "black", alpha = 1,
                                linewidth = .5, labels_col = "black", labels_size = 1, labels_family = "", labels_with_tick = FALSE, tick_lwd = 1, labels_hjust = .5, labels_vjust = .5, labels_fontface = "plain", labels_lineheight = 1.2) {
  coords <- coord$transform(data, panel_scales)

  xrange <- range(c(data$x, data$xstart, data$xend, data$labelx, panel_scales$x.range))
  yrange <- range(c(data$y, data$ystart, data$yend, data$labely, panel_scales$y.range))
  coords$x <- rescale(data$x, from = xrange)
  coords$y <- rescale(data$y, from = yrange)
  if (!is.null(coords$label)) {
    just_df <- data.frame(
      hjust = ifelse(data$labelx < x0, 1 - labels_hjust, labels_hjust),
      vjust = ifelse(data$labely < y0, 1 - labels_vjust, labels_vjust),
      labelx = data$labelx
    )
    coords$labelx <- rescale(data$labelx, from = xrange) # from = panel_scales$x.rang
    coords$labely <- rescale(data$labely, from = yrange)
    if (labels_with_tick) {
      coords$xstart <- rescale(data$xstart, from = xrange) # from = panel_scales$x.rang
      coords$xend <- rescale(data$xend, from = xrange)
      coords$ystart <- rescale(data$ystart, from = yrange)
      coords$yend <- rescale(data$yend, from = yrange)
    }
  }
  cols <- intersect(colnames(coords), c("xstart", "ystart", "xend", "yend", "labelx", "labely", "label"))
  df_label <- unique(coords[, cols])

  first_row <- coords[1, ]

  fills <- coords[order(coords$cat), ]
  fills <- unique(fills[c("cat", "fill")])
  fills <- fills$fill
  pie_grob <- grid::polygonGrob(
    x = coords$x,
    y = coords$y,
    default.units = "native",
    id = coords$cat,
    gp = grid::gpar(
      col = colour,
      fill = fills,
      lwd = linewidth,
      alpha = alpha
    )
  )
  grid::grobTree(
    pie_grob,
    if (!is.null(df_label$label) && labels_with_tick) {
      grid::segmentsGrob(
        x0 = df_label$xstart,
        y0 = df_label$ystart,
        x1 = df_label$xend,
        y1 = df_label$yend,
        default.units = "native",
        gp = grid::gpar(
          col = colour,
          lwd = tick_lwd
        )
      )
    },
    if (!is.null(df_label$label)) {
      just_df <- unique(just_df)
      df <- data.frame(
        x = df_label$labelx,
        y = df_label$labely,
        label = df_label$label,
        colour = labels_col, size = labels_size, angle = 0, hjust = just_df$hjust,
        vjust = just_df$vjust, alpha = NA, family = labels_family, fontface = labels_fontface, lineheight = labels_lineheight
      )
      ggplot2::GeomText$draw_panel(
        df,
        list(),
        coord
      )
    }
  )
}

#' See [ggplot2::Geom]
#' @format NULL
#' @usage NULL
#' @export
GeomPie <- ggplot2::ggproto("GeomPie", ggplot2::GeomPolygon,
  setup_data = function(data, params) {
    data
  },
  draw_panel = draw_panel_function,
  required_aes = c("cat", "x", "y"),
  default_aes = ggplot2::aes(
    colour = NA, fill = NA, linewidth = 0.5,
    alpha = NA
  ),
  optional_aes = c("label"),
  rename_size = TRUE
)

#' Create pie plot using Cartesian coordinates system
#'
#' There are two  arguments absolutely needed in  `aes()` mappings:
#' - `cat` A discrete categories vector.
#' - `val` A numerical values vector.
#'
#' @inheritParams common_doc
#' @param radius Driving circle radius
#'
#' @examples
#' my_df <- data.frame(cat = c("Apple", "Banana", "Pineapple"), val = c(2.65, 4.5, 6.25))
#' my_df |>
#'   ggplot2::ggplot() +
#'   geom_pie(ggplot2::aes(cat = cat, val = val)) +
#'   ggplot2::coord_equal()
#'
#' @returns A ggplot2 layer.
#' @export
geom_pie <- function(mapping = NULL, data = NULL,
                     show.legend = NA,
                     na.rm = FALSE, inherit.aes = TRUE,
                     init_angle = 0, x0 = 0, y0 = 0, radius = 1, color = "black", alpha = 1, linewidth = .5, spotlight_max = FALSE, spotlight_cat = NULL, spotlight_position = NULL, labels_with_tick = FALSE,
                     labels_family = "",
                     labels_size = 5,
                     labels_col = "black",
                     labels_hjust = .5, labels_vjust = .5, labels_fontface = "plain", labels_lineheight = 1.2, tick_lwd = 1, ...) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatPie,
    geom = GeomPie,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    check.param = FALSE,
    params = list(
      na.rm = na.rm,
      init_angle = init_angle,
      x0 = x0,
      y0 = y0,
      radius = radius,
      color = color,
      alpha = alpha,
      linewidth = linewidth,
      spotlight_max = spotlight_max,
      spotlight_cat = spotlight_cat,
      spotlight_position = spotlight_position,
      labels_col = labels_col,
      labels_size = labels_size,
      labels_with_tick = labels_with_tick,
      labels_family = labels_family,
      labels_fontface = labels_fontface,
      labels_hjust = labels_hjust,
      labels_vjust = labels_vjust,
      labels_lineheight = labels_lineheight,
      tick_lwd = tick_lwd,
      ...
    )
  )
}
