
draw_panel_function <- function(data, panel_scales, coord,
                                # params to use in draw function
                                # values are changed when changed in geom
                                x0 = 0, y0 = 0,
                                colour = "black", alpha = 1,
                                linewidth = .5, labels_col = "black", labels_size = 1, labels_family = "", labels_with_tick = FALSE, tick_lwd = 1, labels_hjust = .5, labels_vjust = .5, labels_fontface = "plain", labels_lineheight = 1.2) {
  coords <- coord$transform(data, panel_scales)
  # Rescale x & y  to fit
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
    coords$labelx <- rescale(data$labelx, from = xrange)
    coords$labely <- rescale(data$labely, from = yrange)
    if (labels_with_tick) {
      coords$xstart <- rescale(data$xstart, from = xrange)
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
#' @usage NULL
#' @format NULL
#' @export
GeomSlice <- ggplot2::ggproto("GeomSlice", ggplot2::GeomPolygon,
  setup_data = function(data, params) {
    data
  },
  draw_panel = draw_panel_function,
  required_aes = c("cat", "x", "y"),
  default_aes = ggplot2::aes(
    colour = NA, fill = "#026841", linewidth = 0.5,
    alpha = NA
  ),
  optional_aes = c("label"),
  rename_size = TRUE
)

#' Create pie slice plot using Cartesian coordinates system
#'
#' There are two  arguments absolutely needed in  `aes()` mappings:
#' - `cat` A discrete categories vector.
#' - `val` A numerical values vector.
#'
#' @inheritParams geom_pie
#' @param slice_angle Pie slice angle
#' @param slice_position Pie slice position
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
geom_slice <- function(mapping = NULL, data = NULL,
                       show.legend = NA,
                       na.rm = FALSE, inherit.aes = TRUE,
                       # For parameters
                       slice_angle = 180, init_angle = 0, x0 = 0, y0 = 0, radius = 1, color = "black", alpha = 1, linewidth = .5, slice_position = NA, labels_with_tick = FALSE,
                       labels_family = "",
                       labels_size = 5,
                       labels_col = "black",
                       labels_hjust = .5, labels_vjust = .5, labels_fontface = "plain", labels_lineheight = 1.2, tick_lwd = 1, ...) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatSlice,
    geom = GeomSlice,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    check.param = FALSE,
    params = list(
      na.rm = na.rm,
      slice_angle = slice_angle,
      init_angle = init_angle,
      x0 = x0,
      y0 = y0,
      radius = radius,
      alpha = alpha,
      slice_position = slice_position,
      color = color,
      linewidth = linewidth,
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
