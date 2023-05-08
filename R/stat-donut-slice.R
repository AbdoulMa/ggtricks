#' See [ggplot2::Geom]
#' @usage NULL
#' @format NULL
#' @export
StatDonutSlice <- ggplot2::ggproto("DonutSlice", ggplot2::Stat,
  setup_params = function(data, params) {
    pre_process_slice_params(data, params)
  },
  setup_data = function(data, params) {
    categorize(data)
  },
  compute_panel = function(data, scales, x0 = 0, y0 = 0, r1 = 1, r2 = .65, slice_angle = 180, color = "black", init_angle = 0,
                           link_with_origin = FALSE,
                           slice_position = "top", labels_with_tick = F) {
    edges <- 100
    data <- data[data$val > 0, ]
    cat <- data$cat
    x <- data$val
    init <- x
    x <- c(0, cumsum(x) / sum(x))
    dx <- diff(x)
    nx <- length(dx)
    df <- data.frame(cat = character(), x = numeric(), y = numeric(), xstart = numeric(), ystart = numeric(), xend = numeric(), yend = numeric(), labelx = numeric(), labely = numeric(), connectx = numeric(), connecty = numeric(), label = character())
    for (i in 1L:nx) {
      n <- max(2, floor(edges * dx[i]))
      P1 <- t2xy_slice(seq.int(x[i], x[i + 1], length.out = n), radius = r1, slice_angle = slice_angle, x0 = x0, y0 = y0, init_angle = init_angle, slice_position = slice_position)
      P2 <- t2xy_slice(seq.int(x[i], x[i + 1], length.out = n), radius = r2, slice_angle = slice_angle, x0 = x0, y0 = y0, init_angle = init_angle, slice_position = slice_position)
      df2 <- data.frame(cat = cat[i], x = c(P1$x, rev(P2$x)), y = c(P1$y, rev(P2$y)))

      if (!is.null(data$fill)) df2$fill <- data$fill[i]
      # Compute ticks coords when necessary
      if (!is.null(data$label)) {
        label_radius <- max(r1, r2)
        Plabel <- t2xy_slice(mean(x[i + 0:1]), radius = label_radius + label_radius * .1, slice_angle = slice_angle, x0 = x0, y0 = y0, init_angle = init_angle, slice_position = slice_position)
        df2$labelx <- Plabel$x
        df2$labely <- Plabel$y
        df2$label <- data$label[i]
        if (labels_with_tick) {
          ticks_start <- t2xy_slice(mean(x[i + 0:1]), radius = label_radius, slice_angle = slice_angle, x0 = x0, y0 = y0, init_angle = init_angle, slice_position = slice_position)
          ticks_end <- t2xy_slice(mean(x[i + 0:1]), radius = label_radius + label_radius * .05, slice_angle = slice_angle, x0 = x0, y0 = y0, init_angle = init_angle, slice_position = slice_position)
          df2$xstart <- ticks_start$x
          df2$ystart <- ticks_start$y
          df2$xend <- ticks_end$x
          df2$yend <- ticks_end$y
        }
      }

      # Links with origin
      if (link_with_origin) {
        if (i == 1) {
          df2$connectx <- P1$x[1]
          df2$connecty <- P1$y[1]
        } else if (i == nx) {
          df2$connectx <- P1$x[length(P1$x)]
          df2$connecty <- P1$y[length(P1$y)]
        } else {
          df2$connectx <- NA_integer_
          df2$connecty <- NA_integer_
        }
      }
      df <- rbind(df, df2)
    }
    df

    # ----
  },
  extra_params = c("na.rm", "slice_angle", "init_angle", "x0", "y0", "r1", "r2", "slice_position"),
  #  required after setup
  required_aes = c("cat", "val")
)

#' See  [ggplot2::stat_identity]
#' @inheritParams geom_donut_slice
#' @inheritParams ggplot2::stat_identity
#' @returns A ggplot2 layer.
#' @export
stat_donut_slice <- function(mapping = NULL, data = NULL, geom = "donut_slice",
                             position = "identity", show.legend = NA, na.rm = FALSE,
                             inherit.aes = TRUE,
                             ...) {
  ggplot2::layer(
    stat = StatSlice,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}
