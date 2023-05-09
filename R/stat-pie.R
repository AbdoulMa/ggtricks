
#' See [ggplot2::Geom]
#' @format NULL
#' @usage NULL
#' @export
StatPie <- ggplot2::ggproto("StatPie", ggplot2::Stat,
  setup_params = function(data, params) {
    pre_process_params(data, params)
  },
  setup_data = function(data, params) {
    categorize(data)
  },
  compute_panel = function(data, scales, x0 = 0, y0 = 0, radius = 1, init_angle = 0,
                           spotlight_cat = NA, spotlight_max = FALSE, spotlight_position = NULL, labels_with_tick = FALSE, cat_is_present = FALSE, cat_index = NA) {
    edges <- 100
    data <- data[data$val > 0, ]
    cat <- data$cat
    x <- data$val

    init <- x
    x <- c(0, cumsum(x) / sum(x))
    dx <- diff(x)
    nx <- length(dx)
    sm <- spotlight_max
    sc <- spotlight_cat
    mp <- spotlight_position
    df <- data.frame(cat = character(), x = numeric(), y = numeric(), xstart = numeric(), ystart = numeric(), xend = numeric(), yend = numeric(), labelx = numeric(), labely = numeric(), label = character())
    for (i in 1L:nx) {
      n <- max(2, floor(edges * dx[i]))
      P <- t2xy(seq.int(x[i], x[i + 1], length.out = n), cat, init, x, x0 = x0, y0 = y0, radius = radius, init.angle = init_angle, spotlight_max = sm, spotlight_cat = sc, spotlight_position = mp, cat_is_present = cat_is_present, cat_index = cat_index)


      df2 <- data.frame(cat = cat[i], x = c(P$x, x0), y = c(P$y, y0))

      if (!is.null(data$fill)) df2$fill <- data$fill[i]
      # Compute ticks coords when necessary
      if (!is.null(data$label)) {
        Plabel <- t2xy(mean(x[i + 0:1]), cat, init, x, x0 = x0, y0 = y0, radius = radius + radius * .1, init.angle = init_angle, spotlight_max = sm, spotlight_cat = sc, spotlight_position = mp, cat_is_present = cat_is_present, cat_index = cat_index)
        df2$labelx <- Plabel$x
        df2$labely <- Plabel$y
        df2$label <- data$label[i]
        if (labels_with_tick) {
          ticks_start <- t2xy(mean(x[i + 0:1]), cat, init, x, x0 = x0, y0 = y0, radius = radius, init.angle = init_angle, spotlight_max = sm, spotlight_cat = sc, spotlight_position = mp, cat_is_present = cat_is_present, cat_index = cat_index)
          ticks_end <- t2xy(mean(x[i + 0:1]), cat, init, x, x0 = x0, y0 = y0, radius = radius + radius * .05, init.angle = init_angle, spotlight_max = sm, spotlight_cat = sc, spotlight_position = mp, cat_is_present = cat_is_present, cat_index = cat_index)
          df2$xstart <- ticks_start$x
          df2$ystart <- ticks_start$y
          df2$xend <- ticks_end$x
          df2$yend <- ticks_end$y
        }
      }
      df <- rbind(df, df2)
    }
    df
  },
  extra_params = c("na.rm", "angle", "x0", "y0", "radius", "spotlight_max", "spotlight_cat", "spotlight_position"),
  #  required after setup
  required_aes = c("cat", "val")
)

#' See [ggplot2::stat_identity]
#' @inheritParams geom_pie
#' @inheritParams ggplot2::stat_identity
#' @returns A ggplot2 layer.
#' @export
stat_pie <- function(mapping = NULL, data = NULL, geom = "pie",
                     position = "identity", show.legend = NA, na.rm = FALSE,
                     inherit.aes = TRUE,
                     ...) {
  ggplot2::layer(
    stat = StatPie,
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
