#' See [ggplot2::Geom]
#' @format NULL
#' @usage NULL
#' @export
StatDonut <- ggplot2::ggproto("StatDonut", ggplot2::Stat,
  setup_params = function(data, params) {
    # Default operations on data with params
    pre_process_params(data, params)
  },
  setup_data = function(data, params) {
    # # Data from here is passed to compute_group
    categorize(data)
  },
  compute_panel = function(data, scales, x0 = 0, y0 = 0, r1 = 1, r2 = 0.75, color = "black", init_angle = 0,
                           spotlight_cat = NA, spotlight_max = FALSE, spotlight_position = NULL, labels_with_tick = FALSE, cat_is_present = FALSE, cat_index = NA) {
    # ----

    # TODO Essayer de voir si on peut récupérer cat_is_present et l'envoyer dans t2xy
    # print(paste("Cat is present:", cat_is_present))
    # print("Compute Panel")
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
    # radius_scale <- ifelse (r1 > r2, r2 / r1, r1 / r2)
    df <- data.frame(cat = character(), x = numeric(), y = numeric(), xstart = numeric(), ystart = numeric(), xend = numeric(), yend = numeric(), labelx = numeric(), labely = numeric(), label = character())
    for (i in 1L:nx) {
      n <- max(2, floor(edges * dx[i]))
      # print(paste0("n: ", n))
      P1 <- t2xy(seq.int(x[i], x[i + 1], length.out = n), cat, init, x, x0 = x0, y0 = y0, radius = r1, init.angle = init_angle, spotlight_max = sm, spotlight_cat = sc, spotlight_position = mp, cat_is_present = cat_is_present, cat_index = cat_index)
      P2 <- t2xy(seq.int(x[i], x[i + 1], length.out = n), cat, init, x, x0 = x0, y0 = y0, radius = r2, init.angle = init_angle, spotlight_max = sm, spotlight_cat = sc, spotlight_position = mp, cat_is_present = cat_is_present, cat_index = cat_index)

      df2 <- data.frame(cat = cat[i], x = c(P1$x, rev(P2$x)), y = c(P1$y, rev(P2$y)))

      if (!is.null(data$fill)) df2$fill <- data$fill[i]
      # TODO gérer le cas
      # Compute ticks coords when necessary
      if (!is.null(data$label)) {
        label_radius <- max(r1, r2)
        Plabel <- t2xy(mean(x[i + 0:1]), cat, init, x, x0 = x0, y0 = y0, radius = label_radius + label_radius * .1, init.angle = init_angle, spotlight_max = sm, spotlight_cat = sc, spotlight_position = mp, cat_is_present = cat_is_present, cat_index = cat_index)
        df2$labelx <- Plabel$x
        df2$labely <- Plabel$y
        df2$label <- data$label[i]
        if (labels_with_tick) {
          ticks_start <- t2xy(mean(x[i + 0:1]), cat, init, x, x0 = x0, y0 = y0, radius = label_radius, init.angle = init_angle, spotlight_max = sm, spotlight_cat = sc, spotlight_position = mp, cat_is_present = cat_is_present, cat_index = cat_index)
          ticks_end <- t2xy(mean(x[i + 0:1]), cat, init, x, x0 = x0, y0 = y0, radius = label_radius + label_radius * .05, init.angle = init_angle, spotlight_max = sm, spotlight_cat = sc, spotlight_position = mp, cat_is_present = cat_is_present, cat_index = cat_index)
          df2$xstart <- ticks_start$x
          df2$ystart <- ticks_start$y
          df2$xend <- ticks_end$x
          df2$yend <- ticks_end$y
        }
      }
      df <- rbind(df, df2)
      # print(paste0("Label x, y ", Plabel$x, Plabel$y))
    }
    df
    # ----
  },
  extra_params = c("na.rm", "angle", "x0", "y0", "r1", "r2", "spotlight_max", "spotlight_cat", "spotlight_position", "color"),
  #  required after setup
  required_aes = c("cat", "val")
)

#' See [ggplot2::stat_identity]
#' @inheritParams geom_donut
#' @inheritParams ggplot2::stat_identity
#' @returns A ggplot2 layer.
#' @export
stat_donut <- function(mapping = NULL, data = NULL, geom = "donut",
                       position = "identity", show.legend = NA, na.rm = FALSE,
                       inherit.aes = TRUE,
                       ...) {
  ggplot2::layer(
    stat = StatDonut,
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
