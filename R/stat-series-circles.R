# Serie circle ----
#' See [ggplot2::Geom]
#' @usage NULL
#' @format NULL
#' @export
StatSeriesCircles <- ggplot2::ggproto("StatsSeriesCircles", ggplot2::Stat,
  setup_params = function(data, params) {
    # Default operations on data with params
    # before  compute_group
    # print("Call from setup params")
    x_class <- class(data$x)[1]
    y_class <- class(data$y)[1]

    params$x_discrete <- (x_class == "mapped_discrete" && y_class == "numeric")
    params$y_discrete <- (y_class == "mapped_discrete" && x_class == "numeric")

    if (params$r > .5) {
      cli::cli_warn("r should be <= .5.")
      params$r <- .5
    }
    classes_constraints <- xor(params$x_discrete, params$y_discrete)
    if (!classes_constraints) {
      cli::cli_abort("There should be a discrete column and a numeric one.")
    }
    params
  },
  setup_data = function(data, params) {
    count <- group_count(params$x_discrete, data$x, data$y, data$group)
    if (params$x_discrete) {
      indexes <- Map(\(x) which(data$group == x)[1], x = seq_along(count))
      indexes <- as.numeric(indexes)
      x_val <- as.factor(indexes)
      y_val <- count
    } else {
      indexes <- Map(\(x) which(data$group == x)[1], x = seq_along(count))
      indexes <- as.numeric(indexes)
      x_val <- count
      y_val <- as.factor(indexes)
    }
    df <- data.frame(
      x = x_val,
      y = y_val,
      PANEL = data$PANEL[indexes],
      group = data$group[indexes]
    )
    if (!is.null(data$fill)) {
      df$fill <- data$fill[indexes]
    }
    if (params$x_discrete) {
      df <- df[df$y > 0, ]
      df$x <- sequence(rle(as.numeric(df$PANEL))$lengths)
      return(df)
    }

    df <- df[df$x > 0, ]
    df$y <- sequence(rle(as.numeric(df$PANEL))$lengths)
    df
    # Data from here is passed to compute_group
  },
  compute_group = function(data, scales, init_angle = 0, r = .5, x_discrete = TRUE, y_discrete = FALSE) {
    # Convert angle from deg to rad
    init_angle <- init_angle * pi / 180
    # Base R Approach
    circle_data <- data
    circle_data$nb_circles <- ifelse(x_discrete, circle_data$y %/% 1, circle_data$x %/% 1)
    circle_data$circles_centers <- lapply(circle_data$nb_circles, \(x) 0:x)
    circle_data$circles_values <- Map(\(X, Y) list(rep(1, X), Y %% 1), X = circle_data$nb_circles, Y = ifelse(x_discrete, circle_data$y, circle_data$x))
    circle_reps <- lengths(circle_data$circles_centers)
    circle_data <- data.frame(
      x = rep(circle_data$x, circle_reps),
      y = rep(circle_data$y, circle_reps),
      nb_circles = rep(circle_data$nb_circles, circle_reps),
      PANEL = rep(circle_data$PANEL, circle_reps),
      group = rep(circle_data$group, circle_reps),
      circles_centers = unlist(circle_data$circles_centers),
      circles_values = unlist(circle_data$circles_values)
    )

    circle_data <- circle_data[circle_data$circles_values != 0, ]
    circle_data$id <- 1:nrow(circle_data)
    # Base version ----
    circle_data <- Map(
      function(id, x, y, PANEL, group, nb_circles, circles_centers, circles_values) {
        prop_val <- 1
        n <- 64

        final_angle <- (circles_values * 2 * pi) / prop_val
        nb_points_to <- (n * final_angle) %/% (2 * pi)
        x_pos <- c(r * cos((0:nb_points_to) * (2 * pi / n) + init_angle), r * cos(final_angle + init_angle))
        y_pos <- c(r * sin((0:nb_points_to) * (2 * pi / n) + init_angle), r * sin(final_angle + init_angle))
        if (circles_values != 1) {
          x_pos <- c(0, x_pos)
          y_pos <- c(0, y_pos)
        }

        x <- ifelse(x_discrete, x, circles_centers) + x_pos
        y <- ifelse(x_discrete, circles_centers, y) + y_pos

        data.frame(id, x, y)
      },
      id = circle_data$id, x = circle_data$x, y = circle_data$y,
      PANEL = circle_data$PANEL, group = circle_data$group,
      nb_circles = circle_data$nb_circles,
      circles_centers = circle_data$circles_centers,
      circles_values = circle_data$circles_values
    )
    # Data passed to geom (Must contain columns needed "required" in geom)
    circle_data <- do.call(rbind, circle_data)
    circle_data
  },
  # Mandatory in data passed to Stat
  required_aes = c("x", "y")
)

#' See  [ggplot2::stat_identity]
#' @inheritParams geom_series_circles
#' @inheritParams ggplot2::stat_identity
#' @returns A ggplot2 layer.
#' @export
stat_series_circles <- function(mapping = NULL, data = NULL, geom = "series_circles",
                                position = "identity", show.legend = NA, na.rm = FALSE,
                                inherit.aes = TRUE,
                                init_angle = NULL,
                                r = NA,
                                ...) {
  ggplot2::layer(
    stat = StatSeriesCircles,
    geom = geom,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      init_angle = init_angle,
      r = r,
      ...
    )
  )
}

# Serie Text ----
#' See [ggplot2::Geom]
#' @usage NULL
#' @format NULL
#' @export
StatSeriesText <- ggplot2::ggproto("StatSeriesText", ggplot2::Stat,
  setup_params = function(data, params) {
    x_class <- class(data$x)[1]
    y_class <- class(data$y)[1]
    params$x_discrete <- (x_class == "mapped_discrete" && y_class == "numeric")
    params$y_discrete <- (y_class == "mapped_discrete" && x_class == "numeric")

    classes_constraints <- xor(params$x_discrete, params$y_discrete)

    if (!classes_constraints) {
      cli::cli_abort("There should be a discrete column and a numeric one.")
    }
    params
  },
  compute_group = function(data, scales, flip = FALSE, x_discrete = TRUE, y_discrete = FALSE) {
    val <- ifelse(x_discrete, data$y, data$x)
    val <- val_to_position(val)

    if (x_discrete) {
      return(data.frame(
        x = data$x,
        y = val + .35
      ))
    }
    data.frame(
      x = val + .5,
      y = data$y
    )
  },
  # Mandatory in data passed to Stat
  required_aes = c("x", "y")
)


#' See [ggplot2::stat_identity]
#' @inheritParams geom_series_text
#' @inheritParams ggplot2::stat_identity
#' @returns A ggplot2 layer.
#' @export
stat_series_text <- function(mapping = NULL, data = NULL, geom = "series_text",
                             position = "identity", show.legend = NA, na.rm = FALSE,
                             inherit.aes = TRUE,
                             ...) {
  ggplot2::layer(
    stat = StatSeriesText,
    geom = geom,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}
