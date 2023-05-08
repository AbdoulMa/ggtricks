# Serie circle ----
generate_id <- function(group, id, max) max * group + id

group_count <- function(x_discrete, x, y, group) {
  if (x_discrete) {
    return(tapply(y, group, sum, na.rm = TRUE))
  }
  tapply(x, group, sum, na.rm = TRUE)
}

# Serie Text
val_to_position <- function(val) {
  if (val %% 1 == 0) {
    return((val - 1) + .5)
  } else if (val %% 1 < .5) {
    return(ceiling(val / .5) * .5)
  }
  floor(val / .5) * .5
}

# Pies and variants utils ----
categorize <- function(data) {
  if (!is.factor(data$cat)) {
    data$cat <- factor(data$cat)
  }
  with(data, data[order(cat), ])
}

pre_process_params <- function(data, params) {
  if (!is.numeric(data$val)) {
    cli::cli_abort("{.field val} column should be numeric.")
  }
  data <- categorize(data)
  cat_is_present <- FALSE
  if (!is.null(params$spotlight_cat)) {
    cat_index <- which(data$cat == params$spotlight_cat)
    cat_is_present <- length(cat_index) > 0 || FALSE
    params$cat_is_present <- cat_is_present
    params$cat_index <- cat_index
    if (!cat_is_present) {
      cli::cli_warn("Cateogry {.field {params$spotlight_cat}} is not present, we can't spotlight it.")
    } else {
      if (params$init_angle != 0 || params$spotlight_max) {
        cli::cli_warn("{.field splotlight_cat} used, {.field init_angle} & {.field spotlight_max} not used.")
      }
    }
  } else if (is.null(params$spotlight_cat) && !is.null(params$spotlight_position) && !params$spotlight_max) {
    cli::cli_warn("No category or max to spotlight, so spotlight position is useless.")
  } else if (params$spotlight_max && params$init_angle != 0) {
    cli::cli_warn("You set {.field spotlight_max} so angle parameter is not used anymore.")
  }
  if (is.null(params$spotlight_position) || !params$spotlight_position %in% c("top", "right", "bottom", "left")) {
    if (cat_is_present || params$spotlight_max) {
      cli::cli_warn(c(
        x = "You set an unknown {.field spotlight_position}.",
        i = "{.code top} is selected by default."
      ))
    }
    params$spotlight_position <- "top"
  }
  params
}

twopi <- 2 * pi

angle_rotation <- function(mean_max, spotlight_position = "top") {
  mean_max_angle <- 360 * mean_max
  angle_rotation <- switch(spotlight_position,
    "bottom" = 270,
    "right" = 0,
    "left" = 180,
    90
  )
  init.angle <- 360 - mean_max_angle + angle_rotation
  init.angle
}

t2xy <- function(t, cat, init, x, x0 = 0, y0 = 0, radius = 1, init.angle = 0, spotlight_cat = NA, spotlight_max = FALSE, spotlight_position = "top", cat_is_present = FALSE, cat_index = NA) {
  if (!(is.null(spotlight_cat) || is.na(spotlight_cat)) && cat_is_present) {
    cat_pos <- cat_index[1]
    mean_cat <- mean(x[cat_pos + 0:1])
    init.angle <- angle_rotation(mean_cat, spotlight_position)
  } else if (spotlight_max) {
    max_pos <- which.max(init)
    mean_max <- mean(x[max_pos + 0:1])
    init.angle <- angle_rotation(mean_max, spotlight_position)
  }
  t2p <- twopi * t + init.angle * pi / 180
  list(x = x0 + radius * cos(t2p), y = y0 + radius * sin(t2p))
}

rescale <- function(x, to = c(0, 1), from = range(x, na.rm = TRUE, finite = TRUE), ...) {
  if (from[1] == from[2] || to[1] == to[2]) {
    return(ifelse(is.na(x), NA, mean(to)))
  }
  (x - from[1]) / diff(from) * diff(to) + to[1]
}

# Slice ----
# Redefine methods
angle_rotation_slice <- function(a, slice_position = "top") {
  init.angle <- -a / 2
  init.angle <- switch(slice_position,
    "right" = init.angle, # Case position "right"
    "top" = init.angle + 90, # Case position "top"
    "left" = init.angle + 180, # Case position "left"
    "bottom" = init.angle + 270, # Case position "bottom"
    0
  )
  init.angle
}

t2xy_slice <- function(t, radius = .8, slice_angle = 180, x0 = 0, y0 = 0, init_angle = 0, slice_position = NA) {
  t2p <- slice_angle * t + init_angle * pi / 180
  list(x = x0 + radius * cos(t2p), y = y0 + radius * sin(t2p))
}

pre_process_slice_params <- function(data, params) {
  if (!is.numeric(data$val)) {
    cli::cli_abort("{.field val} column should be numeric.")
  }
  if (!is.na(params$slice_position)) {
    if (params$init_angle != 0) {
      cli::cli_warn("{.field slice_position} used,  {.field init_angle} not used.")
    }
    params$init_angle <- angle_rotation_slice(params$slice_angle, params$slice_position)
  }
  params$slice_angle <- params$slice_angle * pi / 180
  params
}
