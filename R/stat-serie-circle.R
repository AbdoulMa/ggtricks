# Serie circle ----
#' @export
StatSerieCircle <- ggplot2::ggproto("StatSerieCircle", ggplot2::Stat,
                           setup_params = function(data, params) {
                             # Default operations on data with params
                             # before  compute_group
                             # print("Call from setup params")
                             x_class <- class(data$x)[1]
                             y_class <- class(data$y)[1]
                             # print(x_class)
                             # print(y_class)
                             params$x_discrete <- (x_class ==  "mapped_discrete" && y_class == "numeric")
                             params$y_discrete <- (y_class == "mapped_discrete" && x_class == "numeric")

                             if(params$r > .5) {
                               print("r should be <= .5")
                               params$r <- .5
                             }
                             params
                           },
                           setup_data = function(data, params) {
                             # print("Call from setup data")
                             # print(params)
                             # print(data)

                             # print(data)
                             count <- group_count(params$x_discrete, data$x, data$y, data$group)
                             # print(count)
                             if (params$x_discrete) {
                               indexes <- Map(\(x) which(data$group == x)[1], x = seq_along(count))
                               indexes <- as.numeric(indexes)
                               x_val <-  as.factor(indexes)
                               y_val <- count
                             } else {
                               indexes <- Map(\(x) which(data$group == x)[1], x = seq_along(count))
                               indexes <- as.numeric(indexes)
                               x_val <- count
                               y_val <- as.factor(indexes)
                             }
                             # print("Indexes")
                             # print(indexes)
                             df <-  data.frame(
                               x = x_val,
                               y = y_val,
                               PANEL = data$PANEL[indexes],
                               group = data$group[indexes]

                             )
                             if (!is.null(data$fill)) {
                               df$fill = data$fill[indexes]
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
                           compute_group = function(data, scales, angle = 0, r = .5, x_discrete = T, y_discrete = F) {
                             # angle takes the value defined in params
                             # print("Call from compute group")

                             classes_constraints <-  x_discrete ||  y_discrete

                             if(!classes_constraints) {
                               stop("There should be a discrete column and a numeric one")
                             }
                             # Convert angle from deg to rad
                             angle <- angle * pi / 180

                             # Base R Aprroach
                             circle_data <- data
                             # print(circle_data)
                             circle_data$nb_circles <- ifelse(x_discrete, circle_data$y %/% 1, circle_data$x %/% 1)
                             circle_data$circles_centers <- lapply(circle_data$nb_circles, \(x) 0:x)
                             circle_data$circles_values <- Map(\(X, Y) list(rep(1,X), Y%% 1), X = circle_data$nb_circles, Y = ifelse(x_discrete, circle_data$y, circle_data$x))
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

                             circle_data <- circle_data[circle_data$circles_values != 0,]
                             circle_data$id <- 1:nrow(circle_data)
                             # Base version ----
                             circle_data <- Map(
                               function(id, x, y, PANEL, group, nb_circles, circles_centers, circles_values){
                                 prop_val <- 1
                                 n <- 64

                                 final_angle <- (circles_values*2*pi)/prop_val
                                 nb_points_to <- (n*final_angle) %/% (2*pi)
                                 x_pos <- c(r*cos((0:nb_points_to)*(2*pi/n) + angle), r*cos(final_angle + angle))
                                 y_pos <- c(r*sin((0:nb_points_to)*(2*pi/n) + angle), r*sin(final_angle + angle))
                                 if (circles_values != 1) {
                                   x_pos <- c(0, x_pos)
                                   y_pos <- c(0, y_pos)
                                 }

                                 x <- ifelse(x_discrete, x, circles_centers) + x_pos
                                 y <- ifelse(x_discrete, circles_centers, y) + y_pos

                                 data.frame(id, x,y)
                               }, id = circle_data$id, x = circle_data$x, y = circle_data$y,
                               PANEL = circle_data$PANEL, group = circle_data$group,
                               nb_circles = circle_data$nb_circles,
                               circles_centers = circle_data$circles_centers,
                               circles_values = circle_data$circles_values
                             )
                             # Data passed to geom (Must contain columns needed "required" in geom)
                             circle_data <- do.call(rbind, circle_data)
                             # print(circle_data)
                             circle_data
                           },
                           # Mandatory in data passed to Stat
                           required_aes = c("x", "y")
)

#' @export
stat_serie_circle <- function(mapping = NULL, data = NULL, geom = "serie_circle",
                              position = "identity", show.legend = NA, na.rm = FALSE,
                              inherit.aes = TRUE,
                              angle = NULL,
                              r = NA,
                              ...) {
  ggplot2::layer(
    stat = StatSerieCircle,
    geom = geom,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
                  angle = angle,
                  r = r,
                  ...)
  )
}

# Serie Text ----
#' @usage NULL
#' @format NULL
 #' @export
StatSerieText  <- ggplot2::ggproto("StatSerieText", ggplot2::Stat,
                          compute_group = function(data, scales, flip = FALSE) {
                            # angle takes the value defined in params
                            x_class <- class(data$x)[1]
                            y_class <- class(data$y)[1]

                            x_discrete <- (x_class ==  "mapped_discrete" && y_class == "numeric")
                            y_discrete <- (y_class == "mapped_discrete" && x_class == "numeric")
                            classes_constraints <-  x_discrete ||  y_discrete

                            if(!classes_constraints) {
                              stop("There should be a discrete column and a numeric one")
                            }


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


#' @export
stat_serie_text <- function(mapping = NULL, data = NULL, geom = "serie_text",
                            position = "identity", show.legend = NA, na.rm = FALSE,
                            inherit.aes = TRUE,
                            ...) {
  ggplot2::layer(
    stat = StatSerieText,
    geom = geom,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
                  ...)
  )
}
