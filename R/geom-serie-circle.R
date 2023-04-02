


draw_panel_function = function(data, panel_scales, coord, r = .5) {
  print("Call from draw panel")
  # print(data)
  max <- max(data$group, data$id)
  data$circle_id = generate_id(data$group, data$id, max)
  coords <- coord$transform(data, panel_scales)
  # view(coords)

  first_row <- coords[1, ]
  # Polygons of first 10 groups are
  # drawn first
  first_plotted <- (coords$group < 10) # & (coords$id <= 10)
  fills_1 <- coords[first_plotted,] # filter(group < 10)
  fills_1 <- fills_1[order(fills_1$group, fills_1$id), ] # arrange(id, group)
  fills_1 <- unique(fills_1[c("id", "group", "fill")]) # distinct(id, group, fill )
  fills_1 <- fills_1$fill

  fills_2 <- coords[!first_plotted,]
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
      lwd = first_row$linewidth * .pt,
      col = first_row$col
    )
  )
}

#' @export
GeomSerieCircle <- ggplot2::ggproto("GeomSerieCircle", ggplot2::GeomPolygon,
                           draw_panel = draw_panel_function,
                           # TODO Very important
                           # # Mandatory columns in draw panel function data
                           # draw_key = draw_key_serie_circle,
                           required_aes = c("x","y"),
                           # Mandatory to have Same that GeomPolygon  if we use GeomPolygon than Geom
                           # For addition can't be GeomPolygon / Need to override
                           # angle = 0,
                           default_aes = ggplot2::aes(colour = NA, fill = "#026841", linewidth = 0.5, linetype = 1,
                                             alpha = NA, subgroup = NULL),
                           rename_size = TRUE
)

#' @export
geom_serie_circle <-  function(mapping = NULL, data = NULL,
                               position = "identity", show.legend = NA,
                               na.rm = FALSE, inherit.aes = TRUE,
                               # For parameters
                               angle = 0, r = .5, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatSerieCircle,
    geom = GeomSerieCircle,
    position = position,
    show.legend =  show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, angle = angle, r = r, ...)
  )
}
