error_serie_df <- data.frame(
  cat = c(.5, .2, .4),
  val = sample(1:10, 3)
)

test_df <- data.frame(
  cat = c("A", "B", "C"),
  val = c(17, 25, 35)
)

# test_that("serie circle only accepts a discret and numerical columns",{
#   expect_error(ggplot2::ggplot(error_serie_df) + geom_serie_circle(ggplot2::aes(cat, val)), fixed = TRUE)
# })

# test_that("series circles builds properly", {
#   skip_if_not_installed("vdiffr")
#   # https://github.com/r-lib/vdiffr
#   serie_circles_plot <- ggplot2::ggplot(test_df) +
#     geom_series_circles(ggplot2::aes(cat, val))
#   vdiffr::expect_doppelganger("Serie circles geom builds", serie_circles_plot)
# })


test_that("geoms build properly.", {
  expect_no_error(ggplot2::ggplot(test_df) +
    geom_series_circles(ggplot2::aes(cat, val)))
  expect_no_error(ggplot2::ggplot(test_df) +
    geom_pie(ggplot2::aes(cat = cat, val = val)))
  expect_no_error(ggplot2::ggplot(test_df) +
    geom_donut(ggplot2::aes(cat = cat, val = val)))
  expect_no_error(ggplot2::ggplot(test_df) +
    geom_slice(ggplot2::aes(cat = cat, val = val)))
  expect_no_error(ggplot2::ggplot(test_df) +
    geom_donut_slice(ggplot2::aes(cat = cat, val = val)))
})
