
test_df <- data.frame(
  cat = c("A", "B", "C"),
  val = c(17,25, 35)
)

test_that("series circles build", {
  skip_if_not_installed("vdiffr")
  # https://github.com/r-lib/vdiffr
  serie_circles_plot <- ggplot2::ggplot(test_df) + geom_serie_circle(ggplot2::aes(cat, val))
  vdiffr::expect_doppelganger("Serie circles geom builds", serie_circles_plot)
})

