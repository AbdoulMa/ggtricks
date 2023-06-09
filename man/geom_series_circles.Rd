% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom-series-circles.R
\name{geom_series_circles}
\alias{geom_series_circles}
\title{Create a series of circles plot}
\usage{
geom_series_circles(
  mapping = NULL,
  data = NULL,
  show.legend = NA,
  na.rm = FALSE,
  inherit.aes = TRUE,
  init_angle = 0,
  r = 0.5,
  color = NA,
  linewidth = 0.5,
  ...
)
}
\arguments{
\item{mapping}{Set of aesthetic mappings created by \code{aes()} or
\code{aes_()}. If specified and \code{inherit.aes = TRUE} (the
default), it is combined with the default mapping at the top level of the
plot. You must supply \code{mapping} if there is no plot mapping.}

\item{data}{to be displayed in this layer}

\item{show.legend}{logical. Should this layer be included in the legends?
\code{NA}, the default, includes if any aesthetics are mapped.
\code{FALSE} never includes, and \code{TRUE} always includes.
It can also be a named logical vector to finely select the aesthetics to
display.}

\item{na.rm}{If \code{FALSE}, the default, missing values are removed with
a warning. If \code{TRUE}, missing values are silently removed.}

\item{inherit.aes}{If \code{FALSE}, overrides the default aesthetics,
rather than combining with them.}

\item{init_angle}{Circle drawing starting angle.}

\item{r}{Circle radius, should be <= 0.5.}

\item{color}{Color of circles/fragments of circles borders.}

\item{linewidth}{Size of circles/fragments of circles borders.}

\item{...}{other arguments passed on to \code{layer()}.}
}
\value{
A ggplot2 layer.
}
\description{
\code{geom_series_circles()} can be used as an alternative for
single or multiple bar charts. It consists of using
whole and fragments of circles to represent numerical values.
As it draws circles, the geom should use with \code{\link[ggplot2:coord_fixed]{ggplot2::coord_equal()}}
to maintain the "aspect ratio".

There are two are arguments absolutely needed in \code{aes()} mappings:
\itemize{
\item \code{x} A vector mapping the abscissa  axis \code{x},  i.e. a character vector
when \code{x} is a numerical vector, or  a numerical vector when \code{y} is a character
vector.
\item \code{y} A vector mapping the ordinate axis \code{y}, i.e. a numerical vector
when \code{x} is a character vector or vice versa.
There is a default mapping \code{fill} with  value \code{black} to fill  circles/fragments
of circles with. It can be used in  \code{aes} mapping or as  a global argument
for all the circles.
}
}
\examples{
my_df <- data.frame(cat = c("Apple", "Banana", "Pineapple"), val = c(2.65, 4.5, 6.25))
my_df |>
  ggplot2::ggplot() +
  geom_series_circles(ggplot2::aes(cat, val)) +
  ggplot2::coord_equal()

}
