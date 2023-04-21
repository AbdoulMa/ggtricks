
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggtricks

<!-- badges: start -->
<!-- badges: end -->

{ggtricks} package is a collection of multiple geom presenting data in
the form of circle (at the moment, but many more to come and not only
circle oriented.) using grammar of graphics philosophy and Cartesian
coordinates system.

You have bench of functions to make sector charts where circle is
divided along it radii, so each section is proportional to value it
represents.

- `geom_pie` Pie charts
- `geom_donut` Donut charts (Pie chart with a hole)
- `geom_slice` Part of Pie charts
- `geom_donut_slice` Part of Donut charts

You also have a function, `geom_series_circles()` to draw what I call
series of circles, which draws for a category as many circles and
fraction of circles needed to represent the value represented by this
category. A companion function `geom_series_text` is defined to put
labels at limit of series circles as computing this limits positions can
be tedious depending on fragments circles starting angles.

## Installation

You can install the development version of ggtricks like so:

``` r
install.packages("ggtricks")
# or 
devtools::install_github("abdoulma/ggtricks")
```

## Examples

### `geom_series_circles`

### `geom_pie`

### `geom_donut`

### `geom_slice`

### `geom_donut_slice`

This is a basic example which shows you how to solve a common problem:

``` r
library(ggtricks)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.

## Limitations

As you surely noted, to generate circle, I use `coord_equal()`, using
`coord_cartesian()` will zoom the plot, not generating a appealing
circle shape even if the underlying drawn plot is a plot. So, we fix,
the `aspect ratio` to force :

> the physical representation of data units on the axes.

according to the official
[documentation](https://ggplot2.tidyverse.org/reference/coord_fixed.html).
Of course, you shouldn’t edit the default `ratio = 1` that ensures that
one unit on `x-axis` is the same length as one unit on the `y-axis`.

When using `geom_serie_circle()`, the desire will come one day to
combine it with `facet_wrap()` or `facet_grid` or whatever faceting
function, **you should not**, or not in the way you are thinking about.

As we use `coord_equal()`, you won’t be able to set `scales` parameter,
what I strongly suspect you to try to do. So for the moment, I don’t
recommend you to do so. Although, I give some tips to go through those
restrictions on package website
<https://www.abdoulma.github.io/ggtricks>

## Roadmap
