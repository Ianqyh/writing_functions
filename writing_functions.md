Writing Functions
================

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.4     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   2.0.1     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
knitr::opts_chunk$set(
  fig.width = 6,
  fig.asp = .6,
  out.width = "90%"
)

theme_set(theme_minimal() + theme(legend.position = "bottom"))

options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d
```

## Z scores

``` r
x_vec = rnorm(25, mean = 5, sd = 4)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1]  0.3170342 -0.1854568 -0.5205818  1.4448100 -1.0683657 -1.3557269
    ##  [7]  0.1953831  1.6780192  1.0865032 -1.0229054 -0.4250197 -1.3653192
    ## [13] -1.1713521 -0.4501432 -0.1080340  0.5790005 -1.5769612  1.2098121
    ## [19] -0.9128797  1.1188704  0.4881263 -0.5967989  1.2929037  1.0408742
    ## [25]  0.3082076

``` r
z_scores = function(x) {
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
  
}

z_scores(x = x_vec)
```

    ##  [1]  0.3170342 -0.1854568 -0.5205818  1.4448100 -1.0683657 -1.3557269
    ##  [7]  0.1953831  1.6780192  1.0865032 -1.0229054 -0.4250197 -1.3653192
    ## [13] -1.1713521 -0.4501432 -0.1080340  0.5790005 -1.5769612  1.2098121
    ## [19] -0.9128797  1.1188704  0.4881263 -0.5967989  1.2929037  1.0408742
    ## [25]  0.3082076

``` r
y_vec = rnorm(40, mean = 12, sd = .3)

z_scores(y_vec)
```

    ##  [1] -0.88984748  1.07723154  1.47089527 -2.48560880 -0.61621091 -1.53797332
    ##  [7]  0.12472710  1.27505450 -0.40590521  0.73119571 -1.49802168  1.37780783
    ## [13]  0.05364937  0.23402859 -0.25756377  0.31112870  1.48630046 -1.23693025
    ## [19] -0.45286824  0.27879532  0.13049067 -0.74184271 -1.54191640  1.14656293
    ## [25]  0.11525146 -0.66907209  0.37301396 -0.10394788  0.79455537 -2.05175548
    ## [31] -0.03983655  1.28386217 -0.64573013  0.67991872  0.41329808  1.32152982
    ## [37]  0.18174821 -0.03064402 -0.52907992  0.87370904

How great is this?

Only kinda great.

Let’s try again.

``` r
z_scores = function(x) {
  
  if (!is.numeric(x)) {
    stop("x needs to be numeric")
  }
  
  if (length(x) < 3) {
    stop("x should have at least 3 numbers")
  }
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
}
```

``` r
z_scores(3)
```

    ## Error in z_scores(3): x should have at least 3 numbers

``` r
z_scores(c("my", "name", "is", "jeff"))
```

    ## Error in z_scores(c("my", "name", "is", "jeff")): x needs to be numeric

``` r
z_scores(mtcars)
```

    ## Error in z_scores(mtcars): x needs to be numeric

``` r
z_scores(x = y_vec)
```

    ##  [1] -0.88984748  1.07723154  1.47089527 -2.48560880 -0.61621091 -1.53797332
    ##  [7]  0.12472710  1.27505450 -0.40590521  0.73119571 -1.49802168  1.37780783
    ## [13]  0.05364937  0.23402859 -0.25756377  0.31112870  1.48630046 -1.23693025
    ## [19] -0.45286824  0.27879532  0.13049067 -0.74184271 -1.54191640  1.14656293
    ## [25]  0.11525146 -0.66907209  0.37301396 -0.10394788  0.79455537 -2.05175548
    ## [31] -0.03983655  1.28386217 -0.64573013  0.67991872  0.41329808  1.32152982
    ## [37]  0.18174821 -0.03064402 -0.52907992  0.87370904

## Multiple outputs

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("x needs to be numeric")
  }
  
  if (length(x) < 3) {
    stop("x should have at least 3 numbers")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  output_df =
    tibble(
      mean = mean_x,
      sd = sd_x
    )
  
  return(output_df)

}

mean_and_sd(x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.10  3.64

``` r
mean_and_sd(y_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  12.1 0.263
