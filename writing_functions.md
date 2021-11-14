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

    ##  [1]  0.7158840 -0.1480795  0.8433106  1.9984010 -1.3369167  1.7390814
    ##  [7]  0.4591405 -0.3287075 -1.0591179  0.6816444 -0.9147937 -0.5316998
    ## [13] -0.4378052 -0.3987253 -0.6501230  0.7036884 -1.1432369 -1.4708581
    ## [19]  1.4873811  1.3124935  0.2729487 -1.0442146 -0.7277759 -0.5380465
    ## [25]  0.5161271

``` r
z_scores = function(x) {
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
  
}

z_scores(x = x_vec)
```

    ##  [1]  0.7158840 -0.1480795  0.8433106  1.9984010 -1.3369167  1.7390814
    ##  [7]  0.4591405 -0.3287075 -1.0591179  0.6816444 -0.9147937 -0.5316998
    ## [13] -0.4378052 -0.3987253 -0.6501230  0.7036884 -1.1432369 -1.4708581
    ## [19]  1.4873811  1.3124935  0.2729487 -1.0442146 -0.7277759 -0.5380465
    ## [25]  0.5161271

``` r
y_vec = rnorm(40, mean = 12, sd = .3)

z_scores(y_vec)
```

    ##  [1]  0.78893058  1.86757904 -0.29868157 -0.09286343  0.34879807 -0.49963410
    ##  [7]  0.20622712 -1.48510473  1.03656920 -0.91162261  1.73737635 -0.46496867
    ## [13]  0.17776468  0.72609528 -0.79495762  1.11551389  1.87083649 -0.29722427
    ## [19] -1.19429685  1.00259812 -0.53367031  0.85060318 -0.42255225  0.05230870
    ## [25]  1.01080297 -0.92735423 -1.23100215  0.36947597 -1.32968729 -1.06436181
    ## [31]  1.08843410 -0.41023495 -0.87281726 -0.42341293 -0.19167995  0.63969667
    ## [37] -2.22056849  1.07279377  0.87380660 -1.16951532

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

    ##  [1]  0.78893058  1.86757904 -0.29868157 -0.09286343  0.34879807 -0.49963410
    ##  [7]  0.20622712 -1.48510473  1.03656920 -0.91162261  1.73737635 -0.46496867
    ## [13]  0.17776468  0.72609528 -0.79495762  1.11551389  1.87083649 -0.29722427
    ## [19] -1.19429685  1.00259812 -0.53367031  0.85060318 -0.42255225  0.05230870
    ## [25]  1.01080297 -0.92735423 -1.23100215  0.36947597 -1.32968729 -1.06436181
    ## [31]  1.08843410 -0.41023495 -0.87281726 -0.42341293 -0.19167995  0.63969667
    ## [37] -2.22056849  1.07279377  0.87380660 -1.16951532

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
    ## 1  5.19  4.32

``` r
mean_and_sd(y_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  12.0 0.276

## Different sample sizes, means, sds

``` r
sim_data =
  tibble(
    x = rnorm(30, mean = 2, sd = 3)
  )
  
sim_data %>%
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.15  3.33

Let’s write a function that simulates data, computes the mean and sd.

``` r
sim_mean_sd = function(n, mu, sigma) {
  
  # do check on inputs
  
  sim_data =
  tibble(
    x = rnorm(n, mean = mu, sd = sigma)
  )
  
sim_data %>%
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
  
}


sim_mean_sd(n = 30, mu = 4, sigma = 3)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.66  3.59
