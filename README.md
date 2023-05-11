
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidybernoulli

<!-- badges: start -->

<!-- badges: end -->

The goal of tidybernoulli is to …

## Installation

You can install the development version of tidybernoulli from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("EvaMaeRey/tidybernoulli")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(tidybernoulli)
## basic example code
```

``` r
bernoulli_trial()
#>   outcome prob
#> 1       0 0.75
#> 2       1 0.25

trial_init() |>
  trial_advance()
#> # A tibble: 4 × 4
#>   t1_outcome t1_prob t2_outcome t2_prob
#>        <int>   <dbl>      <int>   <dbl>
#> 1          0    0.75          0    0.75
#> 2          0    0.75          1    0.25
#> 3          1    0.25          0    0.75
#> 4          1    0.25          1    0.25


trial_init() |>
  trial_advance() |>
  trial_advance() 
#> # A tibble: 8 × 6
#>   t1_outcome t1_prob t2_outcome t2_prob t3_outcome t3_prob
#>        <int>   <dbl>      <int>   <dbl>      <int>   <dbl>
#> 1          0    0.75          0    0.75          0    0.75
#> 2          0    0.75          0    0.75          1    0.25
#> 3          0    0.75          1    0.25          0    0.75
#> 4          0    0.75          1    0.25          1    0.25
#> 5          1    0.25          0    0.75          0    0.75
#> 6          1    0.25          0    0.75          1    0.25
#> 7          1    0.25          1    0.25          0    0.75
#> 8          1    0.25          1    0.25          1    0.25
```

``` r
library(magrittr)
trial_init(prob = .3) %>%
  trial_advance() %>%
  trial_advance() %>%
  .$out %>%
  sum_across() %>%
  prod_across()
#> # A tibble: 8 × 8
#>   global_probs global_outcome t1_outcome t1_prob t2_ou…¹ t2_prob t3_ou…² t3_prob
#>          <dbl>          <dbl>      <int>   <dbl>   <int>   <dbl>   <int>   <dbl>
#> 1        0.343              0          0     0.7       0     0.7       0     0.7
#> 2        0.147              1          0     0.7       0     0.7       1     0.3
#> 3        0.147              1          0     0.7       1     0.3       0     0.7
#> 4        0.063              2          0     0.7       1     0.3       1     0.3
#> 5        0.147              1          1     0.3       0     0.7       0     0.7
#> 6        0.063              2          1     0.3       0     0.7       1     0.3
#> 7        0.063              2          1     0.3       1     0.3       0     0.7
#> 8        0.027              3          1     0.3       1     0.3       1     0.3
#> # … with abbreviated variable names ¹​t2_outcome, ²​t3_outcome
```

``` r
library(magrittr)
bernoulli_trial(prob = .5) %>%
  add_trials() %>%
  add_trials() %>%
  add_trials(5) %>%
  .$out %>%
  sum_across() %>%
  prod_across()
#> # A tibble: 128 × 16
#>    global_probs global…¹ t1_ou…² t1_prob t2_ou…³ t2_prob t3_ou…⁴ t3_prob t4_ou…⁵
#>           <dbl>    <dbl>   <int>   <dbl>   <int>   <dbl>   <int>   <dbl>   <int>
#>  1      0.00781        0       0     0.5       0     0.5       0     0.5       0
#>  2      0.00781        1       0     0.5       0     0.5       0     0.5       0
#>  3      0.00781        1       0     0.5       0     0.5       0     0.5       0
#>  4      0.00781        2       0     0.5       0     0.5       0     0.5       0
#>  5      0.00781        1       0     0.5       0     0.5       0     0.5       0
#>  6      0.00781        2       0     0.5       0     0.5       0     0.5       0
#>  7      0.00781        2       0     0.5       0     0.5       0     0.5       0
#>  8      0.00781        3       0     0.5       0     0.5       0     0.5       0
#>  9      0.00781        1       0     0.5       0     0.5       0     0.5       1
#> 10      0.00781        2       0     0.5       0     0.5       0     0.5       1
#> # … with 118 more rows, 7 more variables: t4_prob <dbl>, t5_outcome <int>,
#> #   t5_prob <dbl>, t6_outcome <int>, t6_prob <dbl>, t7_outcome <int>,
#> #   t7_prob <dbl>, and abbreviated variable names ¹​global_outcome, ²​t1_outcome,
#> #   ³​t2_outcome, ⁴​t3_outcome, ⁵​t4_outcome
```

``` r
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
bernoulli_trial(prob = .5) %>%
  add_trials() %>%
  add_trials() %>%
  add_trials(5) %>%
  .$out %>%
  sum_across() %>%
  prod_across() %>%
  group_by(global_outcome) %>%
  summarize(probs = sum(global_probs))
#> # A tibble: 8 × 2
#>   global_outcome   probs
#>            <dbl>   <dbl>
#> 1              0 0.00781
#> 2              1 0.0547 
#> 3              2 0.164  
#> 4              3 0.273  
#> 5              4 0.273  
#> 6              5 0.164  
#> 7              6 0.0547 
#> 8              7 0.00781
```
