# Create a table with the results of the Bland-Altman analysis

Uses `tinytable:tt()` to create a publication-ready table of the results
of the Bland-Altman analysis. By default, results from a log-transformed
analysis is exponentiated.

## Usage

``` r
BA_table(ba_obj, decimals = 2, decimals_pct = 1, keep_log_scale = FALSE)
```

## Arguments

- ba_obj:

  An object created with
  [`BAtrending::compare_methods()`](https://johannesne.github.io/BAtrending/reference/compare_methods.md).

- decimals:

  A single numeric value specifying the number of decimal places for
  estimates and confidence intervals.

- decimals_pct:

  A single numeric value specifying the number of decimal places for
  percentage statistics.

- keep_log_scale:

  Show log transformed differences. If `FALSE` (default), values and
  parameters are exponentiated.

## Value

A table of the estimates from the Bland-Altman analysis.
