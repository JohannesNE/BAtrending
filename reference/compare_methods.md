# Create Bland-Altman analysis

Creates a Bland-Altman analysis including trending agreement from a
dataset of simultaneous measurements with two methods (alt and ref).

## Usage

``` r
compare_methods(df, ref_col, alt_col, id_col, logtrans = FALSE, unit = NULL)
```

## Arguments

- df:

  Data frame. Each row contains simultaneous measurements of the same
  value.

- ref_col:

  name of the column containing reference measurements.

- alt_col:

  name of the column containing alternative measurements.

- id_col:

  name of the column containing unique subject id's.

- logtrans:

  Log-transform measurements before fitting the difference model.

- unit:

  Measurement unit (e.g. "L/min"). Currently only used for plots.

## Value

Bland-Altman analysis object (of class ba_analysis)

## Details

`compare_methods()` uses
[`lme4::lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html) to fit two
mixed models:

1.  A difference model describing the (dis)agreement between methods
    (alt - ref). This model is used to calculate the Bland-Altman
    statistics including trending parameters, accounting for repeated
    measurements between subjects.

2.  A distribution model descibing the distribution of the measured
    value, represented by the average ((alt + ref)/2). This model is
    used to descibe within-subject and between-subject variation in the
    measured variable (e.g. cardiac output).

## Examples

``` r
data(CO)
compare_methods(CO, ref_col = rv, alt_col = ic, id_col = sub)
#> 60 paired measurements in 12 subjects
#> 
#>                                    est 
#> === Distribution ===
#> Mean                           :   5.035  
#> Between-subject variation (SD) :   1.210  
#> Within-subject variation (SD)  :   0.283  
#> Total variation (SD)           :   1.243  
#> 
#> === Method comparison ===
#> Bias (alt - ref)               :  -0.705  
#> Between-subject variation (SD) :   0.934  
#> Within-subject variation (SD)  :   0.413  
#> Total variation (SD)           :   1.022  
#> Intraclass correlation
#> └ Between/Total variance       :   0.836  
#> Limits of agreement (95%)
#> ├ Upper limit                  :   1.298  
#> └ Lower limit                  :  -2.707  
#> Percentage error               :   0.398  
#> 
#> --- Trending ---
#> Within-subject perc. error     :   0.161  
#> Change LoA [±] (95%)           :   1.146  

if (FALSE) { # \dontrun{
compare_methods(CO, ref_col = "rv", alt_col = "ic", id_col = "sub") # also works
} # }
```
