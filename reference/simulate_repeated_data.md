# Simulate repeated data

Simulates `n_rep` repeated values in `n_sub` subjects.

## Usage

``` r
simulate_repeated_data(
  n_sub = 50,
  n_rep = 5,
  avg = 5,
  between_sub_sd = 0.5,
  within_sub_rel_change_sd = 0.3,
  var_name = "co_true"
)
```

## Arguments

- n_sub:

  A single number specifying the number of subjects.

- n_rep:

  A single number specifying the number of repetitions per subject.

- avg:

  A single number specifying the average value across subjects.

- between_sub_sd:

  A single number specifying the standard deviation between subjects.

- within_sub_rel_change_sd:

  A single number controlling the within-subject relative change
  variability.

- var_name:

  A single string specifying the name of the simulated variable column.

## Value

A data frame containing simulated data with `id` and the simulated
variable.
