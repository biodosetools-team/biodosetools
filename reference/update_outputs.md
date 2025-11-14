# Update dose estimation box with yield values

Update dose estimation box with yield values

## Usage

``` r
update_outputs(num_cases, reactive_data, output, yield_fun, manual, table)
```

## Arguments

- num_cases:

  number of cases to estimate.

- reactive_data:

  to access reactive data.

- output:

  output for the UI.

- yield_fun:

  function for calculating yield, in utils_estimation.R

- manual:

  logical, indicates if using manual input or file data.

- table:

  the data input table.

## Value

yield per dose. Numeric
