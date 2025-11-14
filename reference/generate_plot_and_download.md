# Plot at the UI and save

Plot at the UI and save

## Usage

``` r
generate_plot_and_download(
  data_type,
  reactive_data,
  output,
  input,
  manual,
  dose_plot,
  name_num
)
```

## Arguments

- data_type:

  gamma or neutron

- reactive_data:

  access to the reactive data

- output:

  connection with UI

- input:

  what you upload in the UI

- manual:

  logical, indicates if using manual input or file data.

- dose_plot:

  the number of the dose case to plot.

- name_num:

  associates a number to the name.

## Value

plot at UI and saving plot server logic
