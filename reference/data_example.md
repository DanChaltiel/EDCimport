# Example databases

List of tables used in EDCimport examples:

- `edc_example()` can be used as the result of
  [`read_trialmaster()`](https://danchaltiel.github.io/EDCimport/reference/read_trialmaster.md)

- `edc_example_plot()` can be used to test
  [`edc_swimmerplot()`](https://danchaltiel.github.io/EDCimport/reference/edc_swimmerplot.md)

- `edc_example_mixed()` can be used to test
  [`split_mixed_datasets()`](https://danchaltiel.github.io/EDCimport/reference/split_mixed_datasets.md)

## Usage

``` r
edc_example_mixed(N = 100, seed = 42)

edc_example(N = 50, seed = 42)

edc_example_plot(N = 50, seed = 42)

edc_example_ae(N = 50, seed = 42)
```

## Arguments

- N:

  the number of patients

- seed:

  the random seed

## Value

a list of tables
