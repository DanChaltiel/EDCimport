# Load a `.RData` file as a list

Instead of loading a `.RData` file in the global environment, extract
every object into a list.

## Usage

``` r
load_as_list(filename)
```

## Arguments

- filename:

  the filename, with the `.RData` extension.

## Value

a list

## Examples

``` r
x = list(a=1, b=mtcars)
save_list(x, "test.RData")
y = load_as_list("test.RData")
print(y$a)
#> [1] 1
```
