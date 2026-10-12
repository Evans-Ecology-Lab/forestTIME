# Read in and join all required tables

Reads in all the tables needed for carbon estimation and population
scaling and joins them into a single table. Then, some additional data
cleaning steps are performed.

1.  Creates unique tree and plot identifiers (`tree_ID` and `plot_ID`,
    respectively).

2.  Fills in missing values for `ACTUALHT` with values from `HT` to
    prepare for interpolation.

3.  Overwrites `SPCD` with whatever the last value of `SPCD` is for each
    tree (to handle trees that change `SPCD`).

4.  Fills a tree's `MORTYR` column so every row contains the recorded
    mortality year.

5.  Filters to only include inventories that came after the start of the
    annual inventory in each state.

## Usage

``` r
fia_tidy(db)
```

## Arguments

- db:

  A list of tables produced by
  [`fia_load()`](https://evans-ecology-lab.github.io/forestTIME/reference/fia_load.md).

## Value

A tibble.

## See also

[`fia_add_composite_ids()`](https://evans-ecology-lab.github.io/forestTIME/reference/fia_add_composite_ids.md)

## Examples

``` r
db <- fia_load("RI", dir = system.file("exdata", package = "forestTIME"))
data_tidy <- fia_tidy(db)
#> ℹ Wrangling data
#> ✔ Wrangling data [413ms]
#> 
```
