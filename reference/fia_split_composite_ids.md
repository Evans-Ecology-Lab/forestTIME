# Split composite ID columns

Splits the composite ID columns `tree_ID` and/or `plot_ID` into their
original component columns

## Usage

``` r
fia_split_composite_ids(data)
```

## Arguments

- data:

  A tibble with the `tree_ID` and/or `plot_ID` columns

## Value

The input tibble with additional columns `UNITCD`, `STATECD`,
`COUNTYCD`, `PLOT` and possibly `SUBP` and `TREE`.

## See also

[`fia_add_composite_ids()`](https://evans-ecology-lab.github.io/forestTIME/reference/fia_add_composite_ids.md)

## Examples

``` r
db <- fia_load("RI", dir = system.file("exdata", package = "forestTIME"))
data_tidy <- fia_tidy(db)
#> ℹ Wrangling data
#> ✔ Wrangling data [439ms]
#> 
fia_split_composite_ids(data_tidy)
#> # A tibble: 15,890 × 35
#>    UNITCD STATECD COUNTYCD PLOT  plot_ID    SUBP  TREE  tree_ID   INVYR MEASYEAR
#>    <chr>  <chr>   <chr>    <chr> <chr>      <chr> <chr> <chr>     <dbl>    <dbl>
#>  1 1      44      1        155   1_44_1_155 NA    NA    NA         2005     2005
#>  2 1      44      1        155   1_44_1_155 NA    NA    NA         2009     2009
#>  3 1      44      1        155   1_44_1_155 NA    NA    NA         2014     2014
#>  4 1      44      1        155   1_44_1_155 NA    NA    NA         2021     2021
#>  5 1      44      1        228   1_44_1_228 1     1     1_44_1_2…  2004     2003
#>  6 1      44      1        228   1_44_1_228 1     1     1_44_1_2…  2008     2009
#>  7 1      44      1        228   1_44_1_228 1     1     1_44_1_2…  2013     2013
#>  8 1      44      1        228   1_44_1_228 1     1     1_44_1_2…  2020     2020
#>  9 1      44      1        228   1_44_1_228 1     2     1_44_1_2…  2004     2003
#> 10 1      44      1        228   1_44_1_228 1     2     1_44_1_2…  2008     2009
#> # ℹ 15,880 more rows
#> # ℹ 25 more variables: PLT_CN <chr>, CONDID <int>, CONDPROP_UNADJ <dbl>,
#> #   PROP_BASIS <chr>, COND_STATUS_CD <int>, STDORGCD <int>, MORTYR <lgl>,
#> #   STATUSCD <int>, RECONCILECD <int>, DECAYCD <int>, STANDING_DEAD_CD <int>,
#> #   DIA <dbl>, CR <int>, HT <int>, ACTUALHT <int>, CULL <int>, SPCD <dbl>,
#> #   CARBON_AG <dbl>, DRYBIO_AG <dbl>, MACRO_BREAKPOINT_DIA <lgl>,
#> #   INTENSITY <int>, SUBCYCLE <int>, PLOT_STATUS_CD <int>, ECOSUBCD <chr>, …
```
