# Get EVALIDs and associated information for plots

Get information about the EVALIDs associated with plots from the various
`POP_*` tables.

## Usage

``` r
fia_eval_info(db)
```

## Arguments

- db:

  A list of tibbles produced by
  [`fia_load()`](https://evans-ecology-lab.github.io/forestTIME/reference/fia_load.md).

## Value

A tibble with variables that can be used for stratified estimation that
can be joined to annualized data.

## Examples

``` r
db <- fia_load("RI", dir = system.file("exdata", package = "forestTIME"))
fia_eval_info(db)
#> # A tibble: 14,396 × 22
#>    plot_ID  INVYR EVALID EVALID_YEAR ESTN_UNIT STRATUMCD STRATUM_CN ESTN_UNIT_CN
#>    <chr>    <int>  <int>       <int>     <int>     <int> <chr>      <chr>       
#>  1 1_44_1_…  2005 440600        2006         1         1 747529080… 19783603501…
#>  2 1_44_1_…  2004 440600        2006         1         5 747529120… 19783603501…
#>  3 1_44_1_…  2006 440600        2006         1         1 747529080… 19783603501…
#>  4 1_44_3_9  2006 440600        2006         1         1 747529080… 19783603501…
#>  5 1_44_3_…  2006 440600        2006         1         5 747529120… 19783603501…
#>  6 1_44_3_…  2005 440600        2006         2     12345 747529130… 19783603901…
#>  7 1_44_3_…  2004 440600        2006         1         5 747529120… 19783603501…
#>  8 1_44_3_…  2004 440600        2006         1         1 747529080… 19783603501…
#>  9 1_44_3_…  2003 440600        2006         1         5 747529120… 19783603501…
#> 10 1_44_3_…  2006 440600        2006         1         5 747529120… 19783603501…
#> # ℹ 14,386 more rows
#> # ℹ 14 more variables: STRATUM_DESCR <chr>, P1POINTCNT <int>,
#> #   ESTN_UNIT_DESCR <chr>, P1PNTCNT_EU <int>, AREA_USED <dbl>, EVAL_CN <chr>,
#> #   EVAL_GRP_CN <chr>, EVAL_DESCR <chr>, START_INVYR <int>, END_INVYR <dbl>,
#> #   ESTN_METHOD <chr>, EVAL_TYPs <chr>, EXPCURR <lgl>, EXPVOL <lgl>
```
