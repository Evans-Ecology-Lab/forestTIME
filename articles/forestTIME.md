# forestTIME

``` r

library(forestTIME)
#> ! forestTIME is an experimental package and currently a work-in-progress. It is
#>   not an official product of the US Forest Service. MESSAGE CHANGE!
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> 
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> 
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
```

## Data Download

[`fia_download()`](https://evans-ecology-lab.github.io/forestTIME/reference/fia_download.md)
downloads zip files of all CSVs and extracts the necessary ones. It will
skip downloading if the files already exist.

``` r

fia_download(
  states = "DE",
  download_dir = "fia",
  keep_zip = TRUE
)
```

## Data Preparation

I need to make sure I have all the columns for population estimation
*and* all the columns for carbon estimation using the walker code
(documented better in `annual_carbon_estimation.qmd`).

[`fia_load()`](https://evans-ecology-lab.github.io/forestTIME/reference/fia_load.md)
is a wrapper around
[`rFIA::readFIA()`](https://www.doserlab.com/files/rFIA/reference/readFIA.html)
and reads in all the required tables as a list of data frames.

``` r

db <- fia_load(states = "DE", dir = "fia")
names(db)
```

    #> [1] "COND"                   "PLOT"                   "PLOTGEOM"
    #> [4] "POP_ESTN_UNIT"          "POP_EVAL_TYP"           "POP_EVAL"
    #> [7] "POP_PLOT_STRATUM_ASSGN" "POP_STRATUM"            "TREE"

[`fia_tidy()`](https://evans-ecology-lab.github.io/forestTIME/reference/fia_tidy.md)
gets all the columns needed into a single table and create the unique
plot and tree IDS, `plot_ID` and `tree_ID`.

``` r

data <- fia_tidy(db)
#> ℹ Wrangling data
#> ✔ Wrangling data [704ms]
#> 
data
```

Check that each tree has only 1 entry per year

``` r

n <- data |>
  group_by(tree_ID, INVYR) |>
  filter(!is.na(tree_ID)) |> #remove empty plots
  summarise(n = n(), .groups = "drop") |>
  filter(n > 1) |>
  nrow()
stopifnot(n == 0)
```

## Annualize

Annualization of the panel data is performed by
[`fia_annualize()`](https://evans-ecology-lab.github.io/forestTIME/reference/fia_annualize.md).
“Under the hood” this is running
[`expand_data()`](https://evans-ecology-lab.github.io/forestTIME/reference/expand_data.md)
followed by
[`interpolate_data()`](https://evans-ecology-lab.github.io/forestTIME/reference/interpolate_data.md)
and then
[`adjust_mortality()`](https://evans-ecology-lab.github.io/forestTIME/reference/adjust_mortality.md).
While you *can* run each of these steps separately, it is not generally
recommended as there are “artifacts” introduced by some steps that are
not “cleaned up” until later steps. One *good* reason to run this
step-wise would be if you are wanting to produce a result both with and
without using `MORTYR` since the slowest step is
[`interpolate_data()`](https://evans-ecology-lab.github.io/forestTIME/reference/interpolate_data.md).

``` r
data_midpt <- fia_annualize(data, use_mortyr = FALSE)
#> ℹ Adjusting for mortality
#> ℹ Interpolating between surveys
#> ℹ Expanding years between surveys
#> ✔ Expanding years between surveys [5.2s]
#> 
#> ℹ Interpolating between surveys
✔ Interpolating between surveys [19.6s]
#> 
#> ℹ Adjusting for mortality
✔ Adjusting for mortality [23.1s]
```

``` r
data_midpt_stepwise <- data |>
  expand_data() |> #has placeholder values (e.g. 999) for some variables to ensure correct interpolation
  interpolate_data() |> #has incorrect values for trees that die or fall between surveys
  adjust_mortality(use_mortyr = FALSE)
#> ℹ Adjusting for mortality
#> ℹ Interpolating between surveys
#> ℹ Expanding years between surveys
#> ✔ Expanding years between surveys [5.2s]
#> 
#> ℹ Interpolating between surveys
✔ Interpolating between surveys [18.4s]
#> 
#> ℹ Adjusting for mortality
✔ Adjusting for mortality [22.3s]
```

``` r

identical(data_midpt, data_midpt_stepwise)
#> [1] TRUE
```

Let’s break down what happens in each step.

[`expand_data()`](https://evans-ecology-lab.github.io/forestTIME/reference/expand_data.md)
expands the table to include all years between inventories.
Time-invariant columns including `plot_ID`, `SPCD`, `ECOSUBCD`,
`DESIGNCD`, `CONDID`, `INTENSITY` and `PROP_BASIS` are simply filled
down. Some `NA`s thar represent missing values in inventory years are
replaced with placeholder values (e.g. 999, 0 for `CULL`) to distinguish
them from the `NA`s between surveys to aid in linear interpolation in
later steps.

``` r

data_expanded <- expand_data(data)
#> ℹ Expanding years between surveys
#> ✔ Expanding years between surveys [5.4s]
#> 
data_expanded
```

[`interpolate_data()`](https://evans-ecology-lab.github.io/forestTIME/reference/interpolate_data.md)
then interpolates continuous and categorical variables between surveys.
Continuous variables are interpolated with
[`inter_extra_polate()`](https://evans-ecology-lab.github.io/forestTIME/reference/inter_extra_polate.md)
and categorical variables are interpolated with
[`step_interp()`](https://evans-ecology-lab.github.io/forestTIME/reference/step_interp.md).
[`interpolate_data()`](https://evans-ecology-lab.github.io/forestTIME/reference/interpolate_data.md)
also joins in the `TPA_UNADJ` column based on `DESIGNCD` and `DIA`. If
trees are interpolated to below FIA thresholds for being measured (DIA
\< 1, ACTUALHT \< 1 for woodland species and \< 4.5 for non-woodland
species), they are assumed to be fallen dead and have `STATUSCD` set to
2 and `STANDING_DEAD_CD` set to 0.

``` r

data_interpolated <- interpolate_data(data_expanded)
#> ℹ Interpolating between surveys
#> ✔ Interpolating between surveys [12.9s]
#> 
data_interpolated
```

[`adjust_mortality()`](https://evans-ecology-lab.github.io/forestTIME/reference/adjust_mortality.md)
adjusts all columns related to mortality (`STATUSCD`,
`STANDING_DEAD_CD`, `DECAYCD`, and measurements like `DIA`, `HT`,
`ACTUALHT`, `CULL`, `CR` etc.). E.g. `DECAYCD` only applies to standing
dead trees and `STANDING_DEAD_CD` only applies to trees with `STATUSCD`
2 (dead).

``` r

data_mortyr <- adjust_mortality(data_interpolated, use_mortyr = TRUE)
#> ℹ Adjusting for mortality
#> Warning: ! No recorded `MORTYR` in data.
#> ℹ Setting `use_mortyr` to `FALSE`
#> ✔ Adjusting for mortality [3.5s]
#> 
data_midpt <- adjust_mortality(data_interpolated, use_mortyr = FALSE)
#> ℹ Adjusting for mortality
#> ✔ Adjusting for mortality [3.5s]
#> 
all.equal(data_mortyr, data_midpt)
#> [1] TRUE
```

These tables will be identical in states like RI where `MORTYR` is never
used, but in states that have records for `MORTYR` these tables would
differ slightly for some subset of trees.

## Carbon Estimation

[`fia_allometry()`](https://evans-ecology-lab.github.io/forestTIME/reference/fia_allometry.md)
uses code provided by David Walker to calculate carbon and biomass
variables using the National Scale Volume and Biomass estimators (NSVB)
([Westfall et al. 2024](#ref-westfall2024)).

``` r

data_midpt_carbon <- fia_allometry(data_midpt)
#> ℹ Prepping for estimating carbon
#> ✔ Prepping for estimating carbon [404ms]
#> 
#> ⠙ Estimating carbon: prepping data
#> ⠹ Estimating carbon: predicting total stem wood and bark volume
#> ⠸ Estimating carbon: predicting merchantable stem wood volume
#> ⠼ Estimating carbon: predicting stump wood volume
#> ⠴ Estimating carbon: predicting sawlog stem wood volume
#> ⠦ Estimating carbon: predicting sawlog stem wood and bark volume
#> ⠧ Estimating carbon: predicting total biomass
#> ⠇ Estimating carbon: predicting foliage weight
#> ✔ Estimating carbon: harmonizing components [32.6s]
#> 
#> ℹ Joining carbon estimation results
#> ✔ Joining carbon estimation results [68ms]
#> 
data_midpt_carbon
```

Westfall, James A., John W. Coulston, Andrew N. Gray, et al. 2024. *A
National-Scale Tree Volume, Biomass, and Carbon Modeling System for the
United States*. <https://doi.org/10.2737/wo-gtr-104>.
