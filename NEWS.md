# forestTIME.builder (development version)

- Fixed a bug in `fia_split_composite_ids()` that caused it to fail when `tree_ID` was `NA` (as it is in conditions with no observations). It now falls back on the information in `plot_ID` when `tree_ID` is in the data frame but `NA` ([#149](https://github.com/Evans-Ecology-Lab/forestTIME-builder/issues/149)).
- `fia_estimate()` now returns the additional variables `DRIBIO_FOLIAGE`, `VOLTSGRS`, and `VOLTSSND` in addition to `DRYBIO_AG` and `CARBON_AG`.
- Code to deal with negative extrapolated values has moved to `adjust_mortality()`.  Therefore, the results of `interpolate_data()` may now contain negative numbers, which are non-sensible.  Use `fia_annualize()` whenever possible to ensure sensible results.
- `fia_download()` arguments have changed.  `keep_zip` now defaults to `TRUE`.  `extract` options have changed from `TRUE`/`FALSE` to `"forestTIME"`, `"rFIA"`, `"all"`, or `"none"` to extract just files needed by `forestTIME`, those used by `rFIA` (for compatibility), all files, or none.
- Fixed a bug in interpolation of `CONDPROP_UNAJ`.  Now the workflow retains all "empty" conditions (i.e. `CONDID`s with no trees in them) and properly interpolates `CONDPROP_UNADJ` so the proportion for all conditions in a plot in a year sum to 1 (within rounding error) ([#64](https://github.com/Evans-Ecology-Lab/forestTIME-builder/issues/64)).
- `fia_annualize()` now adds and `EXPNS` column calculated as the total land area of the state in acres divided by the number of plots in the interpolated data.  It *should* be usable in the same ways the `EXPNS` column in the "raw" FIA data can be used.
- Renames the `state_codes` dataset to `state_areas` and adds a column for state land area in acres.

# forestTIME.builder 2.0.0

- The separate `prep_carbon()` and `estimate_carbon()` functions are no longer exported and are replaced by the combined `fia_estimate()` function.
- Added `fia_annualize()` which is a wrapper for `df |> expand_data() |> interpolate_data |> adjust_mortality()` and prefered over running each step separately as the individual steps contain artifacts of the annualization process.
- Renamed `split_composite_ids()` to `fia_split_composite_ids()`
- Renamed `add_composite_ids()` to `fia_add_composite_ids()`
- Renamed `prep_data()` to `fia_tidy()`
- Renamed `read_fia()` to `fia_load()`
- Renamed `get_fia_tables()` to `fia_download()`

# forestTIME.builder 1.1.0

- Fixed a bug causing the `INTENSITY` column (and possibly other plot-level variables) to be filled incorrectly by `expand_data()` ([#122](https://github.com/Evans-Ecology-Lab/forestTIME-builder/issues/122), reported by @brian-f-walters-usfs)
- `expand_data()` now converts `NA`s for `CULL` to 0s (this is what the carbon estimation code in `predictCRM2()` does already anyways) so that they are better interpolated.  `CULL` values are converted *back* to `NA` if `DIA` is < 5 after interpolation by `interpolate_data()` ([#77](https://github.com/Evans-Ecology-Lab/forestTIME-builder/issues/77)).
- `prep_data()` no longer filters out any rows (un-doing #59 and addressing #99).  If you want to remove certain rows, do this between `prep_data()` and `expand_data()`.
- Trees with only a single measurement have their single measurement carried forward during extrapolation rather than getting dropped from the data (#94, #99).
- In the case when `MORTYR` is an inventory year where the tree is alive (`STATUSCD` 1), it is now assumed by `adjust_mortality()` that the tree died in the year following `MORTYR` in order to keep the observation (#61).
- `interpolate_data()` no longer produces negative values for `HT`, `DIA`, or `ACTUALHT`.  Instead, trees that get extrapolated to have DIA < 1 or HT or ACTUALHT < 4.5 (or < 1 for woodland species) are assumed to be fallen and dead (`STATUSCD` 2 and `STANDING_DEAD_CD` 0). These fallen dead trees then have their measurements set to `NA` by `adjust_mortality()`. Therefore, `prep_carbon()` no longer filters out trees with negative values for `HT`. (Fixes #60).
- `adjust_mortality()` now assures trees in non-sampled conditions (`COND_STATUS_CD != 1`) don't have interpolated values.
- `estimate_carbon()` no longer modifies any columns and no longer filters out any rows. It only adds columns for biomass and carbon estimates (which may be `NA` if they couldn't be estimated) (finally fixes #63)
- `expand_data()` now fills `MORTYR` so it is constant for a particular tree.  NOTE this is different from how this column is populated in the raw data.
- Fixed a bug in `adjust_mortality()` that was causing trees that go from STATUSCD 2 to STATUSCD 0 (move to non-sampled area) to inapropriately have extrapolated values (#100 reported by @dnsteinberg)
- Fixed a bug in `expand_data()` that was caused `STANDING_DEAD_CD` and `DECAYCD` to not be interpolated correctly, resulting in extrapolated measurments for fallen dead trees (#101 reported by @dnsteinberg)
- `prep_data()` now converts `PLT_CN` from numeric to character for better readability in the output.
- Empty plots are no longer dropped silently by `prep_data()` and should be handled correctly by the rest of the workflow through `interpolate_data()`.
- `expand_data()` now adds a column, `interpolated`, that marks whether an observation was interpolated (`TRUE`) or in the original data (`FALSE`).
- Trees that have always been fallen and have no measurements are now removed by `prep_data()`
- Trees that change species (more than one `SPCD` value) are assumed to have always been their last recorded species.  `prep_data()` now overwrites `SPCD` with the last recorded `SPCD` for each tree.
- Additional columns `PLT_CN`, `COND_STATUS_CD` are kept for the interpolated data.
- Added a vignette (WIP) on how to use outputs of `forestTIME.builder` to get population level estimates.
- `forestTIME.builder` is now an R package
- Added functions `add_composite_ids()` and `split_composite_ids()` to deal with the composite ID columns `tree_ID` and `plot_ID`.  This should make it easier to join to other FIA tables.

# forestTIME-builder v1.0.0

- Refactored to not use databases and instead to produce a single table of interpolated data
- Added new "main" functions `get_fia_tables()`, `read_fia()`, `prep_data()`, `expand_data()`, `interpolate_data()`, `adjust_mortality()`, `prep_carbon()`, and `estimate_carbon()`
- Outlined the process of creating annualized data in `docs/pop_scaling.qmd`
- Added "null and length 0 coalescing operator" `%|||%` to `R/utils.R` which is used to suppress warnings that come from adjusting for mortality when a tree hasn't yet died.
- fixed bug where TPA_UNADJ wasn't getting joined for trees with DIA between 4.9 and 5 (#68)
- fixed a bug where categorical vars weren't interpolated correctly when switching from `NA`s (#72)
- trees that have had RECONCILECD 7 ("Cruiser error") or 8 ("Procedural change") at any inventory are now removed in `prep_data()` (#59)
- interpolates trees with STATUSCD 0 and RECONCILECD 5, 6 or 9 at t2 to midpoint between t1 and t2 and then removes them from sample (#59)

# forestTIME-builder v0.1.0

- Initial release with "pre-carbon" code to annualize the tree table, but not estimate carbon or biomass
