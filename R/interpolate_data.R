#' Interpolate expanded tree data
#'
#' This is an "internal" function—most users will want to run [fia_annualize()]
#' instead. Fills in `NA`s between survey years with either linear interpolation
#' / extrapolation or by switching categorical variables at the midpoint
#' (rounded down) between surveys. Linear interpolation/extrapolation is
#' accomplished with [inter_extra_polate()] and the categorical variables are
#' handled with [step_interp()]. Also converts temporary `999` values created by
#' [expand_data()] back to `NA`s.  This also assigns a value for `TPA_UNADJ`
#' based on `DESIGNCD` and interpolated values of `DIA` according to Appendix G
#' of the FIADB user guide and adds an `EXPNS` column equivalent to the one in
#' FIA, but accounting for the fact that the data now contains interpolated
#' plots (i.e. `EXPNS` = the state land area divided by the total number of
#' plots in that state in that year in the *interpolated* data).
#'
#' @note If `HT` or `ACTUALHT` are extrapolated to values < 4.5 (or < 1 for
#' woodland species) OR `DIA` is extrapolated to < 1, the tree is marked as
#' fallen dead (`STATUSCD` 2 and `STANDING_DEAD_CD` 0). All measurements for
#' these trees will be removed (set to `NA`) by [adjust_mortality()]. Trees with
#' only one measurement have that measurement carried forward as appropriate
#' (e.g. until fallen and dead or in non-sampled condition).
#'
#' Since missing values for `CULL` are already assumed to be 0 by
#' [fia_estimate()], they are converted to 0s by [expand_data()] for better
#' linear interpolation here and then set back to `NA` if `DIA` < 5.
#'
#' @references Burrill, E.A., Christensen, G.A., Conkling, B.L., DiTommaso,
#' A.M., Kralicek, K.M., Lepine, L.C., Perry, C.J., Pugh, S.A., Turner, J.A.,
#' Walker, D.M., 2024. The Forest Inventory and Analysis Database User Guide
#' (NFI). USDA Forest Service.
#' <https://research.fs.usda.gov/understory/forest-inventory-and-analysis-database-user-guide-nfi>
#'
#' @param data_expanded tibble produced by [expand_data()]
#' @export
#' @keywords internal
#' @returns a tibble
interpolate_data <- function(data_expanded) {
  cli::cli_progress_step("Interpolating between surveys")
  #variables to linearly interpolate/extrapolate
  cols_interpolate <- c(
    "ACTUALHT",
    "DIA",
    "HT",
    "CULL",
    "CR" #,
    #  "CONDPROP_UNADJ" #this gets interpolated separately
  )
  #variables that switch at the midpoint (rounded down) between surveys
  cols_midpt_switch <- c(
    "PLT_CN", #used to join to POP tables.  Possibly a better way...
    "STATUSCD",
    "RECONCILECD",
    "DECAYCD",
    "STANDING_DEAD_CD",
    "STDORGCD",
    "CONDID",
    "COND_STATUS_CD"
  )

  # Interpolate COND table separately to account for the number of conditions
  # (CONDIDs) changing from year to year
  # https://github.com/Evans-Ecology-Lab/forestTIME-builder/issues/64
  cond <- data_expanded |>
    dplyr::filter(interpolated == FALSE) |>
    dplyr::group_by(plot_ID, YEAR, CONDID, COND_STATUS_CD) |>
    dplyr::filter(!is.na(CONDID)) |>
    dplyr::summarize(
      CONDPROP_UNADJ = dplyr::first(CONDPROP_UNADJ), #they *should* all be the same
      .groups = "drop"
    )

  # Make it so each plot has every CONDID in every year and if the CONDID wasn't
  # in the original data, just set CONDPROP_UNADJ to 0.  This is all so linear
  # interpolation works correctly
  all_conds <- cond |>
    dplyr::group_by(plot_ID) |>
    tidyr::expand(
      CONDID = unique(CONDID),
      YEAR = unique(YEAR)
    )

  cond_complete <- dplyr::full_join(
    cond,
    all_conds,
    by = dplyr::join_by(plot_ID, YEAR, CONDID)
  ) |>
    dplyr::mutate(
      CONDPROP_UNADJ = dplyr::if_else(
        is.na(CONDPROP_UNADJ) & !is.na(CONDID),
        0,
        CONDPROP_UNADJ
      )
    )

  cond_all_years <-
    cond_complete |>
    dplyr::group_by(plot_ID) |>
    tidyr::expand(CONDID, YEAR = tidyr::full_seq(YEAR, 1))

  # Expand and interpolate the COND table to get CONDPROP_UNADJ values that sum
  # to 1 for each plot in a given year.

  cond_expanded <- dplyr::right_join(
    cond_complete,
    cond_all_years,
    by = dplyr::join_by(plot_ID, CONDID, YEAR)
  ) |>
    dplyr::arrange(plot_ID, YEAR, CONDID)

  cond_interpolated <-
    cond_expanded |>
    dplyr::group_by(plot_ID, CONDID) |>
    dplyr::mutate(
      CONDPROP_UNADJ = inter_extra_polate(
        YEAR,
        CONDPROP_UNADJ,
        extrapolate = FALSE
      ),
      COND_STATUS_CD = step_interp(COND_STATUS_CD)
    )

  # interpolate the data
  data_interpolated <- data_expanded |>
    dplyr::group_by(plot_ID, tree_ID) |>
    dplyr::mutate(
      #linearly interpolate/extrapolate
      dplyr::across(
        dplyr::any_of(cols_interpolate),
        \(var) inter_extra_polate(x = YEAR, y = var)
      ),
      #interpolate to switch at midpoint
      dplyr::across(dplyr::any_of(cols_midpt_switch), step_interp)
    ) |>
    # convert 999 back to NA for some vars
    dplyr::mutate(dplyr::across(
      dplyr::any_of(cols_midpt_switch),
      \(x) dplyr::if_else(x == 999, NA, x)
    )) |>
    dplyr::ungroup() |>
    # Cull only measured for trees with DIA >= 5
    dplyr::mutate(CULL = dplyr::if_else(DIA < 5, NA, CULL)) |>
    # Populate TPA_UNADJ based on PROP_BASIS and cutoffs
    dplyr::mutate(
      TPA_UNADJ = dplyr::case_when(
        DIA >= 1 & DIA < 5 ~ 74.965282,
        PROP_BASIS == "SUBP" & DIA >= 5 ~ 6.018046,
        PROP_BASIS == "MACR" & DIA >= 5 & DIA < MACRO_BREAKPOINT_DIA ~ 6.018046,
        PROP_BASIS == "MACR" & DIA >= MACRO_BREAKPOINT_DIA ~ 0.999188
      )
    )

  # merge the interpolated COND values back in
  data_adjusted_cond <- dplyr::full_join(
    data_interpolated |> dplyr::select(-CONDPROP_UNADJ, -COND_STATUS_CD),
    cond_interpolated,
    by = dplyr::join_by(plot_ID, CONDID, YEAR)
  )

  # merge in land areas and calculate EXPNS
  out <- data_adjusted_cond |>
    dplyr::mutate(
      #get STATECD out of plot_ID
      STATECD = as.numeric(stringr::str_extract(plot_ID, "\\d+(?=_)")),
      # .before = plot_ID
    ) |>
    dplyr::left_join(
      state_areas |> dplyr::select(STATECD, state_land_area),
      by = dplyr::join_by(STATECD)
    ) |>
    dplyr::group_by(YEAR, STATECD) |>
    dplyr::mutate(
      EXPNS = state_land_area / length(unique(plot_ID))
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-STATECD, -state_land_area) |>
    # Switch tree_ID for empty conditions back to NA
    dplyr::mutate(
      tree_ID = dplyr::if_else(stringr::str_starts(tree_ID, "NA_"), NA, tree_ID)
    ) |>
    dplyr::arrange(plot_ID, tree_ID, YEAR, CONDID)

  # return:
  out
}
