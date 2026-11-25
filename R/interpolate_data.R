#' Interpolate expanded and mortality adjusted tree data
#'
#' This is an "internal" function - most users will want to run [fia_annualize()] instead.
#'
#' This does the following
#' - Fills in `NA`s between survey years with either linear interpolation/extrapolation or by
#' switching categorical variables at the midpoint (rounded down) between surveys.
#' - Linear interpolation/extrapolation is accomplished with [inter_extra_polate()] and the
#' categorical variables are handled with [step_interp()].
#' - Converts temporary `999` values created by [expand_data()] back to `NA`s. This also
#'  assigns a value for `TPA_UNADJ` based on `DESIGNCD` and interpolated values of `DIA`
#' according to Appendix G of the FIADB user guide.
#' - Adjusts `STANDING_DEAD_CD` so that it only applies to dead trees
#' - Adjusts `DECAYCD` so that it only applies to standing dead trees
#' - Adjusts `DIA`, `HT`, `ACTUALHT`, `CULL`, and `CR` so that they only apply
#'   to live or standing dead trees in sampled conditions.
#'
#' #' @note If `HT` or `ACTUALHT` are extrapolated to values < 4.5 (or < 1 for
#' woodland species) OR `DIA` is extrapolated to < 1, the tree is marked as
#' fallen dead (`STATUSCD` 2 and `STANDING_DEAD_CD` 0). All measurements for
#' these trees will be removed (set to `NA`) by [adjust_mortality()]. Trees with
#' only one measurement have that measurement carried forward as appropriate
#' (e.g. until fallen and dead or in non-sampled condition).
#'
#' Since missing values for `CULL` are already assumed to be 0 by
#' [fia_allometry()], they are converted to 0s by [expand_data()] for better
#' linear interpolation here and then set back to `NA` if `DIA` < 5.
#'
#' @references Burrill, E.A., Christensen, G.A., Conkling, B.L., DiTommaso,
#' A.M., Kralicek, K.M., Lepine, L.C., Perry, C.J., Pugh, S.A., Turner, J.A.,
#' Walker, D.M., 2024. The Forest Inventory and Analysis Database User Guide
#' (NFI). USDA Forest Service.
#' <https://research.fs.usda.gov/understory/forest-inventory-and-analysis-database-user-guide-nfi>
#'
#' @param data_adjusted tibble produced by [adjust_data()]
#' @export
#' @keywords internal
#' @returns a tibble

interpolate_data <- function(data_adjusted) {
  cli::cli_progress_step("Interpolating between surveys")

  # variables to linearly interpolate/extrapolate
  cols_interpolate <- c(
    "ACTUALHT",
    "DIA",
    "HT",
    "CULL",
    "CR",
    # Temporary workaround to get carbon estimates for woodland species
    # TODO: eventually remove these
    "CARBON_AG",
    "DRYBIO_AG"
  )

  # variables that switch at the midpoint (rounded down) between surveys
  cols_midpt_switch <- c(
    "PLT_CN", #used to join to POP tables. Possibly a better way...
    "RECONCILECD",
    "STDORGCD",
    "CONDID",
    "COND_STATUS_CD",
    "INTENSITY",
    "SUBCYCLE",
    "PLOT_STATUS_CD"
  )

  # variables that switch at the mortality year
  cols_mort_switch <- c(
    "STATUSCD",
    "DECAYCD",
    "STANDING_DEAD_CD"
  )

  # Interpolate COND table separately to account for the number of conditions
  # (CONDIDs) changing from year to year
  # https://github.com/Evans-Ecology-Lab/forestTIME/issues/64
  cond <- data_adjusted |>
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

  cond_all_years <- cond_complete |>
    dplyr::group_by(plot_ID) |>
    tidyr::expand(
      CONDID,
      YEAR = tidyr::full_seq(YEAR, 1)
    )

  # Expand and interpolate the COND table to get CONDPROP_UNADJ values that sum
  # to 1 for each plot in a given year.
  cond_expanded <- dplyr::right_join(
    cond_complete,
    cond_all_years,
    by = dplyr::join_by(plot_ID, CONDID, YEAR)
  ) |>
    dplyr::arrange(plot_ID, YEAR, CONDID)

  cond_interpolated <- cond_expanded |>
    dplyr::group_by(plot_ID, CONDID) |>
    dplyr::mutate(
      CONDPROP_UNADJ = inter_extra_polate(
        YEAR,
        CONDPROP_UNADJ,
        extrapolate = FALSE
      ),
      COND_STATUS_CD = step_interp(COND_STATUS_CD)
    )


  # Interpolate the data
  data_interpolated <- data_adjusted |>
    dplyr::group_by(plot_ID, tree_ID) |>
    dplyr::mutate(

      # linear interpolation/extrapolation
      dplyr::across(
        dplyr::any_of(cols_interpolate),
        \(var) inter_extra_polate(YEAR, var)
      ),

      # interpolate to switch at midpoint
      dplyr::across(
        dplyr::any_of(cols_midpt_switch),
        step_interp
      )
    ) |>

    # convert 999 to NA for midpoint-switch vars
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(c(cols_midpt_switch, cols_mort_switch)),
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
    ) |>

    # make it so CR cannot be negative after interpolation. Set dead trees to NA for CR, Keep NA values as NAs, replace negative values with 0, and keep all other interpolated CR values.
    dplyr::mutate(
      CR = dplyr::case_when(
        STATUSCD == 2 ~ NA_real_,
        is.na(CR)     ~ NA_real_,
        CR < 0        ~ 0,
        TRUE          ~ CR
      )
    )

  # If trees are interpolated to below FIA thresholds for being measured, set
  # them to fallen dead. For most trees, this is DIA < 1 and ACTUALHT < 4.5.
  # For woodland species, the ACTUALHT threshold is 1. To figure out if a tree
  # is a woodland species, we need to pull in one of the ref tables
  # temporarily.
  ref_species <-
    REF_SPECIES |>
    dplyr::select(
      SPCD,
      JENKINS_SPGRPCD)

  data_interpolated <- data_interpolated |>
    dplyr::left_join(ref_species, by = dplyr::join_by(SPCD)) |>
    # TODO: Is there a way of only having to do the case_when once?  E.g. would
    # it be faster to create a column "dead_fallen" and then in a subsequent
    # step use dead_fallen to set STATUSCD and STANDING_DEAD_CD? this function
    # feels a lot slower since adding this bit
    dplyr::mutate(
      STATUSCD = dplyr::case_when(
        JENKINS_SPGRPCD < 10 &
          (DIA < 1 | HT < 4.5 | ACTUALHT < 4.5) ~ 2,
        JENKINS_SPGRPCD == 10 &
          (DIA < 1 | HT < 1 | ACTUALHT < 1) ~ 2,
        .default = STATUSCD
      ),
      STANDING_DEAD_CD = dplyr::case_when(
        JENKINS_SPGRPCD < 10 &
          (DIA < 1 | HT < 4.5 | ACTUALHT < 4.5) ~ 0,
        JENKINS_SPGRPCD == 10 &
          (DIA < 1 | HT < 1 | ACTUALHT < 1) ~ 0,
        .default = STANDING_DEAD_CD
      )
    ) |>
    dplyr::select(-JENKINS_SPGRPCD) |>

    # STANDING_DEAD_CD only applies to dead trees
    dplyr::mutate(
      STANDING_DEAD_CD = dplyr::if_else(
        STATUSCD == 2,
        STANDING_DEAD_CD,
        NA
      )
    ) |>

    # DECAYCD only applies to standing dead trees
    dplyr::mutate(
      DECAYCD = dplyr::if_else(
        STANDING_DEAD_CD == 1,
        DECAYCD,
        NA
      )
    ) |>

    # Fallen trees shouldn't have measurements for anything
    dplyr::mutate(
      dplyr::across(
        c(
          DIA,
          HT,
          ACTUALHT,
          CULL,
          CR,
          # Temporary workaround for woodland sp
          CARBON_AG,
          DRYBIO_AG
        ),
        \(x) dplyr::if_else(STANDING_DEAD_CD == 0, NA, x, missing = x)
      )
    ) |>

    # trees in non-sampled areas or that were included by mistake shouldn't have measurements for anything
    # https://github.com/Evans-Ecology-Lab/forestTIME/issues/59
    dplyr::mutate(
      dplyr::across(
        c(DIA,
          HT,
          ACTUALHT,
          CULL,
          CR,
          CARBON_AG,
          DRYBIO_AG),
        \(x) dplyr::if_else(
          (STATUSCD == 0 & RECONCILECD %in% c(5, 6, 7, 8, 9)) |
            (COND_STATUS_CD != 1),
          NA,
          x,
          missing = x
        )
      )
    )

  # merge the interpolated COND values back in
  data_adjusted_cond <- dplyr::full_join(
    data_interpolated |>
      dplyr::select(-CONDPROP_UNADJ, -COND_STATUS_CD),
    cond_interpolated,
    by = dplyr::join_by(plot_ID, CONDID, YEAR)
  )

  out <- data_adjusted_cond |>
    dplyr::mutate(
      tree_ID = dplyr::if_else(
        stringr::str_starts(tree_ID, "NA_"),
        NA,
        tree_ID
      )
    ) |>
    dplyr::arrange(plot_ID, tree_ID, YEAR, CONDID) |>
    # Rename CARBON_AG and DRYBIO_AG to avoid confusion
    dplyr::rename(
      CARBON_AG_interpolated = CARBON_AG,
      DRYBIO_AG_interpolated = DRYBIO_AG
    )

  out
}
