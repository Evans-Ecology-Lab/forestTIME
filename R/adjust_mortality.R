#' Adjust interpolated tables for mortality
#'
#' This is an "internal" function—most users will want to run [fia_annualize()]
#' instead. Trees in the input `data_interpolated` already have had their switch
#' to `STATUSCD` 2 (i.e. death) interpolated to the midpoint (rounded down)
#' between it's last survey alive and first survey dead.
#'
#' This does the following:
#' - Optionally figures out if a tree has a recorded `MORTYR` and uses that for
#'   the transition to `STATUSCD` 2 instead of the interpolated values. If the
#'   tree is alive (`STATUSCD` 1) in `MORTYR`, then it is assumed it died in the
#'   following year.
#' - Adjusts `STANDING_DEAD_CD` so that it only applies to dead trees
#' - Adjusts `DECAYCD` so that it only applies to standing dead trees
#' - Adjusts `DIA`, `HT`, `ACTUALHT`, `CULL`, and `CR` so that they only apply
#'   to live or standing dead trees in sampled conitions.
#'
#' @param data_interpolated tibble created by [interpolate_data()]
#' @param use_mortyr logical; use `MORTYR` (if recorded) as the first year a
#'   tree was dead?
#' @export
#' @keywords internal
#' @returns a tibble with additional columns `MORTYR_eff`, which is `MORTYR` but
#'   adjusted forward a year in the case when the tree was alive in the recorded
#'   `MORTYR`, and `midpt_dead`, which is just the midpoint year (rounded down)
#'   between the inventory year when a tree was last observed a live and the
#'   inventory year it was first observed dead.
adjust_mortality <- function(data_interpolated, use_mortyr = TRUE) {
  cli::cli_progress_step("Adjusting for mortality")

  # STATUSCD is already interpolated to the midpoint between surveys, but if
  # MORTYR is used it is more complicated

  if (isTRUE(use_mortyr)) {
    # If there aren't any recorded MORTYR, warn and skip the complicated stuff
    any_mortyr <- any(!is.na(data_interpolated$MORTYR))
    if (isFALSE(any_mortyr)) {
      cli::cli_warn(
        c(
          "!" = "No recorded {.var MORTYR} in data. ",
          "i" = "Setting {.arg use_mortyr} to `FALSE`"
        )
      )
      use_mortyr <- FALSE
    }
  }

  if (isTRUE(use_mortyr)) {
    # If a tree is marked as alive (STATUSCD 1) in the recorded MORTYR (i.e.
    # MORTYR is an inventory year), then assume death happened the year after
    # that inventory year
    # (https://github.com/Evans-Ecology-Lab/forestTIME-builder/issues/61)

    df <- data_interpolated |>
      dplyr::group_by(tree_ID) |>
      dplyr::mutate(
        MORTYR_eff = if_else(
          YEAR == MORTYR & STATUSCD == 1,
          MORTYR + 1,
          MORTYR
        ),
        .after = MORTYR
      ) |>
      # this "fills in" the new effective MORTYR for all rows since the above
      # if_else() only increments it for the inventory year row
      dplyr::mutate(MORTYR_eff = max(MORTYR_eff)) |>
      dplyr::mutate(
        # STATUSCD is interpolated to the midpoint between surveys
        # see utils.R for more info on what %|||% does
        midpt_dead = YEAR[min(which(STATUSCD == 2) %|||% NA)]
      ) |>
      # When a tree has a recorded MORTYR, adjust STATUSCD depending on whether
      # MORTYR is before or after the midpoint (midpt_dead). This works because
      # MORTYR is filled in for every row of a tree by fia_tidy() and
      # expand_data(). Can't assume tree has STATUSCD 2 after MORTYR since
      # sometimes STATUSCD goes from 1 to 2 to 0.
      dplyr::mutate(
        STATUSCD = dplyr::case_when(
          is.na(MORTYR_eff) ~ STATUSCD, #do nothing
          MORTYR_eff == midpt_dead ~ STATUSCD, #do nothing

          # if MORTYR is earlier than midpoint and the year is between the
          # MORTYR and the midpoint, adjust STATUSCD to 2
          MORTYR_eff < midpt_dead & YEAR >= MORTYR_eff & YEAR < midpt_dead ~ 2,

          # if MORTYR is after the midpoint and the year is between the midpoint
          # and MORTYR, adjust STATUSCD to 1
          MORTYR_eff > midpt_dead & YEAR < MORTYR_eff & YEAR >= midpt_dead ~ 1,
          .default = STATUSCD
        )
      ) |>
      # MORTYR might be earlier than the midpoint, so backfill NAs for DECAYCD
      # and STANDING_DEAD_CD. These will be corrected later anyways
      tidyr::fill(DECAYCD, STANDING_DEAD_CD, .direction = "up") |>
      # dplyr::select(-midpt_dead, -MORTYR_eff)
      dplyr::relocate(MORTYR_eff, midpt_dead, .after = MORTYR)
  } else {
    df <- data_interpolated |>
      dplyr::group_by(tree_ID) |>
      dplyr::mutate(
        midpt_dead = YEAR[min(which(STATUSCD == 2) %|||% NA)],
        .after = MORTYR
      )
  }

  # If trees are interpolated to below FIA thresholds for being measured, set
  # them to fallen dead. For most trees, this is DIA < 1 and ACTUALHT < 4.5.
  # For woodland species, the ACTUALHT threshold is 1. To figure out if a tree
  # is a woodland species, we need to pull in one of the ref tables
  # temporarily.

  ref_species <-
    REF_SPECIES |>
    dplyr::select(
      SPCD,
      JENKINS_SPGRPCD
    )

  data_adjusted <- df |>
    dplyr::left_join(ref_species, by = dplyr::join_by(SPCD)) |>
    # TODO: Is there a way of only having to do the case_when once?  E.g. would
    # it be faster to create a column "dead_fallen" and then in a subsequent
    # step use dead_fallen to set STATUSCD and STANDING_DEAD_CD? this function
    # feels a lot slower since adding this bit
    dplyr::mutate(
      STATUSCD = dplyr::case_when(
        JENKINS_SPGRPCD < 10 & (DIA < 1 | HT < 4.5 | ACTUALHT < 4.5) ~ 2,
        JENKINS_SPGRPCD == 10 & (DIA < 1 | HT < 1 | ACTUALHT < 1) ~ 2,
        .default = STATUSCD
      ),
      STANDING_DEAD_CD = dplyr::case_when(
        JENKINS_SPGRPCD < 10 & (DIA < 1 | HT < 4.5 | ACTUALHT < 4.5) ~ 0,
        JENKINS_SPGRPCD == 10 & (DIA < 1 | HT < 1 | ACTUALHT < 1) ~ 0,
        .default = STANDING_DEAD_CD
      )
    ) |>
    dplyr::select(-JENKINS_SPGRPCD)

  data_adjusted |>
    # adjust STATUSCD & DECAYCD

    # STANDING_DEAD_CD only applies to dead trees
    dplyr::mutate(
      STANDING_DEAD_CD = dplyr::if_else(STATUSCD == 2, STANDING_DEAD_CD, NA)
    ) |>
    # and DECAYCD only applies to standing dead trees
    dplyr::mutate(
      DECAYCD = dplyr::if_else(STANDING_DEAD_CD == 1, DECAYCD, NA)
    ) |>
    # fallen trees shouldn't have measurements for anything
    dplyr::mutate(
      dplyr::across(
        c(DIA, HT, ACTUALHT, CULL, CR),
        \(x) dplyr::if_else(STANDING_DEAD_CD == 0, NA, x, missing = x)
      )
    ) |>
    # trees in non-sampled areas shoudn't have measurements for anything
    # https://github.com/Evans-Ecology-Lab/forestTIME-builder/issues/59
    dplyr::mutate(
      dplyr::across(
        c(DIA, HT, ACTUALHT, CULL, CR),
        \(x) {
          dplyr::if_else(
            (STATUSCD == 0 & RECONCILECD %in% c(5, 6, 9)) |
              (COND_STATUS_CD != 1),
            NA,
            x,
            missing = x
          )
        }
      )
    ) |>
    dplyr::ungroup()
}
