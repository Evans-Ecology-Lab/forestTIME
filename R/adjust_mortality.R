#' Adjust expanded tables for mortality
#'
#' This is an "internal" function most users will want to run [fia_annualize()] instead.
#'
#' Under the FIA sampling design, trees are observed only at discrete inventory years. When a
#' tree changes status between two observations (e.g., live -> standing dead, standing dead ->
#' fallen), the exact transition year is generally unknown.
#'
#' This function inserts mortality transitions into the expanded dataset before interpolation
#' occurs so that mortality timing is consistent.
#'
#' This function does the following:
#' - Optionally figures out if a tree has a recorded `MORTYR` and uses that for the transition
#' to `STATUSCD` 2 instead of the interpolated values. If the tree status is alive (`STATUSCD`
#' 1) in `MORTYR`, then it is assumed it died in the following year
#' - Prepars dataset for trees that experience a `STATUSCD` transition for interpolation
#' (regarless of midpoint vs MORTYR selection) by moving t2 values between observations to the
#' transition year between measurements. This prevents dead trees from growing.
#'
#' Things that can happen to a tree from t1 to t2
#'
#' Live (STATUSCD = 1, STANDING_DEAD_CD = NA) -> Live (STATUSCD = 1, STANDING_DEAD_CD = NA)
#' - If a tree is alive at t1 and at t2 then no adjustment is needed.
#'
#' Live (STATUSCD = 1, STANDING_DEAD_CD = NA) -> Standing dead (STATUSCD = 2, STANDING_DEAD_CD = 1)
#' - If a tree goes from alive to standing dead, the observed dead-tree measurements from the
#' second observation are moved back to the transition year so that interpolation occurs
#' between the live measurements at t1 and the dead measurements at the transition year
#' (either midpoint or MORTYR). After that point, the tree's standing dead measurements are
#' held constant until the next observation.
#'
#' Standing Dead (STATUSCD = 2, STANDING_DEAD_CD = 1) - > Standing Dead (STATUSCD = 2, STANDING_DEAD_CD = 1)
#' - Standing dead trees cannot grow. If a later observation has larger values (potentially
#' due to measurement error) than the ealier standing dead observations, the earlier values
#' are carried forward, replacing the larger, later values. If later observations are smaller,
#' the tree can "shrink" and interpolation continues downward until the tree drops out at the
#' minimum DIA threshold of 1cm.
#'
#' Dropout transition
#' - Live (STATUSCD = 1, STANDING_DEAD_CD = NA) -> Fallen Dead (STATUSCD = 2, STANDING_DEAD_CD
#' = 0)
#' - Live (STATUSCD = 1, STANDING_DEAD_CD = NA) -> Nonsampled (STATUSCD = 0, STANDING_DEAD_CD =
#' NA)
#' - Standing Dead (STATUSCD = 2, STANDING_DEAD_CD = 1) -> Fallen Dead (STATUSCD = 2,
#' STANDING_DEAD_CD = 0)
#' - Standing Dead (STATUSCD = 2, STANDING_DEAD_CD = 1) -> Nonsampled (STATUSCD = 0,
#' STANDING_DEAD_CD = NA)
#'
#' These transitions are treated as removals from the sample population. The last observed
#' measurements are copied to the transition year rather than assigning NA values. This results
#' in flat interpolation between the last observed year and the dropout year.
#' This prevents interpolation/ extrapolation beyond the last observation and holds tree
#' measurements constant until the tree leaves the sample.
#'
#' use_mortyr argument
#' - If you want the inferred year of death to be assigned to the field estimated year of
#' mortality use use_mortyr = TRUE.
#' - If you want the inferred year of death to be assigned to the midpoint between plot
#' observations use use_mortyr = FALSE. THIS IS THE DEFAULT. If nothing is specified when
#' the function is run then midpoint mortality assignment will be used.
#'
#' @param data_expanded tibble created by [expand_data()]
#' @param use_mortyr logical; use `MORTYR` (if recorded) as the first year a
#'   tree was dead? The default for this argument is
#'   use_mortyr = false, midpoint mortality assignment.
#' @export
#' @keywords internal
#' @returns a tibble

adjust_mortality <- function(data_expanded, use_mortyr = FALSE) {

  cli::cli_progress_step("Adjusting for mortality")

  # Use MORTYR only when requested
  if (isTRUE(use_mortyr)) {

    # Check whether MORTYR exists in the dataset
    any_mortyr <- any(!is.na(data_expanded$MORTYR))

    if (isFALSE(any_mortyr)) {

      cli::cli_warn(
        c(
          "!" = "No recorded {.var MORTYR} in data.",
          "i" = "Setting {.arg use_mortyr} to `FALSE`"
        )
      )

      # Midpoint-based transition timing
      use_mortyr <- FALSE

    }

  }

  # Group records by tree and chronological order
  data_adjusted <- data_expanded |>
    dplyr::group_by(tree_ID) |>
    dplyr::arrange(YEAR, .by_group = TRUE)

  if (isTRUE(use_mortyr)) {

    data_adjusted <- data_adjusted |>

      # If the recorded mortality year matches the final live census, shift MORTYR forward by one year so that the tree status remains alive during it's last observed inventory.
      dplyr::mutate(
        MORTYR_eff = dplyr::if_else(
          YEAR == MORTYR & STATUSCD == 1,
          MORTYR + 1,
          MORTYR
        ),
        .after = MORTYR
      ) |>

      # Each tree should only have one MORTYR if MORTYR is recorded. Store a single effective mortality year across all records belonging to each tree
      dplyr::mutate(
        MORTYR_eff = if (all(is.na(MORTYR_eff))) {
          NA_real_
        } else {
          max(MORTYR_eff, na.rm = TRUE)
        }
      )

  } else {

    # When midpoint timing is used rather than MORTYR, populate MORTYR_eff with NA. The actual transition year will later be assinged at the interval midpoint.
    data_adjusted <- data_adjusted |>
      dplyr::mutate(
        MORTYR_eff = NA_real_
      )

  }

  data_adjusted <- data_adjusted |>

    dplyr::group_modify(\(.x, .y) {

      # Measured inventory records (non-interpolated observations) define the start and end points for interpolation intervals.
      obs_rows <- which(!.x$interpolated)

      if (length(obs_rows) < 2) {
        return(.x)
      }

      numeric_vars <- c(
        "DIA",
        "HT",
        "ACTUALHT",
        "CULL",
        "CR",
        "CARBON_AG",
        "DRYBIO_AG"
      )

      mortality_vars <- c(
        "STATUSCD",
        "STANDING_DEAD_CD",
        "DECAYCD"
      )

      # Evaluate each interval bounded by two observed inventories
      for (i in seq_len(length(obs_rows) - 1)) {

        t1_row <- obs_rows[i]
        t2_row <- obs_rows[i + 1]

        t1_year <- .x$YEAR[t1_row]
        t2_year <- .x$YEAR[t2_row]

        # Midpoint year used when no MORTYR available
        midpoint <- floor(
          (t1_year + t2_year) / 2
        )

        mortyr_eff <- .x$MORTYR_eff[t2_row]

        # Determine the year where mortality status should switch
        switch_year <- dplyr::case_when(
          !use_mortyr ~ midpoint,
          is.na(mortyr_eff) ~ midpoint,
          mortyr_eff <= t1_year ~ midpoint,
          TRUE ~ mortyr_eff
        )

        switch_row <- which(
          .x$YEAR == switch_year
        )[1]

        if (is.na(switch_row)) {
          next
        }

        # Status at the beginning and end of the interval
        status_t1 <- .x$STATUSCD[t1_row]
        status_t2 <- .x$STATUSCD[t2_row]

        sd_t1 <- .x$STANDING_DEAD_CD[t1_row]
        sd_t2 <- .x$STANDING_DEAD_CD[t2_row]

        # Transition-state flags
        live_t1 <- isTRUE(status_t1 == 1)

        standing_t1 <- isTRUE(
          status_t1 == 2 &&
            sd_t1 == 1
        )

        standing_t2 <- isTRUE(
          status_t2 == 2 &&
            sd_t2 == 1
        )

        fallen_t2 <- isTRUE(
          status_t2 == 2 &&
            sd_t2 == 0
        )

        out_t2 <- isTRUE(
          status_t2 == 0
        )

        # If a tree goes from live -> standing dead
        # Assign dead tree t2 measurements and mortality status to transition year
        if (live_t1 && standing_t2) {

          .x[switch_row, numeric_vars] <-
            .x[t2_row, numeric_vars]

          .x[switch_row, mortality_vars] <-
            .x[t2_row, mortality_vars]

        }

        # If a tree goes from standing dead -> standing dead (dead trees cannot grow).
        # If observed values increase, replace with the previous dead tree measurements.
        if (standing_t1 && standing_t2) {

          for (v in numeric_vars) {

            t1_val <- .x[[v]][t1_row]
            t2_val <- .x[[v]][t2_row]

            if (
              !is.na(t1_val) &&
              !is.na(t2_val) &&
              t2_val > t1_val
            ) {

              .x[[v]][t2_row] <- t1_val

            }

          }

        }

        # Any transition that removes tree from sample
        drops_out <-

          (live_t1 && fallen_t2) ||

          (live_t1 && out_t2) ||

          (standing_t1 && fallen_t2) ||

          (standing_t1 && out_t2)

        if (drops_out) {

          # Apply mortality status at the transition year
          .x[switch_row, mortality_vars] <-
            .x[t2_row, mortality_vars]

          # Hold last observed measurements constant until the dropout year
          .x[switch_row, numeric_vars] <-
            .x[t1_row, numeric_vars]

        }

      }

      .x

    }) |>

    # Carry mortality status forward after transitions
    tidyr::fill(
      dplyr::any_of(
        c(
          "STATUSCD",
          "STANDING_DEAD_CD",
          "DECAYCD"
        )
      ),
      .direction = "down"
    ) |>

    dplyr::select(-MORTYR_eff) |>

    dplyr::ungroup()

  data_adjusted

}
