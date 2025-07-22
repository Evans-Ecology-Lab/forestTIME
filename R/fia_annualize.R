#' Create annualized FIA data
#'
#' Converts tidied panel data into annualized data with interpolated measurments
#' for trees for years between inventories. This happens in three steps, which
#' can be "manually" replicated by chaining other `forestTIME.builder`
#' functions.
#'
#' First, data is expanded by [expand_data()] to add rows for years between
#' inventories for each tree in the data. Next, data is interpolated with
#' [interpolate_data()]. Finally, [adjust_mortality()] is applied. For trees
#' that die and/or fall between inventories, we adjust their history according
#' either to a recorded `MORTYR` (if `use_morty = TRUE`) or, as a fall-back, the
#' midpoint between surveys, rounded down. Unlike these intermediate functions,
#' `fia_annualize()` produces a dataset which can be safely used for other
#' analyses (with the caveat that all of this is experimental).
#'
#' @note Most users should use this "wrapper" function rather than running each
#' step separately since the intermediate steps may contain data artifacts.
#' However, one reason to use the stepwise workflow would be to save time when
#' generating interpolated data with and without using `MORTYR` as
#' `interpolate_data()` is the slowest step.
#'
#' @seealso For more details on each step, see: [expand_data()],
#'   [interpolate_data()], [adjust_mortality()]
#' @param data_tidy A tibble produced by [fia_tidy()].
#' @param use_mortyr logical; Use `MORTYR` (if recorded) as the first year a
#'   tree was dead? Passed to [adjust_mortality()].
#' @export
fia_annualize <- function(data_tidy, use_mortyr = TRUE) {
  
  # TODO: you *need* to move this into interpolate_data() so the step-wise
  # workflow still works

  # Interpolate COND table separately to account for the number of conditions
  # (CONDIDs) changing from year to year
  # https://github.com/Evans-Ecology-Lab/forestTIME-builder/issues/64

  cond <- data_tidy |>
    dplyr::group_by(plot_ID, INVYR, CONDID) |>
    dplyr::filter(!is.na(CONDID)) |>
    dplyr::summarize(
      CONDPROP_UNADJ = dplyr::first(CONDPROP_UNADJ), #they *should* all be the same
      .groups = "drop"
    )

  all_conds <- cond |>
    dplyr::group_by(plot_ID) |>
    tidyr::expand(
      CONDID = unique(CONDID),
      INVYR = unique(INVYR)
    )

  cond_complete <- dplyr::full_join(cond, all_conds) |>
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
    tidyr::expand(CONDID, YEAR = tidyr::full_seq(INVYR, 1))

  cond_expanded <- dplyr::right_join(
    cond_complete,
    cond_all_years,
    by = dplyr::join_by(plot_ID, CONDID, INVYR == YEAR)
  ) |>
    dplyr::arrange(plot_ID, INVYR, CONDID)

  cond_interpolated <-
    cond_expanded |>
    rename(YEAR = INVYR) |>
    group_by(plot_ID, CONDID) |>
    dplyr::mutate(
      CONDPROP_UNADJ = inter_extra_polate(
        YEAR,
        CONDPROP_UNADJ,
        extrapolate = FALSE
      )
    )
  # cond_interpolated is missing some CONDIDs because they apparently aren't all
  # there in the tidied data.  I thought I had fixed that and kept any rows with
  # no trees, but maybe I just kept *plots* with no trees and not plot * CONDID
  # combinations.

  data_interpolated <- data_tidy |>
    expand_data() |>
    interpolate_data()

  left_join(data_interpolated |> select(-CONDPROP_UNADJ), cond_interpolated) |>
    adjust_mortality(use_mortyr = use_mortyr)

}

