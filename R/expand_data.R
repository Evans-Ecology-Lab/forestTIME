#' Expand data to include years between inventory years
#'
#' This is an "internal" function—most users will want to run [fia_annualize()]
#' instead. This expands the data frame in preparation for interpolation of now
#' "missing" values between inventory years or measurement years depending on the year variable selection. Time-invariant variables `tree_ID`,
#' `plot_ID`, `SPCD`, `MORTYR`, `ECOSUBCD`, `DESIGNCD`, and `PROP_BASIS` are
#' simply filled in with [tidyr::fill()]. Categorical variables `STATUDSCD`,
#' `RECONCILECD`, `STDORGCD`, `CONDID`, and `COND_STATUS_CD` are modified to
#' replace `NA`s with `999` so that they are properly interpolated by
#' [interpolate_data()] (which converts them back to `NA`s).
#'
#' @param data tibble produced by [fia_tidy()]---must have at least `tree_ID`
#'   and `INVYR` columns.
#' @param year_var character indicating which year variable to use; default is `INVYR`.
#' @export
#' @keywords internal
#' @returns a tibble with a logical column `interpolated` marking whether a row
#'   was present in the original data (`FALSE`) or was added (`TRUE`).
expand_data <- function(data, year_var = c("INVYR", "MEASYEAR")) {
  cli::cli_progress_step("Expanding years between surveys")

  year_var <- match.arg(year_var)

  # add column that identifies if YEAR is coming from INVYR or MEASYEAR
  data <- data |>
    dplyr::mutate(YEAR_SOURCE = year_var)

  data <- data |>
    # replace NAs for some categorical variables with 999 (temporarily) so they
    # switch from NA correctly
    # (https://github.com/Evans-Ecology-Lab/forestTIME/issues/72)
    dplyr::mutate(dplyr::across(
      any_of(c(
        "STATUSCD",
        "RECONCILECD",
        "DECAYCD",
        "STANDING_DEAD_CD",
        "STDORGCD",
        "CONDID",
        "COND_STATUS_CD",
        "PLOT_STATUS_CD"
      )),
      \(x) dplyr::if_else(is.na(x) & !is.na(tree_ID), 999, x)
    )) |>
    # replace NAs for CULL with 0s so they interpolate correctly
    # (https://github.com/Evans-Ecology-Lab/forestTIME/issues/77)
    dplyr::mutate(
      CULL = dplyr::if_else(is.na(CULL) & !is.na(tree_ID), 0, CULL)
    ) |>
    # Some rows are all NAs just to keep all CONDID x plot_ID combinations in
    # the data, but they interfere with interpolation because all tree_IDs of NA
    # get treated like the same tree. So we make them unique per CONDID and
    # later they get changed back to NA in interpolate_data()
    dplyr::mutate(
      tree_ID = dplyr::if_else(is.na(tree_ID), paste0("NA_", CONDID), tree_ID)
    )

  # create YEAR column to match the user defined year variable (either INVYR or MEASYEAR)
  data <- data |>
    dplyr::mutate(YEAR = !!rlang::sym(year_var))

  # if a tree or empty plot has the same measurement year for two different inventory years, drop the first for year_var = "MEASYEAR" only
  if (year_var == "MEASYEAR") {
    data <- data |>
      dplyr::group_by(plot_ID, tree_ID, MEASYEAR) |>
      dplyr::arrange(INVYR, .by_group = TRUE) |>
      dplyr::mutate(
        MEASYEAR_repeat_flag = dplyr::case_when(
          dplyr::n() == 2 &
            dplyr::n_distinct(INVYR) == 2 &
            dplyr::row_number() == 2 ~
            "repeat MEASYEAR; kept later INVYR",
          TRUE ~ NA_character_
        )
      ) |>
      dplyr::filter(
        !(dplyr::n() == 2 &
            dplyr::n_distinct(INVYR) == 2 &
            dplyr::row_number() == 1)
      ) |>
      dplyr::ungroup()
  }

  # create full time series of years from first observation to last
  all_yrs <-
    data |>
    dplyr::group_by(plot_ID, tree_ID) |>
    tidyr::expand(YEAR = tidyr::full_seq(YEAR, 1))

  # join while creating a flag indicating whether the row is from the original
  # data or a result of "expanding"
  tree_annual <-
    dplyr::right_join(
      data |> dplyr::mutate(interpolated = FALSE),
      all_yrs |> dplyr::mutate(interpolated = TRUE),
      by = dplyr::join_by(plot_ID, tree_ID, YEAR)
    ) |>
    dplyr::mutate(
      interpolated = dplyr::coalesce(interpolated.x, interpolated.y),
      .keep = "unused" #to remove interpolated.x and interpolated.y
    ) |>
    dplyr::arrange(tree_ID, YEAR) |>
    #fill down any time-invariant columns
    dplyr::group_by(plot_ID, tree_ID) |>
    tidyr::fill(
      any_of(c(
        "plot_ID",
        # "INTENSITY",
        "SPCD",
        "ECOSUBCD",
        "PROP_BASIS",
        "MACRO_BREAKPOINT_DIA",
        "MORTYR",
        "YEAR_SOURCE"
      )),
      .direction = "downup"
    ) |>
    dplyr::ungroup() |>
    #rearrange
    dplyr::select(
      any_of(c(
        "plot_ID",
        "tree_ID",
        "YEAR",
        "YEAR_SOURCE",
        "interpolated",
        "DIA",
        "HT",
        "ACTUALHT",
        "CR",
        "CULL"
      )),
      everything()
    )

  tree_annual
}
