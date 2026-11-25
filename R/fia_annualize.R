#' Create annualized FIA data
#'
#' Converts tidied panel data into annualized data with interpolated measurements
#' for trees for years between inventories. This happens in three steps, which
#' can be "manually" replicated by chaining other `forestTIME`
#' functions.
#'
#' First, data is expanded by [expand_data()] to add rows for years between
#' inventories for each tree in the data. Next, [adjust_mortality()] is applied. For trees
#' that die and/or fall between inventories, we adjust their history according
#' either to a recorded `MORTYR` (if `use_morty = TRUE`) or, as a fall-back, the
#' midpoint between surveys, rounded down. Finally, data is interpolated with
#' [interpolate_data()]. Unlike these intermediate functions,
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
#'   [adjust_mortality()], [interpolate_data()]
#' @param data_tidy A tibble produced by [fia_tidy()].
#' @param year_var character indicating which year variable to use; default is `INVYR`.
#' @param use_mortyr logical; Use `MORTYR` (if recorded) as the first year a
#'   tree was dead? Passed to [adjust_mortality()]. The default for this argument is
#'   use_mortyr = false, midpoint mortality assignment.
#' @export
#' @examples
#' \dontrun{
#' db <- db <- fia_load("RI", dir = system.file("exdata", package = "forestTIME"))
#' data_tidy <- fia_tidy(db)
#' data_annualized <- fia_annualize(data_tidy)
#' }
#'
fia_annualize <- function(data_tidy, year_var = c("INVYR", "MEASYEAR"), use_mortyr = FALSE) {
  data_tidy |>
    expand_data(year_var = year_var) |>
    adjust_mortality(use_mortyr = use_mortyr) |>
    interpolate_data()
}
