#' Read in and join all required tables
#'
#' Reads in all the tables needed for carbon estimation and population scaling
#' and joins them into a single table. Then, some additional data cleaning steps
#' are performed.
#' 1. Creates unique tree and plot identifiers (`tree_ID` and `plot_ID`,
#'   respectively).
#' 2. Fills in missing values for `ACTUALHT` with values from `HT` to prepare
#'   for interpolation.
#' 3. Overwrites `SPCD` with whatever the last value of `SPCD` is for each tree
#'   (to handle trees that change `SPCD`).
#' 4. Fills a tree's `MORTYR` column so every row contains the recorded
#'   mortality year.
#' 5. Filters to only include inventories that came after the start of the
#'    annual inventory in each state.
#'
#' @param db A list of tables produced by [fia_load()].
#' @export
#' @seealso [fia_add_composite_ids()]
#' @returns A tibble.
#' @examples
#' db <- fia_load("RI", dir = system.file("exdata", package = "forestTIME"))
#' data_tidy <- fia_tidy(db)
fia_tidy <- function(db) {
  # Select only the columns we need from each table, to keep things slim
  cli::cli_progress_step("Wrangling data")
  PLOTGEOM <-
    db$PLOTGEOM |>
    dplyr::mutate(CN = as.character(CN)) |>
    dplyr::select(PLT_CN = CN, INVYR, ECOSUBCD)

  PLOT <-
    db$PLOT |>
    #fix Oregon plot with incorrect UNITCD
    dplyr::mutate(UNITCD = dplyr::case_when(STATECD  == 41 &
                                     COUNTYCD == 49 &
                                     PLOT     == 60223 &
                                     INVYR    == 2002 &
                                     UNITCD   == 4 ~ 3,
                                     TRUE ~ UNITCD)) |>
    dplyr::mutate(CN = as.character(CN)) |>
    fia_add_composite_ids() |>
    dplyr::select(
      plot_ID,
      PLT_CN = CN,
      INVYR,
      MEASYEAR,
      MACRO_BREAKPOINT_DIA, #for assigning TPA_UNADJ
      INTENSITY,
      SUBCYCLE,
      PLOT_STATUS_CD
    )

  COND <-
    db$COND |>
    dplyr::mutate(PLT_CN = as.character(PLT_CN)) |>
    fia_add_composite_ids() |>
    dplyr::select(
      plot_ID,
      PLT_CN,
      INVYR,
      CONDID,
      CONDPROP_UNADJ,
      PROP_BASIS, #for assigning TPA_UNADJ
      COND_STATUS_CD,
      STDORGCD
    )

  TREE <-
    db$TREE |>
    dplyr::mutate(PLT_CN = as.character(PLT_CN)) |>
    fia_add_composite_ids() |>
    dplyr::select(
      plot_ID,
      tree_ID,
      INVYR,
      PLT_CN,
      CONDID,
      MORTYR,
      STATUSCD,
      RECONCILECD,
      DECAYCD,
      STANDING_DEAD_CD,
      DIA,
      CR,
      HT,
      ACTUALHT,
      CULL,
      SPCD,
      # Temporary workaround for woodland species is to linearly interpolate
      # carbon and biomass rather than re-calculated it.
      # TODO: Eventually remove these columns to avoid confusing users.
      CARBON_AG,
      DRYBIO_AG
    )

  # Join the tables
  data <-
    COND |>
    dplyr::as_tibble() |>
    dplyr::left_join(
      TREE,
      by = dplyr::join_by(plot_ID, PLT_CN, CONDID, INVYR)
    ) |>
    dplyr::left_join(PLOT, by = dplyr::join_by(plot_ID, PLT_CN, INVYR)) |>
    dplyr::left_join(PLOTGEOM, by = dplyr::join_by(INVYR, PLT_CN))

  # Only keep years that are part of the annual inventory in each state.
  data <- data |>
    fia_split_composite_ids() |>
    dplyr::left_join(annual_inventory_start, by = "STATECD") |>
    dplyr::filter(INVYR >= annual_inventory_start) |>
    dplyr::select(-UNITCD, -STATECD, -COUNTYCD, -PLOT, -SUBP, -TREE)

  # filter out subcycle 0 and 99 plots
  data <- data |>
    dplyr::filter(SUBCYCLE !=0 & SUBCYCLE !=99)

  # fill MORTYR so it is a property of trees
  data <- data |>
    dplyr::group_by(tree_ID) |>
    tidyr::fill(MORTYR, .direction = c("updown")) |>
    # if trees have more than one SPCD, set all to be the most recent SPCD
    # (https://github.com/Evans-Ecology-Lab/forestTIME/issues/53)
    dplyr::mutate(SPCD = dplyr::last(SPCD)) |>
    dplyr::ungroup()

  # filter out trees that were recorded due to cruiser error or procedural change.
  # identify trees to drop
  #trees_to_drop <- data |>
    #group_by(tree_ID) |>
    #arrange(INVYR, .by_group = TRUE) |>
    #summarize(
      #first_reconcile = first(RECONCILECD),
      #last_reconcile  = last(RECONCILECD),
      #n_records       = n(),
      #.groups = "drop"
    #) |>
    #filter(
      #last_reconcile %in% c(7, 8) &          # last measurement is 7 or 8
        #!(first_reconcile %in% c(7, 8)) &    # first measurement is NOT 7 or 8
        #n_records > 1                        # tree has earlier measurements
    #) |>
    #pull(tree_ID)

  #cli::cli_alert_warning(
    #"{length(trees_to_drop)} trees dropped due cruiser error or procedural change.")

  # drop them
  #data <- data |> filter(!tree_ID %in% trees_to_drop)


  # at this point, get the list of plots and years as following steps may remove
  # "empty" plots
  all_plots <- data |>
    dplyr::select(plot_ID, INVYR) |>
    dplyr::distinct() |>
    dplyr::left_join(PLOT |> dplyr::select(-MEASYEAR),
                     by = dplyr::join_by(plot_ID, INVYR))

  # coalesce ACTUALHT so it can be interpolated
  data <- data |>
    dplyr::mutate(ACTUALHT = dplyr::coalesce(ACTUALHT, HT))

  # fill NA values in MEASYEAR column with INVYR.
  data <- data |>
    dplyr::mutate(
      MEASYEAR_NA_flag = dplyr::if_else(
        is.na(MEASYEAR),
        "MEASYEAR filled with INVYR",
        NA_character_),
      MEASYEAR = dplyr::coalesce(MEASYEAR, INVYR))

  # join the empty plots back in
  data <-
    dplyr::full_join(
      data,
      all_plots,
      by = dplyr::join_by(
        plot_ID,
        PLT_CN,
        INVYR,
        MACRO_BREAKPOINT_DIA,
        INTENSITY,
        SUBCYCLE,
        PLOT_STATUS_CD
      )
    ) |>
    dplyr::arrange(plot_ID, tree_ID, INVYR) |>
    dplyr::select(plot_ID, tree_ID, INVYR, MEASYEAR, everything()) |>
    dplyr::select(-annual_inventory_start)

  # return:
  data
}

