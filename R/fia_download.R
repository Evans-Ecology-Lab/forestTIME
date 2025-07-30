#these are the tables we need
tables_ft <- c(
  "PLOT",
  "COND",
  "TREE",
  "PLOTGEOM",
  "POP_ESTN_UNIT",
  "POP_EVAL",
  "POP_EVAL_TYP",
  "POP_PLOT_STRATUM_ASSGN",
  "POP_STRATUM"
)

tables_rfia <- c(
  "COND",
  "COND_DWM_CALC",
  "INVASIVE_SUBPLOT_SPP",
  "PLOT",
  "POP_ESTN_UNIT",
  "POP_EVAL",
  "POP_EVAL_GRP",
  "POP_EVAL_TYP",
  "POP_PLOT_STRATUM_ASSGN",
  "POP_STRATUM",
  "SUBPLOT",
  "TREE",
  "TREE_GRM_COMPONENT",
  "TREE_GRM_MIDPT",
  "TREE_GRM_BEGIN",
  "SUBP_COND_CHNG_MTRX",
  "SEEDLING",
  "SURVEY",
  "SUBP_COND",
  "P2VEG_SUBP_STRUCTURE"
)

#' Download zip files from FIA datamart
#'
#' The zip files are smaller than just the *_TREE.csv, so this just downloads
#' the whole zip and extracts the required CSV files. Uses
#' `curl::multi_download()` which resumes and skips partial and incomplete
#' downloads, respectively, when run subsequent times.
#' @param states vector of state abbreviations; for all states use `state.abb`.
#' @param download_dir where to save the zip files.
#' @param extract which files to extract from the downloaded zip file—those
#'   needed by `forestTIME`, those needed by `rFIA`, all the files, or none.
#' @param keep_zip logical; keep the .zip file after CSVs are extracted?
#' Defaults to `TRUE`.
#'
#' @export
#' @returns returns nothing
fia_download <- function(
  states,
  download_dir = "fia",
  extract = c("forestTIME", "rFIA", "all", "none"),
  keep_zip = TRUE
) {
  extract <- match.arg(extract)
  tables_needed <- switch(
    extract,
    "forestTIME" = tables_ft,
    "rFIA" = tables_rfia
  )

  states <- match.arg(states, datasets::state.abb, several.ok = TRUE)
  fs::dir_create(download_dir)
  files <- glue::glue("{states}_CSV.zip")

  #check if zip files are already downloaded
  zip_check <- fs::path(download_dir, files) |> fs::file_exists()
  if (extract == "none" & all(zip_check)) {
    cli::cli_warn("All zip files already downloaded!")
    return(invisible(NULL))
  }

  base_url <- "https://apps.fs.usda.gov/fia/datamart/CSV/"
  out_paths <- fs::path(download_dir, files)
  urls <- utils::URLencode(paste0(base_url, files))


  #check if .csvs are there from a previous run with keep_zip = FALSE

  if (!extract %in% c("all", "none")) {
    csv_check <-
      purrr::map(states, function(state) {
        fs::path(download_dir, glue::glue("{state}_{tables_needed}.csv"))
      }) |>
      rlang::set_names(states) |>
      purrr::map_lgl(\(x) all(fs::file_exists(x)))
  } else {
    #can't reliably check if all CSVs are there when extract = "all"
    csv_check <- rep(FALSE, length(states)) |> purrr::set_names(states)
  }
  
  # if no zip and not all CSVs, download zips
  need_dl <- !(zip_check | csv_check)
  if (any(need_dl)) {
    urls <- urls[need_dl]
    out_paths <- out_paths[need_dl]

    #download file(s)
    cli::cli_alert_info("Downloading FIA data for {states[need_dl]}")
    resp <- curl::multi_download(
      urls = urls,
      destfiles = out_paths,
      resume = TRUE,
      progress = TRUE,
      multiplex = TRUE,
      useragent = "forestTIME-builder (https://github.com/Evans-Ecology-Lab/forestTIME-builder)"
    )
    # zips <- resp$destfile #not sure if this is used
    # TODO: Don't error if download fails, instead check response for issues and
    # retry.  Then warn if only some downloads failed.
    # https://github.com/Evans-Ecology-Lab/forestTIME-builder/issues/91

    # if (!any(fs::file_exists(resp$destfile))) {
    #   cli::cli_abort(c(
    #     "Download failed:",
    #     resp$destfile[!fs::file_exists(resp$destfile)]
    #   ))
    # }

    #update zip_check
    zip_check <- fs::path(download_dir, files) |> fs::file_exists()
  }
  if (extract == "none") {
    invisible(NULL)
  }

  # Unzip CSVs
  if (extract == "all") {
    unzip_csvs(
      names(zip_check)[zip_check],
      dir = download_dir,
      keep_zip = keep_zip,
    )
    return(invisible(NULL))
  } else if (extract %in% c("forestTIME", "rFIA") & all(csv_check)) {
    cli::cli_alert_warning("All CSVs already downloaded!")
    return(invisible(NULL))
  } else if (extract %in% c("forestTIME", "rFIA") & any(!csv_check))
    unzip_csvs(
      names(zip_check)[zip_check & !csv_check],
      dir = download_dir,
      keep_zip = keep_zip,
      tables = tables_needed
    )
    return(invisible(NULL))
  }

unzip_csvs <- function(zips, dir, keep_zip, tables = NULL) {
  cli::cli_alert_info("Extracting CSVs from .zip files")
  #pull out the CSVs of interest for each state
  lapply(zips, \(zip) {
    if (!is.null(tables)) {
      csvs <- stringr::str_replace(
        fs::path_file(zip),
        "CSV.zip",
        glue::glue("{tables}.csv")
      )
    } else {
      csvs <- NULL #for when extract = "all"
    }
    utils::unzip(zip, files = csvs, exdir = dir)
    if (isFALSE(keep_zip)) {
      cli::cli_alert_info("Removing .zip file")
      fs::file_delete(zip)
    }
  })
  return(invisible(TRUE))
}

