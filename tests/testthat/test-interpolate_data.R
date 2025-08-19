library(dplyr)
test_that("variables with NAs get interpolated correctly", {
  db <- fia_load(
    "DE",
    dir = system.file("exdata", package = "forestTIME.builder")
  )
  data <- fia_tidy(db) |>
    dplyr::filter(tree_ID == "10_1_1_104_1_28") |>
    dplyr::select(
      plot_ID,
      tree_ID,
      CONDID,
      COND_STATUS_CD,
      CONDPROP_UNADJ,
      SPCD,
      INVYR,
      DIA,
      HT,
      ACTUALHT,
      CULL,
      CR,
      STATUSCD,
      STANDING_DEAD_CD,
      DECAYCD,
      DESIGNCD
    )
  data_interpolated <- data |>
    expand_data() |>
    interpolate_data()

  expect_equal(
    data_interpolated |> filter(YEAR %in% 2017:2020) |> pull(STANDING_DEAD_CD),
    rep(0, 4)
  )
})


test_that("interpolation of CULL is correct", {
  data <- fia_load(
    "DE",
    dir = system.file("exdata", package = "forestTIME.builder")
  ) |>
    fia_tidy() |>
    dplyr::filter(tree_ID == "10_1_1_128_1_24")

  data_interpolated <- data |>
    expand_data() |>
    interpolate_data()

  expect_true(
    data_interpolated |>
      filter(DIA < 5) |>
      pull(CULL) |>
      is.na() |>
      all()
  )

  cull_vals <- data_interpolated |>
    filter(DIA >= 5) |>
    pull(CULL) |>
    unique()
  expect_false(
    identical(cull_vals, c(0, 1))
  )
})


test_that("CONDPROP_UNADJ sums to ~1", {
  data <- fia_load(
    "RI",
    dir = system.file("exdata", package = "forestTIME.builder")
  ) |>
    fia_tidy() |>
    dplyr::filter(INVYR %in% c(2009:2014))

  data_interpolated <- data |>
    expand_data() |>
    interpolate_data()

  #test that cond props add to 1 within rounding error
  cond_sums <- data_interpolated |>
    group_by(plot_ID, YEAR, CONDID) |>
    summarize(
      CONDPROP_UNADJ = mean(CONDPROP_UNADJ)
    ) |>
    group_by(plot_ID, YEAR) |>
    summarize(cond_sum = sum(CONDPROP_UNADJ)) |>
    pull(cond_sum)
  expect_equal(cond_sums, rep(1, length(cond_sums)))
})

test_that("TPA_UNADJ is interpolated correctly", {
  test_data <- structure(
    list(
      plot_ID = c("53_5_57_98830", "53_5_33_61941", "53_5_53_53463"),
      tree_ID = c(
        "53_5_57_98830_1_309",
        "53_5_33_61941_2_158",
        "53_5_53_53463_4_391"
      ),
      YEAR = c(2006, 2004, 2006),
      DIA = c(3, 5.8, 35.5),
      CULL = c(0, 0, 0),
      PROP_BASIS = c("MACR", "MACR", "MACR"),
      MACRO_BREAKPOINT_DIA = c(30L, 30L, 30L),
      interpolated = c(FALSE, FALSE, FALSE),
      CONDID = c(1, 1, 1),
      COND_STATUS_CD = c(1, 1, 1),
      CONDPROP_UNADJ = c(1, 1, 0.802895)
    ),
    row.names = c(NA, -3L),
    class = c("tbl_df", "tbl", "data.frame")
  )

  data_interpolated <- interpolate_data(test_data) |> dplyr::arrange(DIA)

  expect_equal(data_interpolated$TPA_UNADJ, c(74.965282, 6.018046, 0.999188))
})
