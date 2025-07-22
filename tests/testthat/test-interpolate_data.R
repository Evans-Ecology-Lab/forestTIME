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

test_that("interpolation flags negative numbers as fallen dead", {
  db <- fia_load(
    "DE",
    dir = system.file("exdata", package = "forestTIME.builder")
  )
  data <- fia_tidy(db) |>
    dplyr::filter(tree_ID %in% c("10_1_1_104_3_4", "10_1_1_148_4_2")) |>
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
    data_interpolated |>
      filter(ACTUALHT < 4.5) |>
      pull(STANDING_DEAD_CD) |>
      unique(),
    0
  )
  expect_equal(
    data_interpolated |> filter(ACTUALHT < 4.5) |> pull(STATUSCD) |> unique(),
    2
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