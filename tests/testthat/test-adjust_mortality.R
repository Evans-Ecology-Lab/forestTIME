library(dplyr)
test_that("fallen dead trees get NAs correctly", {
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
      MORTYR,
      STATUSCD,
      STANDING_DEAD_CD,
      DECAYCD,
      PROP_BASIS,
      MACRO_BREAKPOINT_DIA,
      COND_STATUS_CD,
      RECONCILECD
    )
  data_interpolated <- data |>
    expand_data() |>
    interpolate_data() |>
    adjust_mortality(use_mortyr = FALSE)

  expect_true(all(is.na(
    data_interpolated |>
      filter(YEAR %in% 2017:2020) |>
      select(DIA, HT, ACTUALHT, CULL, CR)
  )))
})

test_that("trees moving to non-sampled conditions have NAs", {
  db <- fia_load(
    "DE",
    dir = system.file("exdata", package = "forestTIME.builder")
  )
  data <- fia_tidy(db) |>
    dplyr::filter(tree_ID == "10_1_1_22_4_3") |>
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
      MORTYR,
      STATUSCD,
      STANDING_DEAD_CD,
      DECAYCD,
      PROP_BASIS,
      MACRO_BREAKPOINT_DIA,
      COND_STATUS_CD,
      RECONCILECD
    )
  data_interpolated <- data |>
    expand_data() |>
    interpolate_data() |>
    adjust_mortality(use_mortyr = FALSE)
  expect_equal(max(data_interpolated$YEAR), 2018)
  expect_true(all(is.na(
    data_interpolated |>
      filter(YEAR >= 2015) |>
      select(DIA, HT, ACTUALHT, CR, CULL)
  )))
})

test_that("method doesn't matter for DE", {
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
      MORTYR,
      STATUSCD,
      STANDING_DEAD_CD,
      DECAYCD,
      PROP_BASIS,
      MACRO_BREAKPOINT_DIA,
      COND_STATUS_CD,
      RECONCILECD
    )
  data_interpolated <- data |>
    expand_data() |>
    interpolate_data()

  data_midpt <- data_interpolated |>
    adjust_mortality(use_mortyr = FALSE)

  data_mortyr <- data_interpolated |>
    adjust_mortality(use_mortyr = TRUE)

  expect_equal(data_midpt, data_mortyr)
})

test_that("No values below thresholds for measurement", {
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
      MORTYR,
      DIA,
      HT,
      ACTUALHT,
      CULL,
      CR,
      STATUSCD,
      STANDING_DEAD_CD,
      DECAYCD,
      RECONCILECD,
      COND_STATUS_CD,
      PROP_BASIS,
      MACRO_BREAKPOINT_DIA
    )
  data_adj <- data |>
    expand_data() |>
    interpolate_data() |>
    adjust_mortality()

  expect_equal(
    nrow(data_adj |> filter(ACTUALHT < 4.5)),
    0
  )
})

test_that("MORTYR gets nudged to next year if tree is alive", {
  data_interpolated <- readRDS(testthat::test_path("testdata/CO_MORTYR.rds"))

  data_adj <- adjust_mortality(data_interpolated, use_mortyr = TRUE)

  expect_equal(
    data_adj |>
      filter(
        tree_ID == "8_1_119_80086_3_12", #alive in MORTYR
        YEAR == MORTYR
      ) |>
      pull(STATUSCD),
    1
  )
  expect_equal(
    data_adj |>
      filter(
        tree_ID == "8_1_119_80086_3_12", # alive in MORTYR
        YEAR == MORTYR + 1
      ) |>
      pull(STATUSCD),
    2
  )

  expect_equal(
    data_adj |>
      filter(tree_ID == "8_1_119_80086_3_12") |>
      pull(MORTYR_eff) |>
      unique(),
    data_adj |>
      filter(tree_ID == "8_1_119_80086_3_12") |>
      pull(MORTYR) |>
      unique() +
      1
  )

  expect_equal(
    data_adj |>
      filter(
        tree_ID == "8_1_119_85646_4_1", # dead in MORTYR
        YEAR == MORTYR
      ) |>
      pull(STATUSCD),
    2
  )
})

test_that("Negative numbers are dealt with", {
  # from dput() on results of interpolate_data() filtered to pick just one tree
  # with negative values for ACTUALHT:
  data_interpolated <-
    structure(
      list(
        plot_ID = c(
          "6_2_49_78561",
          "6_2_49_78561",
          "6_2_49_78561",
          "6_2_49_78561",
          "6_2_49_78561",
          "6_2_49_78561",
          "6_2_49_78561",
          "6_2_49_78561",
          "6_2_49_78561",
          "6_2_49_78561",
          "6_2_49_78561",
          "6_2_49_78561",
          "6_2_49_78561",
          "6_2_49_78561",
          "6_2_49_78561",
          "6_2_49_78561"
        ),
        tree_ID = c(
          "6_2_49_78561_4_126",
          "6_2_49_78561_4_126",
          "6_2_49_78561_4_126",
          "6_2_49_78561_4_126",
          "6_2_49_78561_4_126",
          "6_2_49_78561_4_126",
          "6_2_49_78561_4_126",
          "6_2_49_78561_4_126",
          "6_2_49_78561_4_126",
          "6_2_49_78561_4_126",
          "6_2_49_78561_4_126",
          "6_2_49_78561_4_126",
          "6_2_49_78561_4_126",
          "6_2_49_78561_4_126",
          "6_2_49_78561_4_126",
          "6_2_49_78561_4_126"
        ),
        YEAR = c(
          2006,
          2007,
          2008,
          2009,
          2010,
          2011,
          2012,
          2013,
          2014,
          2015,
          2016,
          2017,
          2018,
          2019,
          2020,
          2021
        ),
        interpolated = c(
          FALSE,
          TRUE,
          TRUE,
          TRUE,
          TRUE,
          TRUE,
          TRUE,
          TRUE,
          TRUE,
          TRUE,
          FALSE,
          TRUE,
          TRUE,
          TRUE,
          TRUE,
          FALSE
        ),
        DIA = c(
          31.2,
          31.12,
          31.04,
          30.96,
          30.88,
          30.8,
          30.72,
          30.64,
          30.56,
          30.48,
          30.4,
          30.32,
          30.24,
          30.16,
          30.08,
          30
        ),
        HT = c(75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75),
        ACTUALHT = c(
          75,
          68.6,
          62.2,
          55.8,
          49.4,
          43,
          36.6,
          30.2,
          23.8,
          17.4,
          11,
          4.6,
          -1.8,
          -8.2,
          -14.6,
          -21
        ),
        CR = c(40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40),
        CULL = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 16, 12, 8, 4, 0),
        PLT_CN = c(
          "8915840010901",
          "8915840010901",
          "8915840010901",
          "8915840010901",
          "8915840010901",
          "345936412489998",
          "345936412489998",
          "345936412489998",
          "345936412489998",
          "345936412489998",
          "345936412489998",
          "345936412489998",
          "345936412489998",
          "750087507290487",
          "750087507290487",
          "750087507290487"
        ),
        CONDID = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
        PROP_BASIS = c(
          "MACR",
          "MACR",
          "MACR",
          "MACR",
          "MACR",
          "MACR",
          "MACR",
          "MACR",
          "MACR",
          "MACR",
          "MACR",
          "MACR",
          "MACR",
          "MACR",
          "MACR",
          "MACR"
        ),
        STDORGCD = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        MORTYR = c(
          2010L,
          2010L,
          2010L,
          2010L,
          2010L,
          2010L,
          2010L,
          2010L,
          2010L,
          2010L,
          2010L,
          2010L,
          2010L,
          2010L,
          2010L,
          2010L
        ),
        STATUSCD = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2),
        RECONCILECD = c(
          NA_real_,
          NA_real_,
          NA_real_,
          NA_real_,
          NA_real_,
          NA_real_,
          NA_real_,
          NA_real_,
          NA_real_,
          NA_real_,
          NA_real_,
          NA_real_,
          NA_real_,
          NA_real_,
          NA_real_,
          NA_real_
        ),
        DECAYCD = c(NA, NA, NA, NA, NA, 3, 3, 3, 3, 3, 3, 3, 3, NA, NA, NA),
        STANDING_DEAD_CD = c(
          NA,
          NA,
          NA,
          NA,
          NA,
          1,
          1,
          1,
          1,
          1,
          1,
          1,
          1,
          0,
          0,
          0
        ),
        SPCD = c(
          747,
          747,
          747,
          747,
          747,
          747,
          747,
          747,
          747,
          747,
          747,
          747,
          747,
          747,
          747,
          747
        ),
        DESIGNCD = c(
          501,
          501,
          501,
          501,
          501,
          501,
          501,
          501,
          501,
          501,
          501,
          501,
          501,
          501,
          501,
          501
        ),
        INTENSITY = c(
          1L,
          1L,
          1L,
          1L,
          1L,
          1L,
          1L,
          1L,
          1L,
          1L,
          1L,
          1L,
          1L,
          1L,
          1L,
          1L
        ),
        ECOSUBCD = c(
          "M261Gf",
          "M261Gf",
          "M261Gf",
          "M261Gf",
          "M261Gf",
          "M261Gf",
          "M261Gf",
          "M261Gf",
          "M261Gf",
          "M261Gf",
          "M261Gf",
          "M261Gf",
          "M261Gf",
          "M261Gf",
          "M261Gf",
          "M261Gf"
        ),
        TPA_UNADJ = c(
          0.999188,
          0.999188,
          0.999188,
          0.999188,
          0.999188,
          0.999188,
          0.999188,
          0.999188,
          0.999188,
          0.999188,
          0.999188,
          0.999188,
          0.999188,
          0.999188,
          0.999188,
          0.999188
        ),
        COND_STATUS_CD = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
        CONDPROP_UNADJ = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
        EXPNS = c(
          235975.225225225,
          207471.287128713,
          182531.358885017,
          161189.230769231,
          148193.776520509,
          145316.227461859,
          149675.714285714,
          163197.819314642,
          173465.231788079,
          188780.18018018,
          201486.538461538,
          208711.155378486,
          235975.225225225,
          282407.008086253,
          353962.837837838,
          440222.68907563
        )
      ),
      row.names = c(NA, -16L),
      class = c("tbl_df", "tbl", "data.frame")
    )

  data_adj <- adjust_mortality(data_interpolated, use_mortyr = TRUE)

  expect_equal(nrow(data_adj |> dplyr::filter(ACTUALHT < 0)), 0)
})
