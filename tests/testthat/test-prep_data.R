library(dplyr)
test_that("fia_tidy() works", {
  fia_dir <- system.file("exdata", package = "forestTIME")
  # fia_download(states = "RI", download_dir = fia_dir, keep_zip = TRUE)
  db <- fia_load(states = "DE", dir = fia_dir)
  data <- fia_tidy(db)

  expect_s3_class(data, "data.frame")
  expect_true("tree_ID" %in% colnames(data))
  expect_true("plot_ID" %in% colnames(data))

  #check that ACTUALHT gets filled in with HT when NA
  ht_check <- data |> dplyr::filter(!is.na(HT) & is.na(ACTUALHT))
  expect_equal(nrow(ht_check), 0)

  #check that SPCD is consistent
  spcd_check <- data |> group_by(tree_ID) |> filter(length(unique(SPCD)) > 1)
  expect_equal(nrow(spcd_check), 0)
})
