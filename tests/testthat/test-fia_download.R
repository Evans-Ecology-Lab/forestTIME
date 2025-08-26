test_that("fia_download() works", {
  skip_on_cran()

  path <- withr::local_tempdir()
  fia_download(states = "RI", download_dir = path, keep_zip = TRUE)
  files <- fs::dir_ls(path)
  expect_true(any(stringr::str_detect(files, "RI_CSV.zip")))
  expect_true(any(stringr::str_detect(files, "RI_PLOTGEOM.csv")))

  db <- fia_load(states = "RI", dir = path)

  expect_type(db, "list")
  expect_s3_class(db[[1]], "data.frame")
})

test_that("`extract = 'rFIA'` works", {
  skip_on_cran()

  path <- withr::local_tempdir()
  fia_download(states = "RI", download_dir = path, extract = "rFIA")
  x <- rFIA::readFIA(dir = path, states = "RI")
  expect_s3_class(x, "FIA.Database")
  y <- fia_load(states = "RI", dir = path)
  expect_type(y, "list")
  data_tidy <- fia_tidy(y)
  expect_s3_class(data_tidy, "data.frame")
})
