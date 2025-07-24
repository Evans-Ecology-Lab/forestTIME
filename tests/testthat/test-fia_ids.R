test_that("fia_add_composite_ids() works", {
  df_tree <- tibble::tribble(
    ~STATECD, ~UNITCD, ~COUNTYCD, ~PLOT, ~SUBP, ~TREE,
    44, 1, 3, 2, 7, 134
  )
  df_ids <- fia_add_composite_ids(df_tree)
  expect_equal(
    colnames(df_ids),
    c("plot_ID", "tree_ID", colnames(df_tree))
  )
  expect_equal(df_ids$plot_ID, "44_1_3_2")
  expect_equal(df_ids$tree_ID, "44_1_3_2_7_134")

  df_plot <- df_tree |> dplyr::select(-SUBP, -TREE) 
  
  df_ids <- fia_add_composite_ids(df_plot)
  expect_equal(
    colnames(df_ids),
    c("plot_ID", colnames(df_plot))
  )
  expect_equal(df_ids$plot_ID, "44_1_3_2")

})

test_that("fia_split_composite_ids() works", {
  df <- tibble::tibble(plot_ID = "44_1_3_2", tree_ID = "44_1_3_2_7_134")
  df_split <- fia_split_composite_ids(df)

  expect_equal(
    sort(colnames(df_split)),
    sort(c("plot_ID", "tree_ID", "STATECD", "UNITCD", "COUNTYCD", "PLOT", "SUBP", "TREE"))
  )
  
  expect_true(
    all(!is.na(df_split))
  )
})