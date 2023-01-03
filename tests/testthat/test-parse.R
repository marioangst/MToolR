test_that("graph object constructed from csv is weighted", {
  test <-
    MToolR::mentalmodel_from_csv(testthat::test_path("example_export.csv"), exclude_nonresponse = TRUE)
  expect_true(igraph::is_weighted(test$graph))
})
