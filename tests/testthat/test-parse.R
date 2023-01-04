test_that("example csv can be parsed with excluding nonresponses", {
  test_obj <-
    MToolR::parse_mtool_csv(testthat::test_path("example_export.csv"), exclude_nonresponse = TRUE)
  expect_true(all(nrow(test_obj) == 392))
})

test_that("example csv can be parsed without excluding nonresponses", {
  test_obj <-
    MToolR::parse_mtool_csv(testthat::test_path("example_export.csv"), exclude_nonresponse = FALSE)
  expect_true(all(nrow(test_obj) == 423))
})

test_that("example csv can be imported to mtoolr object", {
  test_obj <-
    MToolR::mentalmodel_from_csv(testthat::test_path("example_export.csv"), exclude_nonresponse = TRUE)
  expect_true(all(nrow(test_obj$data) == 392,
                  length(test_obj$users) == 53))
})

test_that("graph object constructed from csv is weighted", {
  test_obj <-
    MToolR::mentalmodel_from_csv(testthat::test_path("example_export.csv"), exclude_nonresponse = TRUE)
  expect_true(igraph::is_weighted(test_obj$graph))
})
