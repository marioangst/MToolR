test_that("graph object constructed from el is weighted", {
  test_el <-
    MToolR::parse_mtools_csv("../test_export.csv", exclude_nonresponse = TRUE)
  expect_true(igraph::is_weighted(igraph_from_mtools_el(test_el)))
})
