test_that("mtoolr objects can be identified", {
  expect_true(is_mtoolr(example_models))
  expect_false(is_mtoolr(mtcars))
})

test_that("A minimal mtoolr object can be created from scratch", {
  df <-
    data.frame(User_ID = c("User1"),
               To = c("Concept1"),
               From = c("Concept2"),
               Weight = c(2))
  min_test <- mentalmodel(df)
  expect_true(is_mtoolr(min_test))
})

test_that("User data can be added", {
  user_df <- data.frame(
    id = example_models$user_data$id,
    var = rnorm(length(example_models$user_data$id))
  )
  user_df$group <- ifelse(user_df$var > 0, "group1","group2")

  example_models <-
    example_models |> add_user_data(user_data = user_df,id_key = "id")
  expect_true(all(c("var","group") %in% colnames(example_models$user_data)))
  expect_true("user_data" %in% names(example_models))
})

test_that("Data can be aggregated by group", {
  user_df <- data.frame(
    id = example_models$user_data$id,
    var = rnorm(length(example_models$user_data$id))
  )
  user_df$group <- ifelse(user_df$var > 0, "group1","group2")

  example_models <-
    example_models |> add_user_data(user_data = user_df,id_key = "id")

  agg_model_group1 <- aggregate_mentalmodel(example_models,group_var = "group",group_value = "group1")
  expect_true(all(agg_model_group1$concepts == example_models$concepts))
  expect_true(is_aggregated(agg_model_group1))
})
