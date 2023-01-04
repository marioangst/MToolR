test_that("similarities are computed in square matrix for all users with diagonal of 1s*", {
  gower_sim_mat <- get_model_sims(example_models)
  expect_true(all(dim(gower_sim_mat) == nrow(example_models$user_data)))
  expect_true(all(diag(gower_sim_mat) == 1))
})

test_that("grouped similarities are computed in list of square matrices for all users with diagonal of 1s*", {
  # simulate user data to add
  user_df <- data.frame(id = example_models$user_data$id,var = rnorm(length(example_models$user_data$id)))
  user_df$group <- ifelse(user_df$var > 0, "group1","group2")
  # add user data
  example_models <- example_models |> add_user_data(user_data = user_df,id_key = "id")
  # compute similarities by group
  gower_sim_mat_list <- get_model_sims(example_models, group_var = "group")
  expect_true(length(gower_sim_mat_list) == length(unique(user_df$group)))
  expect_true(all(unlist(
    lapply(
      gower_sim_mat_list,
      function(x) dim(x)[1] == dim(x)[2]
    )
  )))
  expect_true(all(unlist(
    lapply(
      gower_sim_mat_list,
      function(x) all(diag(x) == 1)
    )
  )))
})
