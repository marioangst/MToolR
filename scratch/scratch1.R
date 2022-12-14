
devtools::document()
devtools::load_all()

test <-
  MToolR::mentalmodel_from_csv("tests/test_export.csv",
                               exclude_nonresponse = TRUE)

test
sloop::s3_dispatch(show(test))
sloop::s3_dispatch(print(test))
sloop::s3_dispatch(plot(test))
print(test)
plot(test)
print(test$data)

test_agg <- get_aggregated(test)
test_agg

plot(test_agg)

calculate_descriptive_statistics(test)
calculate_descriptive_statistics(test_agg)

get_user_model_sim(test$user_list[1],test$user_list[2], test, method = "gower")
get_user_model_sim(test$user_list[1],test$user_list[2], test, method = "exact_overlap")

sims <- get_model_sims(test)
heatmap(sims)
unique(diag(sims))

sims2 <- get_model_sims(test,method = "jaccard")
heatmap(sims2)
unique(diag(sims2))

sims3 <- get_model_sims(test,method = "simple_overlap")
heatmap(sims3)
unique(diag(sims3))

cor(data.frame(as.vector(sims),as.vector(sims2),as.vector(sims3)))
