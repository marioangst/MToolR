
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

get_user_model_sim(test$user_list[1],test$user_list[2], test)
get_model_sims(test)
