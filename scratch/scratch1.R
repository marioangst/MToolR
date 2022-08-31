test_el <-
  MToolR::parse_mtools_csv("tests/test_export.csv", exclude_nonresponse = TRUE)

g <- MToolR::igraph_from_mtools_el(test_el)

plot_aggregate_model(test_el)

user_el <- get_user_el(test_el, user = test_el$User_ID[100])
plot_user_model(test_el, user = test_el$User_ID[1])

calculate_user_stats(test_el, user = test_el$User_ID[100])
calculate_aggregate_stats(test_el)
