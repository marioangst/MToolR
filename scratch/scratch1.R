
devtools::document()
devtools::load_all()

test <-
  MToolR::mentalmodel_from_csv("data-raw/example_export.csv",
                               exclude_nonresponse = TRUE)

test

user_df <- data.frame(
  id = test$user_data$id,
  var = rnorm(length(test$user_data$id))
)
user_df$group <- ifelse(user_df$var > 0, "group1","group2")

test <-
test |> add_user_data(user_data = user_df,id_key = "id")

test

sloop::s3_dispatch(show(test))
sloop::s3_dispatch(print(test))
sloop::s3_dispatch(plot(test))
print(test)
plot(test)
print(test$data)

test_agg <- aggregate_mentalmodel(test)
test_agg

plot(test_agg)

test_group1_agg <- aggregate_mentalmodel(test,group_var = "group",group_value = "group1")
test_group2_agg <- aggregate_mentalmodel(test,group_var = "group",group_value = "group2")

coords <- igraph::layout_in_circle(test_group1_agg$graph)
library(patchwork)
(plot(test_group1_agg, layout = coords) + ggplot2::ggtitle("Group 1")) /
  (plot(test_group2_agg, layout = coords) + ggplot2::ggtitle("Group 2"))

calculate_descriptive_statistics(test)
calculate_descriptive_statistics(test_agg)

sims_list <- get_model_sims(test,method = "jaccard", group_var = "group")
sims_list

heatmap(sims_list$group1,main = "Heatmap of group 1")
heatmap(sims_list$group2)

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


