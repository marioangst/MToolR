
library("ggraph")

mental_model_ggraph <- function(graph){
  ggraph::ggraph(graph,layout = "sugiyama") +
    ggraph::geom_edge_fan(aes(start_cap =
                                label_rect(node1.name,
                                                   padding =
                                                     margin(2, 2, 2.5, 2, "mm")),
                              end_cap = label_rect(
                                node2.name,
                                padding =
                                  margin(2, 2, 2.5, 2, "mm")),
                              width = weight),
                          arrow = arrow(length = unit(4, 'mm'))) +
    ggraph::geom_node_label(aes(label = name),
                            label.padding = unit(0.2, "lines"),) +
    ggraph::scale_edge_width(range = c(1,3)) +
    ggraph::theme_graph() +
    coord_cartesian(clip = 'off')
}

plot_aggregate_model <- function(edgelist){
  agg_el <- MToolR::get_aggregate_el(edgelist)
  g <- igraph_from_mtools_el(agg_el)
  mental_model_ggraph(g)
}

plot_user_model <- function(edgelist, user){
  user_el <- MToolR::get_user_el(edgelist, user)
  g <- igraph_from_mtools_el(user_el)
  mental_model_ggraph(g)
}
