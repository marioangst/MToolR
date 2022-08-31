
library("ggraph")

#' A ggraph plotting setup for mental model data
#'
#' @param graph Igraph object based on mental model data
#'
#' @return A ggplot object
#' @export
#'
#' @examples
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

#' Plot the aggregate model from an edgelist supplied by M-Tool
#'
#' @param edgelist Likely a raw edgelist data frame generated from parse_mtools_csv().
#'
#' @return A ggplot object
#' @export
#'
#' @examples
plot_aggregate_model <- function(edgelist){
  agg_el <- MToolR::get_aggregate_el(edgelist)
  g <- igraph_from_mtools_el(agg_el)
  mental_model_ggraph(g)
}

#' Plot the mental model of a specific user
#'
#' @param edgelist Likely a raw edgelist data frame generated from parse_mtools_csv().
#' @param user A user ID as appearing in the User_ID column of the edgelist supplied
#'
#' @return
#' @export
#'
#' @examples
plot_user_model <- function(edgelist, user){
  user_el <- MToolR::get_user_el(edgelist, user)
  g <- igraph_from_mtools_el(user_el)
  mental_model_ggraph(g)
}
