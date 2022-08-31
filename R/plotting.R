
#' A ggraph plotting setup for mental model data
#'
#' @param graph Igraph object based on mental model data
#'
#' @return A ggplot object
#' @export
#'
#' @examples
mental_model_ggraph <- function(graph){
  ggraph::ggraph(graph,layout = "stress") +
    ggraph::geom_edge_fan(ggplot2::aes(start_cap =
                                         ggraph::label_rect(node1.name,
                                                            padding =
                                                              ggplot2::margin(2, 2, 2.5, 2, "mm")),
                                       end_cap = ggraph::label_rect(
                                         node2.name,
                                         padding =
                                           ggplot2::margin(2, 2, 2.5, 2, "mm")),
                                       width = weight),
                          arrow = ggplot2::arrow(length = ggplot2::unit(4, 'mm'))) +
    ggraph::geom_node_label(ggplot2::aes(label = name),
                            label.padding = ggplot2::unit(0.2, "lines"),) +
    ggraph::scale_edge_width(range = c(1,3)) +
    ggraph::theme_graph() +
    ggplot2::coord_cartesian(clip = 'off')
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
