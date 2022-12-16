
#' Default mental model plot function using ggraph
#'
#' @param mentalmodel An object of type mtoolr
#'
#' @return A plot
#' @export
#'
#' @examples
plot.mtoolr <- function(mentalmodel, ...){
  if(is_aggregated(mentalmodel)){
    logger::log_info("Plotting aggregated mental model")
    mental_model_ggraph(mentalmodel$graph, ...)
  }
  if(!is_aggregated(mentalmodel)){
    user_sample <-
      sample(mentalmodel$user_data$id,
           1, replace = FALSE)
    logger::log_info("Plotting randomly selected user models: {user_sample}")
    plot_user_model(user_sample,
                    mentalmodel,
                    layout = "stress") +
      ggplot2::ggtitle(
        "Mental model sample",
        subtitle = paste0("user ",user_sample))
  }
}

#' A ggraph plotting setup for mental model data
#'
#' @param graph Igraph object based on mental model data
#' @param ... other parameters to ggraph, eg. layout choice - normally defaults
#' to "stress"
#' Check the graphlayouts package for more options, eg. "circle" or "sugiyama"
#'
#' @return A ggplot object
#' @export
#'
#' @examples
mental_model_ggraph <- function(graph, ...){
  ggraph::ggraph(graph, ...) +
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
    ggraph::scale_edge_width(range = c(0.5,1.5)) +
    ggraph::theme_graph() +
    ggplot2::coord_cartesian(clip = 'off')
}

#' Plot the mental model of a specific user
#'
#' @param mentalmodel A mtoolr object containing the mental model of the user
#' @param user The id of the user
#'
#' @return
#' @export
#'
#' @examples
plot_user_model <- function(user, mentalmodel, ...){
  g <- get_user_graph(user, mentalmodel)
  mental_model_ggraph(g,...)
}
