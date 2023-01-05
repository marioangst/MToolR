
#' Default mental model plot function using ggraph
#'
#' Calls `plot_mentalmodel()`.
#' If data on multiple users are supplied, a sample user model is plotted.
#' If you supply a user argument, you can plot the model of a specific user.
#' If aggregated data are supplied, the aggregated model is plotted.
#'
#' @param x An object of type mtoolr
#' @param ... other parameters to ggraph, eg. layout choice - normally defaults
#' to "stress"
#' Check the graphlayouts package for more options, eg. "circle" or "sugiyama"
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' # NOT RUN
#' # #plot a random user model
#' # plot(example_models)
#' # #plot a specific user
#' # plot(example_models, user = "ad84c4ed-b73e-4ba2-8e1f-edbe365bb225")
#' # #aggregate and then plot
#' # aggregated_model <- aggregate_mentalmodel(example_models)
#' # plot(aggregated_model)
plot.mtoolr <- function(x, ...){
  plot_mentalmodel(mentalmodel = x, ...)
}

#' Plot mental models using ggraph
#'
#' If data on multiple users are supplied, a sample user model is plotted.
#' If you supply a user argument, you can plot the model of a specific user.
#' If aggregated data are supplied, the aggregated model is plotted.
#'
#' @param mentalmodel An object of type mtoolr
#' @param user A character string giving the name of a specific user for which to plot a mental model
#' @param ... other parameters to ggraph, eg. layout choice - normally defaults
#' to "stress"
#' Check the graphlayouts package for more options, eg. "circle" or "sugiyama"
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' # NOT RUN
#' # #plot a random user model
#' # plot_mentalmodel(example_models)
#' # #plot a specific user
#' # plot_mentalmodel(example_models, user = "ad84c4ed-b73e-4ba2-8e1f-edbe365bb225")
#' # #aggregate and then plot
#' # aggregated_model <- aggregate_mentalmodel(example_models)
#' # plot_mentalmodel(aggregated_model)
plot_mentalmodel <- function(mentalmodel, user = NULL, ...){
  if(is_aggregated(mentalmodel)){
    logger::log_info("Plotting aggregated mental model")
    p <- mental_model_ggraph(mentalmodel$graph, ...)
    return(p)
  }
  if(!is_aggregated(mentalmodel)){
    if(is.null(user)){
      user <-
        sample(mentalmodel$user_data$id,
               1, replace = FALSE)
      logger::log_info("Plotting randomly selected user models: {user}")
    }
    p <-
      plot_user_model(mentalmodel = mentalmodel,
                      user = user,
                      layout = "stress") +
      ggplot2::ggtitle(
        "Mental model sample",
        subtitle = paste0("user ",user))
    return(p)
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
  p <-
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
  return(p)
}

#' Plot the mental model of a specific user
#'
#' @param mentalmodel A mtoolr object containing the mental model of the user
#' @param user The id of the user
#' @param ... further arguments to ggraph
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' # plot_user_model(example_models, user = "ad84c4ed-b73e-4ba2-8e1f-edbe365bb225")
plot_user_model <- function(mentalmodel, user, ...){
  g <- get_user_graph(user, mentalmodel)
  mental_model_ggraph(g, ...)
}
