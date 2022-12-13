

#' Create aggregate edgelist from M-Tool Data
#'
#' This function returns a mental model aggregating all
#' individual mental models in a mtoolr object.
#' The current default aggregation function is the median weight.
#'
#' @param mentalmodel A mtoolr object
#'
#' @return A aggregated mtoolr object
#' @export
#'
#' @examples
get_aggregated <- function(mentalmodel, aggregate_function = "median"){
  stopifnot(is_mtoolr(mentalmodel))
  stopifnot(!(is_aggregated(mentalmodel)))
  edgelist <- mentalmodel$data
  aggregated_el <- edgelist |>
    dplyr::group_by(From,To) |>
    dplyr::summarise(Weight = median(Weight,na.rm = TRUE), .groups = 'keep' )  |>
    dplyr::ungroup()

  logger::log_info("aggregated {length(mentalmodel$users)} models using aggregation function {aggregate_function}")

  aggregated <- new_mtoolr(aggregated_el, aggregated = TRUE)
  return(aggregated)
}

#' Create overview stats for a mental model
#'
#' @param mentalmodel A mtoolr mental model object
#'
#' @return A tibble with the following stats:
#'
#' - weighted betwenness
#' - weighted indegree
#' - weighted outdegree
#' - weighted total degree
#'
#' If a aggregated mental model is supplied, statistics
#' are returned for all aggregated groups.
#'
#' If a mental model object with individual user data is
#' supplied, statistics are returned per user.
#'
#' @export
#'
#' @examples
calculate_descriptive_statistics <- function(mentalmodel){

  if(is_aggregated(mentalmodel)){
    return(
     network_stats(mentalmodel$graph)
    )
  }
  else{
    return(
      do.call("rbind",lapply(
        names(mentalmodel$users),
        function(x){
          network_stats(mentalmodel$users[[x]][["graph"]]) |>
            mutate(user = x)
        }
      ))
    )
  }

}

network_stats <- function(g){
  tibble::tibble(
    concept = names(igraph::betweenness(g)),
    w_betweenness = igraph::betweenness(g),
    w_in_degree = igraph::degree(g,mode = "in"),
    w_out_degree = igraph::degree(g, mode = "out"),
    w_total_degree = igraph::degree(g, mode = "total")
  )
}


