

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
aggregate_mentalmodel <- function(mentalmodel,
                                  aggregate_function = "median",
                                  group_var = NULL,
                                  group_value = NULL){
  stopifnot(is_mtoolr(mentalmodel))
  stopifnot(!(is_aggregated(mentalmodel)))
  edgelist <- mentalmodel$data
  if(!(is.null(group_var))){
    check_group_var_is_valid(mentalmodel,group_var)
    if(!(is.factor(mentalmodel$user_data[group_var]))){
      logger::log_info("Converting grouping variable {group_var} to factor. This might have unintended consequences.
                       To avoid this, provide a factor variable.")
      mentalmodel$user_data[[group_var]] <- as.factor(mentalmodel$user_data[[group_var]])
    }
    levels_group_var <- levels(mentalmodel$user_data[[group_var]])
    if(!(group_value %in% levels_group_var)){
      stop(paste0(group_value," is not a factor level of variable ",group_var))
    }
    else{
      group_ids <- get_group_subset_ids(value = group_value,
                                        group_var = group_var,
                                        x = mentalmodel)
      edgelist <- edgelist |>
        dplyr::filter(User_ID %in% group_ids)
    }
  }
  aggregated_el <- edgelist |>
    dplyr::group_by(From,To) |>
    dplyr::summarise(Weight = median(Weight,na.rm = TRUE), .groups = 'keep' )  |>
    dplyr::ungroup()

  if(!(is.null(group_var))){
    logger::log_info("aggregated {length(group_ids)} models using aggregation function {aggregate_function} grouping by
                     grouping variable {group_var} on value {group_value}")

  }
  if(is.null(group_var)){
    logger::log_info("aggregated {length(mentalmodel$users)} models using aggregation function {aggregate_function}")

  }

  aggregated <- new_mtoolr(aggregated_el, aggregated = TRUE, concepts = mentalmodel$concepts)
  return(aggregated)
}

get_group_subset_ids <- function(value, group_var, x){
  df <- x$user_data
  id_vec <- dplyr::pull(df[df[[group_var]] == value,c("id")])
  return(id_vec)
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
        mentalmodel$user_data$id,
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


