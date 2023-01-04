

#' Aggregate mental models from different users
#'
#' This function aggregates individual mental models in an mtoolr object.
#' If a grouping variable (eg. a column "likes_cycling" added to the object, see `add_user_data()`) is supplied
#' and a grouping value (eg. "yes") is supplied, only mental
#' models of this group are aggregated (so only the mental models of users who like cycling).
#' Currently, models are aggregated using the median value of edge weights to establish edges and edge weight.
#'
#' @param mentalmodel A mtoolr object
#' @param aggregate_function Currently, this defaults to the median value.
#' @param group_var A character string giving a (preferably factor) variable in the user data to group by (eg. "likes_cycling").
#' @param group_value A value in group_var to match by (eg. "yes")
#'
#' @return An aggregated mtoolr object
#' @export
#'
#' @examples
#' # aggregate all models in example data
#' aggregate_mentalmodel(example_models)
#' # simulate user data to add
#' user_df <- data.frame(id = example_models$user_data$id,var = rnorm(length(example_models$user_data$id)))
#' user_df$group <- ifelse(user_df$var > 0, "group1","group2")
#' # add user data
#' example_models <- example_models |> add_user_data(user_data = user_df,id_key = "id")
#' # aggregate by group
#' agg_model_group1 <- aggregate_mentalmodel(example_models,group_var = "group",group_value = "group1")
#' agg_model_group1
aggregate_mentalmodel <- function(mentalmodel,
                                  aggregate_function = "median",
                                  group_var = NULL,
                                  group_value = NULL){
  stop_if_not_mtoolr(mentalmodel)
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
#' If an aggregated mental model is supplied, statistics
#' are returned for all aggregated groups.
#'
#' If a mental model object with individual user data is
#' supplied, statistics are returned per user.
#'
#' @export
#'
#' @examples
#' # This returns statistics for each concept by user
#' calculate_descriptive_statistics(example_models)
#' # If an aggregated object is supplied, statistics are supplied for the in the aggregate
#' aggregated_model <- aggregate_mentalmodel(example_models)
#' calculate_descriptive_statistics(aggregated_model)
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
            dplyr::mutate(user = x)
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


