
#' Get similiarities between mental models of users
#'
#' This function calculates the similarity between the mental models of
#' of users, either among all users or among groups of users.
#'
#' @param mentalmodel A mtoolr object with models from multiple users (therefore not an aggregated object)
#' @param method The method to calculate similarity between mental models of users.
#' At the moment, this defaults to "gower". Other options are "simple_overlap" and "jaccard".
#' @param group_var A character string giving the name of a (preferably factor) variable in the user data to create
#' groups of users. For each group of users, a similarity matrix is calculated. All matrices
#' are returned in a list.
#'
#' @return If no group_var is supplied a similarity matrix. If a group_var is supplied
#' a list of similarity matrices for each level of the grouping variable.
#' @export
#'
#' @examples
#' # get gower similarity between mental models of all users
#' gower_sim_mat <- get_model_sims(example_models)
#' gower_sim_mat
#' # simulate user data to add
#' user_df <- data.frame(id = example_models$user_data$id,var = rnorm(length(example_models$user_data$id)))
#' user_df$group <- ifelse(user_df$var > 0, "group1","group2")
#' # add user data
#' example_models <- example_models |> add_user_data(user_data = user_df,id_key = "id")
#' # compute similarities by group
#' gower_sim_mat_list <- get_model_sims(example_models, group_var = "group")
get_model_sims <- function(mentalmodel,
                           method = NULL,
                           group_var = NULL){
  stop_if_not_mtoolr(mentalmodel)
  if (is_aggregated(mentalmodel)){
    stop("Model supplied is aggregated. Please supply a non-aggregated mtoolr object.")
  }
  if (is.null(method)){
    method <- "gower"
    logger::log_info("Using Gower similarity, comparing only edge weights > 0 as default.")
  }

  if (!is.null(group_var)){
    check_group_var_is_valid(mentalmodel,group_var)
    if(!(is.factor(mentalmodel$user_data[group_var]))){
      logger::log_info("Converting grouping variable {group_var} to factor. This might have unintended consequences.
                       To avoid this, provide a factor variable.")
      mentalmodel$user_data[[group_var]] <- as.factor(mentalmodel$user_data[[group_var]])
    }
    levels_group_var <- levels(mentalmodel$user_data[[group_var]])
    users_list <-
      lapply(levels_group_var,
             get_group_subset_ids,
             x = mentalmodel,
             group_var = group_var)
    sims_list <-
      lapply(users_list,
             get_users_sim_mat,
             mentalmodel = mentalmodel,
             method = method)
    names(sims_list) <- levels_group_var
    return(sims_list)
  }
  else{
    users <- mentalmodel$user_data$id
    sim_mat <- get_users_sim_mat(users,
                                 mentalmodel = mentalmodel,
                                 method = method)
    return(sim_mat)
  }

}

#' Get the similarity matrix between all of a group of users
#'
#' @param users A character vector of user IDs
#' @param mentalmodel A mtoolr object
#' @param method The method to use (see `?get_model_sims` for available)
#'
#' @return A similarity matrix
#' @export
#'
#' @examples
get_users_sim_mat <- function(users,mentalmodel,method){
  sim_mat <- matrix(
    NA,
    nrow = length(users),
    ncol = length(users),
    dimnames = list(users,users)
  )
  combs <- expand.grid(users,users,stringsAsFactors = FALSE)
  sim_mat[cbind(combs[,1],combs[,2])] <-
    unlist(
      pbapply::pblapply(
        c(1:nrow(combs)),
        function(x)
          get_user_model_sim(combs[x,1],
                             combs[x,2],
                             mentalmodel = mentalmodel,
                             method = method)
      )
    )
  return(sim_mat)
}

#' Get the similarity between two specific users
#'
#' @param user1 A character string for user 1
#' @param user2 A character string for user 2
#' @param mentalmodel A mtoolr object
#' @param method The method to use (see `?get_model_sims` for available)
#'
#' @return A numeric value between 0 and 1
#' @export
#'
#' @examples
get_user_model_sim <- function(user1,
                               user2,
                           mentalmodel,
                           method){
  graph1 <- get_user_graph(user1,mentalmodel)
  graph2 <- get_user_graph(user2,mentalmodel)
  if (method == "jaccard"){
    sim <-
      jaccard_edgeset_similarity(graph1,graph2)
    return(sim)
  }
  if (method == "gower"){
    sim <-
      gower_edgeset_similarity(graph1,graph2)
    return(sim)
  }
  if (method == "simple_overlap"){
    sim <-
      simpleoverlap_edgeset_similarity(graph1, graph2)
    return(sim)
  }
}

get_graph_edgeweights <- function(g){
  adjmat <- igraph::as_adjacency_matrix(g,attr = "Weight",sparse = FALSE)
  as.vector(adjmat)
}

get_graph_edgeweights_f <- function(g){
  adjmat <- igraph::as_adjacency_matrix(g,attr = "Weight",sparse = FALSE)
  as.factor(as.vector(adjmat))
}

gower_edgeset_similarity <- function(graph1,graph2){

  comp_df <-
    data.frame(
      weights1 <- get_graph_edgeweights(graph1),
      weights2 <- get_graph_edgeweights(graph2)
    )

  comp_df_nozero <- comp_df[rowSums(comp_df) > 0,]

  type_list <- as.list(
    c(1:nrow(comp_df_nozero))
  )
  names(type_list) <- rep("ordratio",nrow(comp_df_nozero))
  abs(
    1- cluster::daisy(t(comp_df_nozero),
                      metric = "gower",
                      type = type_list)[1]
  )

}

jaccard_edgeset_similarity <- function(graph1, graph2) {
  inter <- length(igraph::E(graph1 %s% graph2))
  un <- length(igraph::E(graph1 %u% graph2))

  if (un == 0) {
    0
  } else {
    inter/un
  }
}

simpleoverlap_edgeset_similarity <- function(graph1,graph2){

  comp_df <-
    data.frame(
      weights1 <- get_graph_edgeweights(graph1),
      weights2 <- get_graph_edgeweights(graph2)
    )
  comp_df$weights1 <- ifelse(comp_df$weights1 > 0,1,0)
  comp_df$weights2 <- ifelse(comp_df$weights2 > 0,1,0)

  comp_df_nozero <- comp_df[rowSums(comp_df) > 0,]

  if(nrow(comp_df_nozero) == 0){
    return(0)
  }
  else{
    sum(comp_df_nozero$weights1 == comp_df_nozero$weights2) / nrow(comp_df_nozero)
  }
}
