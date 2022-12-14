
get_model_sims <- function(mentalmodel,
                           method = NULL){
  if (is.null(method)){
    method <- "gower"
    logger::log_info("Using Gower similarity, comparing only edge weights > 0 as default.")
  }
  users <- mentalmodel$user_list
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
  inter <- length(E(graph1 %s% graph2))
  un <- length(E(graph1 %u% graph2))

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
