
get_model_sims <- function(mentalmodel,
                           method = c(
                             "jaccard"
                           )){
  logger::log_info("Using Jaccard similarity as default.
                   This is likely not the best choice")
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
                           method = c(
                             "jaccard"
                           )){
  graph1 <- get_user_graph(user1,mentalmodel)
  graph2 <- get_user_graph(user2,mentalmodel)
  if (method == "jaccard"){
    sim <-
      jaccard_edgeset_similarity(graph1,graph2)
    return(sim)
  }

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
