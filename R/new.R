

#' Is the object of class mtoolr?
#'
#' @param object An R object
#' @param aggregated Is this an aggreagated mental model?
#'
#' @return TRUE if the object is of class mtoolr
#' @export
#'
#' @examples
is_mtoolr <- function(object){
  "mtoolr" %in% class(object)
}

new_mtoolr <- function(x = list(),
                       aggregated = logical()){

  stopifnot(is.logical(aggregated))

  mentalmodel <- list()
  mentalmodel$data <- tibble::tibble(x)
  mentalmodel$concepts <- unique(c(x$To,x$From))
  mentalmodel$user_list <- unique(mentalmodel$data$User_ID)
  mentalmodel$graph <- igraph_from_mtools_el(x,
                                             concepts = mentalmodel$concepts)

  if(!(aggregated)){
    is_valid_mtool_edgelist(x)
    mentalmodel$users <- users_graphs_constructor(edgelist = mentalmodel$data,
                                                  user_list = mentalmodel$user_list,
                                                  concepts = mentalmodel$concepts)
  }
  structure(mentalmodel,
            class = "mtoolr",
            aggregated = aggregated)
}

mentalmodel <- function(x,
                        aggregated = FALSE){
  new_mtoolr(x)
}

is_valid_mtool_edgelist <- function(edgelist){
  all(
    is.list(edgelist),
    check_mtool_columns_exist(edgelist))
}

is_aggregated <- function(x){
  attributes(x)$aggregated
}

check_mtool_columns_exist <- function(x){
  MTOOL_EXPORT_COLUMNS %in% colnames(x)
}

get_user_graph <- function(user,x){
  x$users[[user]]$graph
}

#' Create an igraph graph object from a M-Tool edgelist
#'
#' Create a weighted, direct igraph graph object from M-Tool data.
#' This most often will not be seen by the user as it is used internally at
#' import.
#'
#' @param edgelist A raw edgelist necessarily containing a sender, receiver and weight column
#' @param from_col The name of the column to read sender nodes from. Defaults to MTool output "From"
#' @param to_col The name of the column to read receiver nodes from. Defaults to MTool output "To"
#' @param weight_col The name of the
#' @param concepts character vector of unique concepts gathered in mental model
#'
#' @return A weighted, directed igraph object
#' @export
#'
#' @examples
igraph_from_mtools_el <- function(edgelist,
                                  concepts,
                                  from_col = "From",
                                  to_col = "To",
                                  weight_col = "Weight"){
  g <- igraph::graph_from_data_frame(d = edgelist[,c(from_col,
                                                     to_col,
                                                     weight_col)],
                                     vertices = concepts)
  g <- igraph::set_edge_attr(graph = g, name = "weight",
                             value= edgelist$Weight)
  return(g)
}

#' Get the edgelist of a specific user of M-Tool
#'
#' @param edgelist Likely a raw edgelist data frame
#' @param user A user ID as appearing in the User_ID column of the edgelist supplied
#'
#' @return A subset edgelist
#' @export
#'
#' @examples
get_user_el <- function(edgelist, user){
  if (!(user %in% unique(edgelist$User_ID))){
    stop(paste0(
      "User not in list of User_IDs. Available User IDs are",
      unique(edgelist$User_ID)
    ))
  }
  else{
    edgelist <- edgelist[edgelist$User_ID %in% user,]
  }
  return(edgelist)
}

create_user_graph <- function(edgelist,user,concepts){
  user_el <- get_user_el(edgelist,user)
  user_graph <- igraph_from_mtools_el(user_el,concepts = concepts)
  return(user_graph)
}

users_graphs_constructor <- function(edgelist, user_list, concepts){
  user_graph_list <-
    lapply(user_list, function(x){
      list(
        data = get_user_el(user = x, edgelist = edgelist),
        graph = create_user_graph(user = x,
                               edgelist = edgelist,
                               concepts = concepts),
        user = x
      )
    })
  names(user_graph_list) <- user_list
  return(user_graph_list)
}

MTOOL_EXPORT_COLUMNS <- c("User_ID",
                          "Total_Start",
                          "Total_Duration",
                          "Type",
                          "Start",
                          "Duration",
                          "From",
                          "To",
                          "Weight",
                          "X",
                          "Y")

