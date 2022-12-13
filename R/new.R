

#' Is the object of class mtoolr
#'
#' @param object An R object
#'
#' @return TRUE if the object is of class mtoolr
#' @export
#'
#' @examples
is_mtoolr <- function(object){
  "mtoolr" %in% class(object)
}

new_mtoolr <- function(x = list()){
  is_valid_mtool_edgelist(x)
  mentalmodel <- list()
  mentalmodel$data <- tibble::tibble(x)
  mentalmodel$users <- users_graphs_constructor(edgelist = mentalmodel$data,
                                                user_list = unique(mentalmodel$data$User_ID))
  mentalmodel$graph <- igraph_from_mtools_el(x)
  structure(mentalmodel,
            class = "mtoolr")
}

mentalmodel <- function(x){
  new_mtoolr(x)
}

is_valid_mtool_edgelist <- function(edgelist){
  all(
    is.list(edgelist),
    check_mtool_columns_exist(edgelist))
}

check_mtool_columns_exist <- function(x){
  MTOOL_EXPORT_COLUMNS %in% colnames(x)
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
#'
#' @return A weighted, directed igraph object
#' @export
#'
#' @examples
igraph_from_mtools_el <- function(edgelist,
                                  from_col = "From",
                                  to_col = "To",
                                  weight_col = "Weight"){
  g <- igraph::graph_from_data_frame(edgelist[,c(from_col,to_col,weight_col)])
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

get_user_graph <- function(edgelist,user){
  user_el <- get_user_el(edgelist,user)
  user_graph <- igraph_from_mtools_el(user_el)
  return(user_graph)
}

users_graphs_constructor <- function(edgelist, user_list){
  user_graph_list <-
    lapply(user_list, function(x){
      list(
        data = get_user_el(user = x, edgelist = edgelist),
        graph = get_user_graph(user = x, edgelist = edgelist),
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

