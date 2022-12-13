

#' Get a aggregate edgelist from M-Tool Data
#'
#' This function returns a edgelist of aggregate weights
#' (the simple sum function) given an M-Tool dataframe supplied
#'
#' @param edgelist Likely a raw edgelist data frame generated from parse_mtools_csv.
#'
#' @return A edgelist (data frame) with three columns: To, From, Weight
#' @export
#'
#' @examples
get_aggregate_el <- function(edgelist){
  edgelist <- edgelist |>
    dplyr::group_by(From,To) |>
    dplyr::summarise(Weight = sum(abs(Weight)), .groups = 'keep' )  |>
    dplyr::ungroup()
  return(edgelist)
}

#' Create overview stats over the aggregate mental model
#'
#' @param edgelist Likely a raw edgelist data frame generated from parse_mtools_csv.
#'
#' @return A data frame with the following stats:
#' - weighted betwenness
#' - weighted indegree
#' - weighted outdegree
#' - weighted total degree
#'
#' @export
#'
#' @examples
calculate_aggregate_stats <- function(edgelist){
  df <- get_aggregate_el(edgelist)
  g <- igraph_from_mtools_el(df)
  data.frame(
    w_betweenness = igraph::betweenness(g),
    w_in_degree = igraph::degree(g,mode = "in"),
    w_out_degree = igraph::degree(g, mode = "out"),
    w_total_degree = igraph::degree(g, mode = "total")
  )
}

#' Create overview stats over a single user mental model
#'
#' @param edgelist Likely a raw edgelist data frame generated from parse_mtools_csv.
#' @param user A user ID as appearing in the User_ID column of the edgelist supplied
#'
#' @return A data frame with the following stats:
#' - weighted betwenness
#' - weighted indegree
#' - weighted outdegree
#' - weighted total degree
#'
#' @export
#'
#' @examples
calculate_user_stats <- function(edgelist, user){
  df <- get_user_el(edgelist,user)
  g <- igraph_from_mtools_el(df)
  data.frame(
    w_betweenness = igraph::betweenness(g),
    w_in_degree = igraph::degree(g,mode = "in"),
    w_out_degree = igraph::degree(g, mode = "out"),
    w_total_degree = igraph::degree(g, mode = "total")
  )
}

