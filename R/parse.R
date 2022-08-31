
library("logger")
library("dplyr")
library("igraph")

#' Parse CSV file exported by M-Tool
#'
#' @param csv_file Path to a .csv file exported from M-Tool
#' @param exclude_nonresponse Should non-responses be excluded? Defaults to TRUE
#'
#' @return A data.frame object containing all information included in the M-Tool export
#' @export
#'
#' @examples
parse_mtools_csv <- function(csv_file,
                             exclude_nonresponse = TRUE){
  if (!grepl("\\.csv$", csv_file)){
    stop("Please supply a .csv file produced by M-Tool")
  }
  else {
    edgelist <-
    read.csv2(file = csv_file,
              head = TRUE,
              sep = ",",
              fill = TRUE )
    if(exclude_nonresponse){
      nonresponses <- edgelist$From == '' | edgelist$To == ''
      edgelist <- edgelist[!nonresponses,]
      logger::log_info("Excluded {sum(nonresponses)} rows with nonresponses")
    }
    logger::log_info("Loaded {nrow(edgelist)} mental model edges from
                     {length(unique(edgelist$User_ID))} users")
  }
  return(edgelist)
}

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
    group_by(From,To) |>
    summarise(Weight = sum(abs(Weight)), .groups = 'keep' )  |>
    ungroup()
  return(edgelist)
}

#' Get the edgelist of a specific user of M-Tool
#'
#' @param edgelist Likely a raw edgelist data frame generated from parse_mtools_csv.
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

#' Create an igraph graph object from a M-Tool edgelist
#'
#' Create a weighted, direct igraph graph object from M-Tool data.
#' This makes it easy to process the data further using various network
#' statistics
#'
#' @param edgelist Likely a raw edgelist data frame generated from parse_mtools_csv.
#'
#' @return A weighted, directed igraph object
#' @export
#'
#' @examples
igraph_from_mtools_el <- function(edgelist){
  g <- igraph::graph_from_data_frame(edgelist[,c("From","To","Weight")])
  g <- igraph::set_edge_attr(graph = g, name = "weight",
                        value= edgelist$Weight)
  return(g)
}
