
library("logger")
library("dplyr")
library("igraph")

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

get_aggregate_el <- function(edgelist){
  edgelist <- edgelist |>
    group_by(From,To) |>
    summarise(Weight = sum(abs(Weight)), .groups = 'keep' )  |>
    ungroup()
  return(edgelist)
}

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

igraph_from_mtools_el <- function(edgelist){
  g <- igraph::graph_from_data_frame(edgelist[,c("From","To","Weight")])
  g <- igraph::set_edge_attr(graph = g, name = "weight",
                        value= edgelist$Weight)
  return(g)
}
