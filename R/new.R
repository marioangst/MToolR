

#' Is the object of class mtoolr?
#'
#' @param object An R object
#'
#' @return TRUE if the object is of class mtoolr
#' @export
#'
#' @examples
#' is_mtoolr(example_models)
#' is_mtoolr(mtcars)
is_mtoolr <- function(object){
  "mtoolr" %in% class(object)
}

stop_if_not_mtoolr <- function(object){
  if(!(is_mtoolr(object))){
    stop("Object supplied is not an mtoolr object.
         You can create an mtoolr object with eg. `mentalmodel_from_csv()`")
  }
}

#' mtoolr object constructor
#'
#' This is the internal object constructor for the mtoolr package.
#' The user should generally not have to engage with it.
#'
#' @param x A list object passing `is_valid_mtool_edgelist`
#' @param aggregated A boolean - is the object aggregated?
#' @param concepts An optional list of unique concepts in the mental model. Present to guard against the case of isolates after aggregation.
#'
#' @return
#' @export
#'
#' @examples
new_mtoolr <- function(x = list(),
                       aggregated = logical(),
                       concepts = NULL){

  stopifnot(is.logical(aggregated))

  mentalmodel <- list()
  mentalmodel$data <- tibble::tibble(x)
  if(is.null(concepts)){
    mentalmodel$concepts <- unique(c(x$To,x$From))
  }
  if(!is.null(concepts)){
    mentalmodel$concepts <- concepts
  }

  mentalmodel$graph <- igraph_from_mtools_el(x,
                                             concepts = mentalmodel$concepts)

  if(!(aggregated)){
    mentalmodel$user_data <- tibble::tibble(
      id = unique(mentalmodel$data$User_ID)
    )
    if(
      !(is_valid_mtool_edgelist(x))){
      stop("The provided mtool edgelist is not valid")
    }
    check_mtool_columns_exist(x)
    mentalmodel$users <- users_graphs_constructor(edgelist = mentalmodel$data,
                                                  user_list = mentalmodel$user_data$id,
                                                  concepts = mentalmodel$concepts)
  }
  structure(mentalmodel,
            class = "mtoolr",
            aggregated = aggregated)
}

#' Create a mtoolr object
#'
#' This function creates a mtoolr object from a data frame parsed from MTool output.
#' This function is useful if you want to process exported data from MTool before passing it to MToolR.
#' The safest and easiest way to import MTool data directly is to use the `mentalmodel_from_csv()`
#' function however.
#'
#' @param x An object to be converted to a mtoolr object for further analysis. Likely a data frame
#' with at least User_ID, To, From and Weight columns specifying mental model edges by user.
#' @param aggregated Is the object an aggregated model? Defaults to FALSE.
#'
#' @return A object of class mtoolr
#' @export
#'
#' @examples
#'
#' minimal_df <- data.frame(User_ID = c("User1"), To = c("Concept1"), From = c("Concept2"), Weight = c(2))
#' mentalmodel(minimal_df)
mentalmodel <- function(x,
                        aggregated = FALSE){
  new_mtoolr(x,aggregated = aggregated)
}

#' Add additional data on users to an mtoolr object
#'
#' This function allows you to add additional data on users who created mental
#' models, eg. age, gender, whether they like cycling... This data can then
#' be used to eg. aggregate or compare models by variables.
#'
#' @param mentalmodel An mtoolr object to add data to
#' @param user_data A data frame containing at least a column to match
#' user M-Tool user ids. It must contain an entry for all users.
#' @param id_key The key of the data frame column to match M-Tool user ids.
#'
#' @return
#' @export
#'
#' @examples
#' # simulate user data to add, including a column "id" to match M-Tool user data
#' user_df <- data.frame(id = example_models$user_data$id,var = rnorm(length(example_models$user_data$id)))
#' # add user data by matching on column "id
#' example_models <- example_models |> add_user_data(user_data = user_df,id_key = "id")
add_user_data <- function(mentalmodel,
                          user_data,
                          id_key){

  if(!is_valid_user_data(mentalmodel = mentalmodel, user_data = user_data, id_key = id_key)){
    stop("The provided user data is not valid. Did you use the wrong id key? Is there an entry for all users?")
  }

  joined_data <- mentalmodel$user_data |> dplyr::right_join(user_data, by = c("id" = id_key))
  n_new_cols <- ncol(joined_data) - ncol(mentalmodel$user_data)
  mentalmodel$user_data <- joined_data
  logger::log_info("Added {n_new_cols} columns to the data")
  return(mentalmodel)
}

is_valid_mtool_edgelist <- function(edgelist){
  all(
    is.list(edgelist),
    check_necessary_mtool_columns_exist(edgelist))
}

is_valid_user_data <- function(mentalmodel,user_data, id_key){
  all(mentalmodel$user_data$id %in% user_data[[id_key]])
}

is_aggregated <- function(x){
  attributes(x)$aggregated
}

check_mtool_columns_exist <- function(x){
  missing <- MTOOL_EXPORT_COLUMNS[!(MTOOL_EXPORT_COLUMNS %in% colnames(x))]
  if(length(missing) > 0){
    logger::log_warn(paste0(c("Usually expected columns", missing, "not in data frame"),collapse = " "))
  }
}

check_necessary_mtool_columns_exist <- function(x){
  MTOOL_EXPORT_COLUMNS_NECESSARY %in% colnames(x)
}

check_group_var_is_valid <- function(mentalmodel,
                                     group_var){
  if(!(group_var %in% colnames(mentalmodel$user_data))){
    stop(paste0("The grouping variable is not in the user data. Existing columns: ",
                paste0(colnames(mentalmodel$user_data),collapse = ",")))
  }
  else{
    return(TRUE)
  }
}

#' Get the igraph graph object for a given user
#'
#' @param user The id of the user as stored in the User_ID column of the M-Tool output.
#' @param x A mtoolr object
#'
#' @return
#' @export
#'
#' @examples
get_user_graph <- function(user,x){
  stop_if_not_mtoolr(x)
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

MTOOL_EXPORT_COLUMNS_NECESSARY <-
  c("User_ID",
    "From",
    "To",
    "Weight")

MTOOL_EXPORT_COLUMNS <-
  c("User_ID",
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

