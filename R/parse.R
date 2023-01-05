
library("logger")
library("dplyr")
library("igraph")

#' Import CSV file exported by M-Tool as mtoolr object
#'
#' @param csv_file Path to a .csv file exported from M-Tool
#' @param exclude_nonresponse Should non-responses be excluded? Defaults to TRUE
#'
#' @return An object of class mtoolr containing all information included in the M-Tool export
#' @export
#'
#' @examples
#' csv_path <- system.file("extdata", "example_export.csv", package = "MToolR", mustWork = TRUE)
#' mental_model <- mentalmodel_from_csv(csv_path, exclude_nonresponse = TRUE)
#' mental_model
mentalmodel_from_csv <- function(csv_file,
                                 exclude_nonresponse = TRUE){

  edgelist <- parse_mtool_csv(csv_file,
                              exclude_nonresponse)
  logger::log_info("Loaded {nrow(edgelist)} mental model edges from
                     {length(unique(edgelist$User_ID))} users")

  new_mentalmodel <- new_mtoolr(edgelist, aggregated = FALSE)

  return(new_mentalmodel)
}

#' Parse an M-Tool csv file to a data frame
#'
#' Use this function if you want to edit your M-Tool data before
#' using it with MToolR. In most cases, you'll want to use
#' `mentalmodel_from_csv()` directly.
#'
#' @param csv_file Path to a .csv file exported from M-Tool
#' @param exclude_nonresponse Should non-responses be excluded? Defaults to TRUE
#'
#' @return A data frame object of your mtool csv
#' @export
#'
#' @examples
#' csv_path <- system.file("extdata", "example_export.csv", package = "MToolR", mustWork = TRUE)
#' mental_model_df <- parse_mtool_csv(csv_path, exclude_nonresponse = TRUE)
#' mental_model_df
parse_mtool_csv <- function(csv_file,
                            exclude_nonresponse){
  stopifnot(is.logical(exclude_nonresponse))
  if (!grepl("\\.csv$", csv_file)){
    stop("Please supply a .csv file produced by M-Tool")
  }
  else{
    edgelist <-
      read.csv2(file = csv_file,
                head = TRUE,
                sep = ",",
                fill = TRUE,
                encoding = "UTF-8")
    if(exclude_nonresponse){
      nonresponses <- edgelist$From == '' | edgelist$To == ''
      edgelist <- edgelist[!nonresponses,]
      logger::log_info("Excluded {sum(nonresponses)} rows with nonresponses")
    }
    return(edgelist)
  }
}






