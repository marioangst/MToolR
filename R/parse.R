
library("logger")
library("dplyr")
library("igraph")

#' Parse CSV file exported by M-Tool
#'
#' @param csv_file Path to a .csv file exported from M-Tool
#' @param exclude_nonresponse Should non-responses be excluded? Defaults to TRUE
#'
#' @return An object of class mtoolr containing all information included in the M-Tool export
#' @export
#'
#' @examples
mentalmodel_from_csv <- function(csv_file,
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

  new_mentalmodel <- new_mtoolr(edgelist)

  return(new_mentalmodel)
}






