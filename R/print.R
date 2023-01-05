
#' Default mental model print method
#'
#' @param x The mental model
#' @param ... Further arguments to print
#'
#' @return Prints a tibble of the raw data and the combined graph object
#' @export
#'
#' @examples
print.mtoolr <- function(x, ...){
  logger::log_info("Data on links between concepts:")
  print(x$data)
  if(!is_aggregated(x)){
    logger::log_info("Data on individual users:")
    print(x$user_data)
  }
  if (is_aggregated(x)){
    logger::log_info("Aggregated graph object:")
    print(x$graph)
  }
}

#' Default mental model show method
#'
#' @param x The mtoolr object
#'
#' @return
#' @export
#'
#' @examples
show.mtoolr <- function(x){
  print.mtoolr(x)
}
