
#' Default mental model print method
#'
#' @param x The mental model
#' @param ...
#'
#' @return Prints a tibble of the raw data and the combined graph object
#' @export
#'
#' @examples
print.mtoolr <- function(x,...){
  print(x$data)
  print(x$graph)
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
