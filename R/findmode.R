#' findmode
#'
#' @param x vector of data
#'
#' @return returns the mode of the vector inputted.
#' @export
#'
#' @examples findmode(c(1,2,1,3,4,1,1))
findmode <- function(x){
  unique(x)[which.max(tabulate(match(x,unique(x))))]
}
