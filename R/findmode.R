#' findmode
#'
#' @param x
#'
#' @return returns the mode of the vector inputted.
#' @export
#'
#' @examples
findmode <- function(x){
  unique(x)[which.max(tabulate(match(x,unique(x))))]
}
