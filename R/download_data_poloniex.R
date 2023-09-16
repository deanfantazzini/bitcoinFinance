#' Pipe function to unite two characters
#'
#' @param e1 is the first term to be united
#' @param e2 is the second term to be united
#'
#' @export
#' @examples
#' "United " %p% "States"

`%p%` <- function(e1,e2) {
  # This pipe unites two characters. It works similarly to '+' in python.
  return(paste0(e1,e2))
}

