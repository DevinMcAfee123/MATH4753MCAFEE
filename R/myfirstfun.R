#' Title
#' @title My first R function
#'
#' @param x A vector of quantitative data
#'
#' @return A vector of squared components
#' @export
#'
#' @examples
#' \dontrun{y<- 1:10; myfirstfun(y)}
myfirstfun <- function(x) {
  x^2
}
