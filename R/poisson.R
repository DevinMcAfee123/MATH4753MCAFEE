#' Title
#' @title Poisson Probability Distribution
#' @param lambda mean number of events during given units of time, volume, or area.
#' @param y value of poisson random variable
#'
#' @return probability of y
#' @export
#'
#' @examples
#' \dontrun{prob_3=poisson(2,3)}
poisson = function(lambda,y) {
  (lambda^y)*(exp(-lambda))/factorial(y)
}
