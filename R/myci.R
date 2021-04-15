#' Title
#' @title Mean Confidence Interval
#' @param x sample
#'
#' @return 95% confidence interval for mean
#' @export
#'
#' @examples
#' \dontrun{myci(rnorm(30, 10, 12))}
myci = function(x){
  mean(x)+c(-1,1)*qt(1-0.05/2, length(x)-1)*sd(x)/sqrt(length(x))
}
