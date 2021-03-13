#' Title
#' @title Normal Distribution and Cumulative Probability Plot
#'
#' @param mu mean of the normal distribution
#' @param sigma standard deviation of the normal distribution
#' @param a for a given a, this function calculates P(Y<=a) for the given normal distribution
#'
#' @return the cumulative probability P(Y<=a)
#' @export
#'
#' @examples
#' \dontrun{myncurve(mu=10,sigma=5, a=6)}
myncurve = function(mu, sigma, a){

  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma),xlab= "x", ylab="Normal Density", main = paste0("Mean = ", mu, ", Sd = ",sigma))

  xcurve = seq(mu-3*sigma,a,length=1000)
  ycurve = dnorm(xcurve,mu, sigma)
  polygon(c(mu-3*sigma, xcurve,a),c(0,ycurve,0), col="Red")

  prob = pnorm(a, mu,sigma)

  prob = round(prob,4)

  prob
}
