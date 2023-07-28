#' myncurve
#'
#' @param mu mean
#' @param sigma standard deviation
#' @param a upper limit of pnorm
#'
#' @return Plots normal curve with mean = mu, sd = sigma, shades area from -inf to a, and prints the area.
#' @export
#'
#' @examples myncurve(0,1,0.5)
myncurve = function(mu,sigma,a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu +
                                              3*sigma))
  xcurve=seq(mu - 3*sigma,a,length=1000)
  ycurve=dnorm(xcurve,mean=mu,sd=sigma)
  polygon(c(mu - 3*sigma,xcurve,a),c(0,ycurve,0),col="Red")
  prob= pnorm(a,mean=mu,sd=sigma)
  prob=round(prob,4)
  paste("Area = ", prob, sep="")
}
