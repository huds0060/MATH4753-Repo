#' ntickets
#'
#' @param N Number of seats on the plane
#' @param gamma probability of overbooking
#' @param p probability that a passenger will show
#'
#' @return outputs optimal number of tickets to sell while minimizing chance of overbooking
#' @export
#'
#' @importFrom stats pnorm
#' @importFrom graphics abline
#'
#' @examples ntickets(200,0.02,0.95)
ntickets <- function(N,gamma,p){
  layout(matrix(c(1,2,1,2), nrow=2,ncol=2), heights = lcm(6.8), widths = lcm(15))

  #Computes for Binomial Distribution
  f = 1- gamma - pbinom(N,c(1:(N+100)),p)
  discreteindex = which.min(abs(f))

  #Creates plot for Binomial Objective function
  plot(c(1:(N+100)),f,col = "red",pch = 20, xlim = c(N,ceiling(N+3*N*p*(1-p))), ylim = c(0,1),
       main = "Discrete Objective function vs. n", xlab = "n", ylab ="Objective",
       cex.main = 0.75, cex.lab = 0.75)
  abline(v = discreteindex)

  #Computes for Normal distribution
  g <- function(x) {(1 - gamma) - pnorm(N+0.5,x*p,sqrt(x*p*(1-p)))}
  gabs <- function(x) {abs((1 - gamma) - pnorm(N+0.5,x*p,sqrt(x*p*(1-p))))}
  continuousindex = optimize(gabs,interval = c(N,N+100), maximum = FALSE)

  #Creates plot for Normal Objective Function
  curve(g,col = "blue", pch = 20, xlim = c(N,ceiling(N+3*N*p*(1-p))), ylim = c(0,1),
        main = "Continuous Objective function vs. n", xlab = "n",ylab = "Objective",
        cex.main = 0.75, cex.lab = 0.75)
  abline(v = continuousindex$minimum)

  #Printing list
  print(list(nd = discreteindex,nc = continuousindex$minimum,N = N, p = p, gamma = gamma))

}

ntickets(200,0.02,0.95)

