ntickets <- function(N,gamma,p){
  #Computes for Binomial Distribution
  f = 1- gamma - pbinom(c(1:N),N,p)
  discreteindex = which.min(abs(f))
  print(discreteindex)

  #Creates plot for Binomial Objective function
  print(plot(c(1:N),f),main = "Discrete Objective function vs. n",)

  #Computes for Normal distribution
  g = 1- gamma - pnorm(c(1:N),N,p)
  continuousindex = which.min(abs(g))
  print(continuousindex)

  print(print(plot(c(1:N),g),main = "Continuous Objective function vs. n",))

}

ntickets(N = 400, gamma = 0.02, p = 0.50)

