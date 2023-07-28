mychisim <-function(iter = 1000, n = 10){     #These are default options for iter and n!

  mat <- matrix(data = NA, nrow = n, ncol = iter, byrow = TRUE) #data NA will populate the matrix with NA's in the entries.

  for(i in 1:iter){
    mat[,i] <- rnorm(n = n,mean = 0, sd = 1)^2         #populates ith column with the kth z^2 terms of generating function
                                                       #The kth row is just a vector of length iter where each component is the kth z^2 term

  }

  stat <- apply(mat,2,sum)           #sums the columns of the matrix, forming a vector

  h <- hist(stat, plot = FALSE)      #stores the stats that make up the histogram in h; ddoes not make a frequency histogram; does not store the graph/plot itself.

  dd <- h$density
  cll <- dd/max(dd)

  hist(stat, freq = FALSE, col = rgb(0,0,cll))

  curve(dnorm(x,mean=n,sd=sqrt(2*n)), add = TRUE, lwd = 2, col = "Red")

}

#windows();mychisim(iter = 10000, n = 10)
