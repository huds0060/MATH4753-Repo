#' myboot2
#'
#' @param iter number of times we resample (with replacement)
#' @param x original sample data
#' @param fun If we want to approx fun(population), fun is the function "fun"
#' @param alpha value of alpha for conf int
#' @param cx  Text size for histogram
#' @param ... Other graphical parameters
#'
#' @return prints Histogram of bootstrap resample distribution with estimate of fun(population) with a 100(1-a)% confidence interval
#' @export
#'
#' @examples myboot2(iter=10000,c(1,1,1,1,3,3,3,3,3,3,4,5,1,1,1,6,2,2,2,2),fun = median, alpha = 0.20)
myboot2<-function(iter=10000,x,fun="mean",alpha=0.05,cx=1.5,...){
  n=length(x)

  y=sample(x,n*iter,replace=TRUE)
  rs.mat=matrix(y,nr=n,nc=iter,byrow=TRUE)
  xstat=apply(rs.mat,2,fun)
  ci=quantile(xstat,c(alpha/2,1-alpha/2))

  para=hist(xstat,freq=FALSE,las=1,
            main=paste("Histogram of Bootstrap sample statistics","\n","alpha=",alpha," iter=",iter,sep=""),
            ...)


  mat=matrix(x,nr=length(x),nc=1,byrow=TRUE)


  pte=apply(mat,2,fun)
  abline(v=pte,lwd=3,col="Black")
  segments(ci[1],0,ci[2],0,lwd=4)
  text(ci[1],0,paste("(",round(ci[1],2),sep=""),col="Red",cex=cx)
  text(ci[2],0,paste(round(ci[2],2),")",sep=""),col="Red",cex=cx)


  text(pte,max(para$density)/2,round(pte,2),cex=cx)

  invisible(list(ci=ci,fun=fun,x=x,xstat = xstat))   #List now contains xstat
}
