#' Pareto Barplot Function
#'
#' @param x data
#' @param mn main name
#' @param ... other stuff
#'
#' @return returns Pareto Barplot of x with title mn
#' @export
#'
#' @examples pareto(c(1,2,3,4),mn="Pareto Barplot")
pareto<-function(x,mn="Pareto barplot",...){  # x is a vector
x.tab=table(x)
xx.tab=sort(x.tab, decreasing=TRUE,index.return=FALSE)
cumsum(as.vector(xx.tab))->cs
length(x.tab)->lenx
bp<-barplot(xx.tab,ylim=c(0,max(cs)),las=2)
lb<-seq(0,cs[lenx],l=11)
axis(side=4,at=lb,labels=paste(seq(0,100,length=11),"%",sep=""),las=1,line=-1,col="Blue",col.axis="Red")
for(i in 1:(lenx-1)){
segments(bp[i],cs[i],bp[i+1],cs[i+1],col=i,lwd=2)
}
title(main=mn,...)

}
