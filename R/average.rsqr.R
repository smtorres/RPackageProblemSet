#' average.rsqr
#'
#' Compute the average R squared of the models including each covariate
#'
#' @param y A numeric object with the same number of observations as rows in \code{x}
#' @param x A matrix with a number of rows equal to dimensionality of \code{y}.
#'
#' @return A list with the elements
#'  \item{numeric}{Vector with R average of the models including each covariate}
#' @author Michelle Torres
#' @note  Use it wisely
#' @examples
#' 
#' myX <- matrix(c(2,1,8,3,6,7,9,3,6,1,8,3,6,9,3,6), ncol=4) 
#' myY <- myX[,3]*3.5
#' average.rsqr(myY, myX)
#' @seealso \code{\link{reg.vars}}
#' @rdname reg.vars
#' @export
average.rsqr<-function(y,x){
  require(combinat)
  require(plyr)
  new2<-cbind(reg.vars(y,x)[[1]], reg.vars(y,x)[[2]])
  new3<-new2[,2:(ncol(x)+1)]
  r.sqr<-new2[,(ncol(x))+2]
  r.average<-NULL
  for (i in 1:ncol(x)){
    r.average[i]<-mean(r.sqr[is.na(new3[,i])==FALSE])
  }
  return(r.average)
}