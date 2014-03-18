#' sig.store
#'
#' Determines if the coefficients of the regressions computed by reg.vars are significant at the 95% level
#'
#' @param y A numeric object with the same number of observations as rows in \code{x}
#' @param x A matrix with a number of rows equal to dimensionality of \code{y}.
#'
#' @return A list with the elements
#'  \item{sig.store}{Matrix with 1 if the coefficient is significant or 0 otherwise}
#' @author Michelle Torres
#' @note  
#' @examples
#' 
#' myX <- matrix(c(2,1,8,3,6,7,9,3,6,1,8,3,6,9,3,6), ncol=4) 
#' myY <- myX[,3]*3.5
#' sig.store(myX, myY)
#' @seealso \code{\link{reg.vars}}
#' @rdname reg.vars
#' @export
sig.store<-function(y,x){
  vars<-list()
  for (k in 1:ncol(x)){
    vars[[k]]<-combn(ncol(x),k)
  }
  vars[[ncol(x)]]<-as.matrix(vars[[ncol(x)]])
  covs.sigs<-list()
  ####  
  sig.covs<-function(mat, ind, dep){
    cov.names<-paste0("x", 1:ncol(mat))
    colnames(mat)<-cov.names
    xnam<-paste0("x", ind)
    dep2<-dep
    for (i in 1:ncol(mat)){
      assign(paste("x",i,sep=""), mat[,i])
    }
    (fmla <- as.formula(paste("dep2 ~ ", paste(xnam, collapse= "+"))))
    sigs<-summary(lm(fmla))$coefficients[,4]
    sigs<-ifelse(sigs<=0.05,1,0)
    output<-list(sigs)
    names(output)<-"Coefficients"
    return(output)
  }
  ##
  for (i in 1:length(vars)){
    covs.sigs[[i]]<-apply(vars[[i]], 2, FUN=sig.covs, mat=x, dep=y)
  }
  matrix.trans<-function(z){
    new<-list()
    for (i in 1:length(z)){
      new[[i]]<-as.data.frame(t(as.matrix(unlist(z[[i]]))))
    }
    return(new)
  }
  covs.sigs<-lapply(covs.sigs, matrix.trans)
  reg.sigs<-lapply(covs.sigs, rbind.fill)
  reg.sigs<-rbind.fill(reg.sigs)
  return(reg.sigs)
}