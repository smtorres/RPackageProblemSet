#' Reg.Vars
#'
#' Calculate coefficients of all possible regressions from a set of covariates
#'
#' @param y A numeric object with the same number of observations as rows in \code{x}
#' @param x A matrix with a number of rows equal to dimensionality of \code{y}.
#'
#' @return A list with the elements
#'  \item{matrix}{Matrix with the coefficients for all regressions computed}
#'  \item{numeric}{Vector with the R-squared of every regression computed}
#' @author Michelle Torres
#' @note Use it wisely
#' @examples
#' 
#' myX <- matrix(c(2,1,8,3,6,7,9,3,6,1,8,3,6,9,3,6), ncol=4) 
#' myY <- myX[,3]*3.5
#' reg.vars(myY, myX)
#' @seealso \code{\link{sig.store}}
#' @rdname reg.vars
#' @export
reg.vars<-function(y,x){
  require(combinat)
  require(plyr)
  b<-NULL
  a<-ncol(x)
  for(i in 1:ncol(x)){
    b[i]<-choose(a,i)
  }
  num<-sum(b)
  vars<-list()
  for (k in 1:ncol(x)){
    vars[[k]]<-combn(ncol(x),k)
  }
  vars[[ncol(x)]]<-as.matrix(vars[[ncol(x)]])
  covs.coeffs<-list()
  covs.store<-function(mat, ind, dep){
    cov.names<-paste0("x", 1:ncol(mat))
    colnames(mat)<-cov.names
    xnam<-paste0("x", ind)
    dep2<-dep
    for (i in 1:ncol(mat)){
      assign(paste("x",i,sep=""), mat[,i])
    }
    (fmla <- as.formula(paste("dep2 ~ ", paste(xnam, collapse= "+"))))
    coeffs<-summary(lm(fmla))$coefficients[,1]
    output<-list(coeffs)
    names(output)<-"Coefficients"
    return(output)
  }
  for (i in 1:length(vars)){
    covs.coeffs[[i]]<-apply(vars[[i]], 2, FUN=covs.store, mat=x, dep=y)
  }
  matrix.trans<-function(z){
    new<-list()
    for (i in 1:length(z)){
      new[[i]]<-as.data.frame(t(as.matrix(unlist(z[[i]]))))
    }
    return(new)
  }
  covs.coeffs<-lapply(covs.coeffs, matrix.trans)
  reg.coeffs<-lapply(covs.coeffs, rbind.fill)
  reg.coeffs<-rbind.fill(reg.coeffs)
  r.store<-function(mat, ind, dep){
    cov.names<-paste0("x", 1:ncol(mat))
    colnames(mat)<-cov.names
    xnam<-paste0("x", ind)
    dep2<-dep
    for (i in 1:ncol(mat)){
      assign(paste("x",i,sep=""), mat[,i])
    }
    (fmla <- as.formula(paste("dep2 ~ ", paste(xnam, collapse= "+"))))
    r2<-summary(lm(fmla))$r.squared
    return(r2)
  }
  r.reg<-list()
  for (i in 1:length(vars)){
    r.reg[[i]]<-apply(vars[[i]], 2, FUN=r.store, mat=x, dep=y)
  }
  r.reg<-unlist(r.reg)
  res<-list(reg.coeffs, r.reg)
  names(res)<-c("Coeffs", "R-squared")
  return(res)
}

