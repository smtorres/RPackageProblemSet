##NUMBER OF REGRESSIONS TO RUN AND POSSIBLE COMBINATIONS
#Input: x->matrix of covariates
#       y-> vector with a dependent variable
#Output: 

reg.vars<-function(y,x){
  require(combinat)
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
##REGRESSIONS
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
##
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
##R-squared
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
#####
res<-list(reg.coeffs, r.reg)
names(res)<-c("Coeffs.", "R-squared")
return(res)
}

###Importance of the coefficients
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

####AVERAGE R
average.r<-function(y,x){
new2<-cbind(reg.vars(y,x)[[1]], reg.vars(y,x)[[2]])
new3<-new2[,2:(ncol(x)+1)]
r.sqr<-new2[,(ncol(x))+2]
r.average<-NULL
for (i in 1:ncol(x)){
  r.average[i]<-mean(r.sqr[is.na(new3[,i])==FALSE])
}
return(r.average)
}