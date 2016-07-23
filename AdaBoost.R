##Implementing AdaBoost Algorithm on USPS Digits Data

setwd('/')

mydata <- read.table("uspsdata.txt")
mylabels <- read.table("uspscl.txt")

#Train Function
train<- function(x,w,y){
  
  j<- ncol(x)
  n<- nrow(x)
  teta<- array()
  weight<-array()
  
  for (i in 1:j){
    newx<- sort(x[,i])
    neworder<- order(x[,i])
    w_cum<-array(w[neworder[1]]*y[neworder[1]])
    
    for (k in 2:n){
      w_cum[k] <- w_cum[k-1]+w[neworder[k]]*y[neworder[k]]
    }
    if (w_cum[1]>0){
      location<- which.max(w_cum)
      weight[i]<- max(w_cum)
    }else{
      location<- which.min(w_cum)
      weight[i]<- min(w_cum)
    }
    teta[i]<-newx[location]
  }
  
  j<-which.max(weight)
  finalteta<-teta[j]
  if (weight[j]>0){
    m<-1
  }else{
    m<--1
  }
  
  return (list(j,finalteta,m))
}

#Classify Function

classify<- function(X,pars){
  j<-pars[[1]][1]
  theta<-pars[[2]][1]
  m<- pars[[3]][1]
  n<- nrow(X)
  y<-array()
  
  x<-X[,j]
  for (i in 1:n){
    if (x[i]>theta){
      y[i]<-m
    }else{
      y[i]<--m
    }
  }
  return(y)
}


#agg_class Function

agg_class<-function(X,alpha,allPars){
  npar<- nrow(allPars)
  n<- nrow(X)
  label<-array()
  
  for (i in 1:n){
    x<-X[i,]
    sum<-0
    for (k in 1:npar){
      j<-allPars[k,1]
      theta<-allPars[k,2]
      m<-allPars[k,3]
      if (x[j]>theta){
        sum<-sum + alpha[k]*m
      }else{
        sum<-sum - alpha[k]*m
      }
    }
    if (sum>0){
      label[i]<-1
    }else{
      label[i]<--1
    }
  }
  return(label)
}


