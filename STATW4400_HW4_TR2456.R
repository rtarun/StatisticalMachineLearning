##Tarun Ruchandani [TR2456]
##STATW4400 - Statistical Machine Learning
##Homework 4 >> Q3

setwd('/Users/tarunruchandani/Documents/STATW4400/Homeworks/Homework4')


##Matrix of Histograms
H<-matrix(readBin("histograms.bin", "double", 640000), 40000, 16)
## Adding a small constant 0.01 to all input histograms to mitigate problems due to empty ones
H <- H+0.01

MultinomialEM<- function(H,K,tau){
  print("Inside MultinomialEM")
  ##Number of histograms
  N <- nrow(H)
  
  ##Number of bins per histograms
  D <- ncol(H)
  
  ##Randomly generating initial centroids
  indices.for.Tk <- sample(1:N,K)
  
  ##Initial centroids
  Tk <- list()
  Tk <- H[indices.for.Tk,]
  
  ##Vector of cluster proportions
  Ck<- rep(1,K)/K
  
  ANew <- EStep(H,Ck,Tk,K)
  Delta1<-norm(ANew,"O")
  
  while(Delta1<tau){
    print(Delta1)
    AOld<-ANew
    list<-MStep(ANew,H,K)
    Ck<-list[[1]]
    Tk<-list[[2]]
    ANew<-EStep(H,Ck,Tk,K)
    A<-ANew-AOld
    Delta1<-norm(A,"O")
  }
  ##M<-c()
  for (i in 1:40000){
    M[i]<-which.max(ANew[i,])
  }
  return(M)
}

#EStep
EStep<-function(H,Ck,Tk,K){
  print("Inside EStep")
  Phi<- matrix(nrow =40000, ncol = K)
  Tk<-log(Tk)

  Phi= exp(H %*% t(Tk)) * Ck
  
  A <- matrix(nrow =40000, ncol = K)
  for (i in 1:40000){
    CL_PhiL<- sum(Ck*Phi[i,])
    for(l in 1:K){
      A[i,l]<-Ck[l]*Phi[i,l]/CL_PhiL
    }
  } 
  return(A)
}

#MStep
MStep<-function(A,H,K){
  print("Inside MStep")
  Ck<-Ck()
  Bk<-list()
  Tk<-list()
  
  for (i in 1:K){
    Ck[i] <- sum(A[,i])/40000
    iteration_count <- 0
    for(j in 1:40000){
      iteration_count<- iteration_count + A[j,i]*H[j,]
    }
    Bk[[i]]<- iteration_count
  }
  
  for (i in 1:K){
    Tk[[i]]<- Bk[[i]]/sum(Bk[[i]])
  }
  
  list<-list()
  list[[1]]<-Ck
  list[[2]]<-Tk
  return(list)
}

#Get Hard Assignment Vector M
M <- MultinomialEM(H,3,0.03)

#Visualizing Image
MM<-matrix(M,ncol = 200)
Z<-matrix(ncol = 200,nrow = 200)

for (i in 1:200){
  c<-MM[i,]
  d<-c()
  for (j in 1:200){
    d[j]<-c[201-j]
  }
  Z[i,]<-d
}

X<-1:200
Y<-1:200
image(X,Y,Z)

