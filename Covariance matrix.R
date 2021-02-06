##zufällige Korrelationsmatrix zu generieren mit runif()##

##mit runif()##
n=3
Q=function(n){
  o=matrix((runif(n*n,min=1,max=9)),nrow=n)#zufällige Matrix zu erstellen#
  for (i in 1:n)
  {
    if(o[i,i]>0)
    {o[i,i]=o[i,i]}
    else
    {o[i,i]=-o[i,i]}
    for(j in 1:n)
    {o[i,j]=o[j,i]
    }#Diagonalmatrix zu erstellen#
  }
  return(o)
}
Q=Q(n)

##mit rnorm()##
  n=3
  Q=function(n){
    o=matrix((rnorm(n*n,mean=10,sd=1)),nrow=n)#zufällige Matrix zu erstellen#  
    for (i in 1:n)
    {
      if(o[i,i]>0)
      {o[i,i]=o[i,i]}
      else
      {o[i,i]=-o[i,i]}
      for(j in 1:n)
      {o[i,j]=o[j,i]
      }#Diagonalmatrix zu erstellen#
    }
    return(o)
  }
  Q=Q(n)

  
##mit Copula()##
n=3
library(Copula)
Q=function(n)
  {
   Gauss.C=ellipCopula(family="normal",param=c(0.8,0.1,0.1),dim=n,dispstr="un")
   o=rCopula(n,Gauss.C)
    for (i in 1:n)
  {
    if(o[i,i]>0)
    {o[i,i]=o[i,i]}
    else
    {o[i,i]=-o[i,i]}
    for(j in 1:n)
    {o[i,j]=o[j,i]
    }#Diagonalmatrix zu erstellen#
  }
  return(o)
}
Q=Q(n)


