##die Menge von Teile##
n=3
##zufällige Korrelationsmatrix zu generieren mit runif()##
Q=function(n){
  o=matrix((runif(n*n,min=1,max=9)),nrow=n)#zufällige Matrix zu erstellen#
  for (i in 1:n)
  {
    if(o[i,i]>0)
    {o[i,i]=o[i,i]}
    else
    {o[i,i]=-o[i,i]}#diagonale Elemente positiv#
    for(j in 1:n)
    {o[i,j]=o[j,i]
    }#Diagonalmatrix zu erstellen#
  }
  return(o)
}
Q=Q(n)
##Einheitsvektor zu erstellen##
E=function(n)
{
  e=matrix(rep(1,n),nrow=1,ncol=n)
  return(e)
}
E=E(n)

##########Standard-alone risk##########
VaR.=numeric()
#VaR von Allokation zu berechnen#
for(i in 1:n)
{VaR.[i]=sqrt(Q[i,i])}
VaR.12=sqrt((e_12)%*%Q%*%t(e_12))
VaR.13=sqrt((e_13)%*%Q%*%t(e_13))
VaR.23=sqrt((e_23)%*%Q%*%t(e_23))


##########Activity-level Method##########
VaR_n=sqrt(E%*%P%*%t(E))#VaR von voll-Allokation#
VaR_s=numeric()
VaR_a=numeric()
distance_a=numeric()
for(i in 1:n)
{
  VaR_s[i]=sqrt(P[i,i])
  }
sum_VaR_s=0
for(i in 1:n)
  {sum_VaR_s=sum_VaR_s+VaR_s[i]}#die Summe von VaR von Allokation#
for(i in 1:n) 
{VaR_a[i]=(VaR_s[i]*VaR_n)/sum_VaR_s}
sum_VaR_a=0
for(i in 1:n)
  {sum_VaR_a=sum_VaR_a+VaR_a[i]}#die Summe von Risikokapital#
  
distance_a=abs((sum_VaR_a-sum_VaR_s)/n)#Distanz zwischen den Summe von VaR und Risikokapital# 

sum_core_a=numeric()
for(i in 1:n)
{
  sum_core_a=0
  {if(VaR.[i]>VaR_a[i])
  {sum_core_a=sum_core_a+VaR_a[i]}
  else
  {sum_core_a=sum_core_a+0}
  }
  if((VaR_a[1]+VaR_a[2])<VaR.12)
  {sum_core_a=sum_core_a+VaR_a[1]+VaR_a[2]}
  if((VaR_a[1]+VaR_a[3])<VaR.13)
  {sum_core_a=sum_core_a+VaR_a[1]+VaR_a[3]}
  if((VaR_a[2]+VaR_a[3])<VaR.23)
  {sum_core_a=sum_core_a+VaR_a[2]+VaR_a[3]}
  return(sum_core_a)#die Summe von kern-Allokation#
  
}

percent_a=sum_core_a/(sum_VaR_a*3+VaR_n)#Prozent von kern-Allokation#

####Incremental method####
deta_VaR_I=numeric()
sum_deta_VaR_I=numeric()
VaR_I=numeric()
distance_I=numeric()
for(i in 1:n)
{
  for(j in 1:n)
   {if(i!=j)
   {
     E[1,i]=0
     E[1,j]=1
   }##die Matrix zeigt, ob Teil i eine Teilmenge der Koalition K ist.
  }
  deta_VaR_I[i]=abs(VaR_n-(E%*%P%*%t(E)))##Incremente in der Koalition K

sum_deta_VaR_I=0
for(i in 1:n)
  {sum_deta_VaR_I=sum_deta_VaR_I+deta_VaR_I[i]
  VaR_I[i]=(deta_VaR_I[i]*VaR_n)/sum_deta_VaR_I}
sum_VaR_I=0
for(i in 1:n)
{sum_VaR_I=sum_VaR_I=VaR_I[i]}##die Summe der Incremente 
 
distance_I=abs((sum_VaR_I-sum_VaR_s)/n)##die Entfernung vom Kern

sum_core_I=numeric()
for(i in 1:n)
{
  sum_core_I=0
  {if(VaR.[i]>VaR_I[i])
  {sum_core_I=sum_core_I+VaR_I[i]}
    else
    {sum_core_I=sum_core_I+0}
  }
  if((VaR_I[1]+VaR_I[2])<VaR.12)
  {sum_core_I=sum_core_I+VaR_I[1]+VaR_I[2]}
  if((VaR_I[1]+VaR_I[3])<VaR.13)
  {sum_core_I=sum_core_I+VaR_I[1]+VaR_I[3]}
  if((VaR_I[2]+VaR_I[3])<VaR.23)
  {sum_core_I=sum_core_I+VaR_I[2]+VaR_I[3]}
  return(sum_core_I)#die Summe von kern-Allokation#
  
}

percent_I=sum_core_I/(sum_VaR_I*3+VaR_n)#Prozent von kern-Allokation#

##########Shapley Method##########
VaR_1S=1/3*VaR.[1]+1/6*(VaR.12-VaR.[2]+VaR.13-VaR.[3])+1/3*(VaR_n-VaR.23)
VaR_2S=1/3*VaR.[2]+1/6*(VaR.12-VaR.[1]+VaR.23-VaR.[3])+1/3*(VaR_n-VaR.13)
VaR_1S=1/3*VaR.[3]+1/6*(VaR.13-VaR.[1]+VaR.23-VaR.[2])+1/3*(VaR_n-VaR.12)

sum_VaR_S=VaR_1S+VaR_2S+VaR_3S
distance_S=abs((sum_VaR_S-sum_VaR_s)/n)#Distanz zwischen den Summe von VaR und Risikokapital# 

sum_core_a=numeric()
for(i in 1:n)
{
  sum_core_S=0
  if(VaR.[1]>VaR_1S)
  {sum_core_S=sum_core_S+VaR_1S}
  if(VaR.[2]>VaR_2S)
  {sum_core_S=sum_core_S+VaR_2S}
  if(VaR.[3]>VaR_3S)
  {sum_core_S=sum_core_S+VaR_3S}
  if((VaR_S1+VaR_S2)<VaR.12)
  {sum_core_S=sum_core_S+VaR_S1+VaR_S2}
  if((VaR_S1+VaR_S3)<VaR.13)
  {sum_core_S=sum_core_S+VaR_S1+VaR_S3}
  if((VaR_S2+VaR_S3)<VaR.23)
  {sum_core_S=sum_core_S+VaR_S2+VaR_aS3}
  return(sum_core_S)#die Summe von kern-Allokation#
  
}

percent_S=sum_core_S/(sum_VaR_a*3+VaR_n)#Prozent von kern-Allokation#




