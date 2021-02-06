#########################################
#################n=3#####################

##Bestimmung von Kovarianzmatrix##
Q=matrix(c(57,22,6,22,42,-9,6,-9,28),ncol=3)
##Einheitsvektor zu erstellen##
e_123=matrix(c(1,1,1),nrow=1,ncol=3)
e_12=matrix(c(1,1,0),nrow=1,ncol=3)
e_23=matrix(c(0,1,1),nrow=1,ncol=3)
e_13=matrix(c(1,0,1),nrow=1,ncol=3)

###########Standard-alone risk##
VaR.1=sqrt(Q[1,1])
VaR.2=sqrt(Q[2,2])
VaR.3=sqrt(Q[3.3])
VaR.12=sqrt((e_12)%*%Q%*%t(e_12))
VaR.13=sqrt((e_13)%*%Q%*%t(e_13))
VaR.23=sqrt((e_23)%*%Q%*%t(e_23))
VaR.123=sqrt((e_123)%*%Q%*%t(e_123))