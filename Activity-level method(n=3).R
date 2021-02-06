############Activity-level method########

##Berechung der Summe von entsprechenden Risikokapital##
VaR=numeric(length(3))
sum_VaR=0
for (i in 1:3)
{
 sum_VaR=sum_VaR+sqrt(Q[i,i])
}
##Berechnung der Risikokapital der Teil i##
VaR_1a=(VaR.1*VaR.123)/sum_VaR
VaR_2a=(VaR.2*VaR.123)/sum_VaR
VaR_3a=(VaR.3*VaR.123)/sum_VaR
VaR_12a=VaR_1a+VaR_2a
VaR_13a=VaR_1a+VaR_3a
VaR_23a=VaR_2a+VaR_3a
VaR_123=VaR.123


                          