######################################
##########Incremental Method##########

##Berechnung der inkrementellen VaR##
deta_VaR_1=VaR.123-VaR.23
deta_VaR_2=VaR.123-VaR.13
deta_VaR_3=VaR.123-VaR.12

##Berechnung der inkrementellen Risikokapital##
VaR_1I=((deta_VaR_1)*VaR.123)/(deta_VaR_1+deta_VaR_2+deta_VaR_3)
VaR_2I=((deta_VaR_2)*VaR.123)/(deta_VaR_1+deta_VaR_2+deta_VaR_3)
VaR_3I=((deta_VaR_3)*VaR.123)/(deta_VaR_1+deta_VaR_2+deta_VaR_3)
VaR_12I=VaR_1I+VaR_2I
VaR_13I=VaR_1a+VaR_3I
VaR_23I=VaR_2a+VaR_3I