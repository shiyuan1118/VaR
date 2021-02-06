#############Shapley Method##########

VaR_1S=1/3*VaR.1+1/6*(VaR.12-VaR.2+VaR.13-VaR.3)+1/3*(VaR.123-VaR.23)
VaR_2S=1/3*VaR.2+1/6*(VaR.12-VaR.1+VaR.23-VaR.3)+1/3*(VaR.123-VaR.13)
VaR_3S=1/3*VaR.3+1/6*(VaR.13-VaR.1+VaR.23-VaR.2)+1/3*(VaR.123-VaR.23)
VaR_12S=VaR_1S+VaR_2S
VaR_13S=VaR_1S+VaR_3S
VaR_23S=VaR_2S+VaR_3S