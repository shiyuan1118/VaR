#读取数据#
OFDI=read.table("~/Desktop/preprocessing/OFDI.csv",header = F)
EPI=read.table("~/Desktop/preprocessing/EPI.csv",header = F)
GDP=read.table("~/Desktop/preprocessing/GDP.csv",header = F)
GDPg=read.table("~/Desktop/preprocessing/GDPg.csv",header = F)
RT=read.table("~/Desktop/preprocessing/RT.csv",header = F)
fuel=read.table("~/Desktop/preprocessing/fuel.csv",header = F)
mineral=read.table("~/Desktop/preprocessing/mineral.csv",header = F)
WAGE=read.table("~/Desktop/preprocessing/WAGE.csv",header = F)
LFP=read.table("~/Desktop/preprocessing/LFP.csv",header = F)

#数据预处理#
as.vector(OFDI,mode = "any")
as.vector(EPI,mode = "any")
as.vector(GDP,mode = "any")
as.vector(GDPg,mode = "any")
as.vector(RT,mode = "any")
as.vector(fuel,mode = "any")
as.vector(mineral,mode = "any")
as.vector(WAGE,mode = "any")
as.vector(LFP,mode = "any")

OFDI=as.numeric(unlist(OFDI))
EPI=as.numeric(unlist(EPI))
GDP=as.numeric(unlist(GDP))
GDPg=as.numeric(unlist(GDPg))
RT=as.numeric(unlist(RT))
fuel=as.numeric(unlist(fuel))
mineral=as.numeric(unlist(mineral))
WAGE=as.numeric(unlist(WAGE))
LFP=as.numeric(unlist(LFP))

mydata_1=data.frame(OFDI,EPI,GDP,GDPg,RT,fuel,mineral,WAGE,LFP)
OFDI=log(OFDI+sqrt(OFDI^2+1))

#检验二变量关系#
cor(mydata)
library(car)
scatterplotMatrix(mydata,lty.smooth=2,spread)
                    
#正态性检验#
library(car)
qqPlot(fit,id.methoe='identify',simulate=TRUE,labels=row.names(mydata),main='Q-Q plot')

#d-w检验
durbinWatsonTest(fit)

#多元线性回归假设检验#
library(gvlma)
gvmodel<-gvlma(fit)
summary(gvmodel)

#逐步回归#
tstep<-step(fit)

