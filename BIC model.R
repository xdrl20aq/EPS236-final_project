setwd("C:/Users/yueyi/Documents/FinalProject/air quality/o3~pm model")
denver.2020=read.csv("2020.denver.8~10.csv")
sacramento.2020=read.csv("2020.sacramento.8~10.csv")
sweetwater.2020=read.csv("2020.sweetwater.8~10.csv")

denver.o3.index = denver.2020$parameter == "o3"
denver.no2.index = denver.2020$parameter == "no2"
denver.pm25.index = denver.2020$parameter == "pm25"
denver.pm10.index = denver.2020$parameter == "pm10"
sacramento.o3.index = sacramento.2020$parameter == "o3"
sacramento.no2.index = sacramento.2020$parameter == "no2"
sacramento.pm25.index = sacramento.2020$parameter == "pm25"
sacramento.pm10.index = sacramento.2020$parameter == "pm10"
sweetwater.o3.index = sweetwater.2020$parameter == "o3"
sweetwater.no2.index = sweetwater.2020$parameter == "no2"
sweetwater.pm10.index = sweetwater.2020$parameter == "pm10"

denver.o3.dailymax=tapply(denver.2020[denver.o3.index,"value"],as.Date(denver.2020[denver.o3.index,"utc"]),max)
plot(unique(as.Date(denver.2020[o3.index,"utc"])),o3.dailymax,type="l")

denver.no2.dailymax=tapply(denver.2020[denver.no2.index,"value"],as.Date(denver.2020[denver.no2.index,"utc"]),max)
denver.pm25.dailymax=tapply(denver.2020[denver.pm25.index,"value"],as.Date(denver.2020[denver.pm25.index,"utc"]),max)
denver.pm10.dailymax=tapply(denver.2020[denver.pm10.index,"value"],as.Date(denver.2020[denver.pm10.index,"utc"]),max)

sacramento.o3.dailymax=tapply(sacramento.2020[sacramento.o3.index,"value"],as.Date(sacramento.2020[sacramento.o3.index,"utc"]),max)
sacramento.no2.dailymax=tapply(sacramento.2020[sacramento.no2.index,"value"],as.Date(sacramento.2020[sacramento.no2.index,"utc"]),max)
sacramento.pm25.dailymax=tapply(sacramento.2020[sacramento.pm25.index,"value"],as.Date(sacramento.2020[sacramento.pm25.index,"utc"]),max)
sacramento.pm10.dailymax=tapply(sacramento.2020[sacramento.pm10.index,"value"],as.Date(sacramento.2020[sacramento.pm10.index,"utc"]),max)

sweetwater.o3.dailymax=tapply(sweetwater.2020[sweetwater.o3.index,"value"],as.Date(sweetwater.2020[sweetwater.o3.index,"utc"]),max)
sweetwater.no2.dailymax=tapply(sweetwater.2020[sweetwater.no2.index,"value"],as.Date(sweetwater.2020[sweetwater.no2.index,"utc"]),max)
sweetwater.pm10.dailymax=tapply(sweetwater.2020[sweetwater.pm10.index,"value"],as.Date(sweetwater.2020[sweetwater.pm10.index,"utc"]),max)


denver.max=data.frame("date"=unique(as.Date(denver.2020[denver.o3.index,"utc"])), 
                      "o3"=denver.o3.dailymax,"no2"=denver.no2.dailymax,"pm25"=denver.pm25.dailymax,"pm10"=denver.pm10.dailymax)
sacramento.max=data.frame("date"=unique(as.Date(sacramento.2020[sacramento.o3.index,"utc"])), 
                      "o3"=sacramento.o3.dailymax,"no2"=sacramento.no2.dailymax,"pm25"=sacramento.pm25.dailymax,"pm10"=sacramento.pm10.dailymax)
sweetwater.max=data.frame("date"=unique(as.Date(sweetwater.2020[sweetwater.o3.index,"utc"])), 
                          "o3"=sweetwater.o3.dailymax,"no2"=sweetwater.no2.dailymax,"pm10"=sweetwater.pm10.dailymax)


york_path= "C:/Users/yueyi/Documents/FinalProject"
source(paste(york_path,"York.R",sep="/"))


library(lmodel2)
require(lmodel2)
library(IsoplotR)
#zz1 = lm(o3~pm25,data=sweetwater.max)

zz2 = YorkFit(sweetwater.max$pm10[-37],sweetwater.max$o3[-37],rep(sd(sweetwater.max$pm10[-37]),88),rep(sd(sweetwater.max$o3[-37]),88)) #1st lowest
zz02 = lm(sweetwater.max$o3[-37]~sweetwater.max$pm10[-37])
zz3 = lmodel2(o3~no2,data=sweetwater.max) 
#zz4 = lm(o3~pm25 + pm10, data=sweetwater.max)
#zz5 = lm(o3~pm25:pm10, data=sweetwater.max)
#zz6 = lm(o3~pm25 + pm10 + no2, data=sweetwater.max)
#zz7 = lm(o3~pm25:pm10 + no2, data=sweetwater.max)
#zz8 = lm(o3~pm25:no2 + pm10, data=sweetwater.max)
#zz9 = lm(o3~pm10:no2 + pm25, data=sweetwater.max)
#zz10 = lm(o3~pm25:no2 + pm10, data=sweetwater.max)
#zz11 = lm(o3~pm25:no2 + pm10:no2 + pm25*pm10, data=sweetwater.max)
zz12 = lm(o3[-37]~pm10[-37]:no2[-37], data=sweetwater.max)
zz13 = lm(o3~pm10 + no2, data=sweetwater.max) #2nd lowest
qqnorm(zz2)

abline(a=zz2[1,1],b=zz2[2,1],col="blue",lty=2,lwd=4)
abline(coef = zz02$coefficients)

print(c(BIC(zz1),BIC(zz2),BIC(zz3),BIC(zz4),BIC(zz5),BIC(zz6),BIC(zz7),BIC(zz8),BIC(zz9),
        BIC(zz10),BIC(zz11),BIC(zz12),BIC(zz13)))

plot(zz12)  

# fit denver with sacramento
# does the value 
denver.max=denver.max[1:86,]
sacramento.max=sacramento.max[4:89,]


szz1 = lm(denver.max$o3~sacramento.max$pm25)
szz2 = lm(sweetwater.max$o3~pm10,data=sacramento.max)
szz3 = lm(sweetwater.max$o3~no2,data=sacramento.max)
szz4 = lm(denver.max$o3~pm25 + pm10, data=sacramento.max)
szz5 = lm(denver.max$o3~pm25:pm10, data=sacramento.max)
szz6 = lm(denver.max$o3~pm25 + pm10 + no2, data=sacramento.max) #newest 2nd lowest
szz7 = lm(denver.max$o3~pm25:pm10 + no2, data=sacramento.max) ## newest 1st lowest value, lower than fit with denver
szz8 = lm(denver.max$o3~pm25:no2 + pm10, data=sacramento.max)
szz9 = lm(denver.max$o3~pm10:no2 + pm25, data=sacramento.max)
szz10 = lm(denver.max$o3~pm25:no2 + pm10, data=sacramento.max)
szz11 = lm(denver.max$o3~pm25:no2 + pm10:no2 + pm25*pm10, data=sacramento.max)
szz12 = lm(sweetwater.max$o3~pm10:no2, data=sacramento.max)
szz13 = lm(sweetwater.max$o3~pm10 + no2, data=sacramento.max) # lower than fit with denver

print(c(BIC(szz1),BIC(szz2),BIC(szz3),BIC(szz4),BIC(szz5),BIC(szz6),BIC(szz7),BIC(szz8),BIC(szz9),
        BIC(szz10),BIC(szz11),BIC(szz12),BIC(szz13)))
# 4 day lag:
#  [1] 974.3811 974.8983 974.0292 978.6888 974.9244 981.9410 978.4340 978.9467 977.4833 978.9467
# [11] 983.5807 974.6570 978.3477

# 6 day lag:
#   [1] 949.8046 951.3479 952.7832 954.1979 950.0235 958.2198 954.0703 955.1233 954.2219 955.1233
# [11] 967.1901 952.0490 955.5145




plot(sweetwater.max[,c("pm10","o3")])
zz2 = YorkFit(sweetwater.max$pm10[-37],sweetwater.max$o3[-37],rep(sd(sweetwater.max$pm10[-37]),88),rep(sd(sweetwater.max$o3[-37]),88)) #1st lowest
abline(a=zz2[1,1],b=zz2[2,1],col="blue",lty=2,lwd=4)

plot(sweetwater.max[,])

#model optimization by LOOCV
library(DescTools)
pred = rep(0,length(x))
validation=rep(0,length(x))
LOOCV = function(x=sweetwater.max$pm10,y=sweetwater.max$o3){  # x: pm10 in different counties; y: o3 in target county
  for(i in 1:length(x)){
    model = YorkFit(x[-i],y[-i],rep(sd(x[-i]),length(x[-i])),rep(sd(y[-i]),length(y[-i]))) #1st lowest
    pred[i] = model[2,1]*x[i]+model[1,1]
    validation[i]=y[i]
     # score/error of ith fold
  }
  return(MAE(pred,validation)) # returns a vector
}

LOOCV(sweetwater.max$pm10, sweetwater.max$o3)
LOOCV(sweetwater.max$pm10[c(-14:-23,-62:-69)], sweetwater.max$o3[c(-14:-23,-62:-69)])

LOOCV(denver.max$pm10, denver.max$o3)
LOOCV(denver.max$pm10[c(-14:-23,-62:-69)], denver.max$o3[c(-14:-23,-62:-69)])

LOOCV(denver.max$pm10, denver.max$o3)
# 8.15-9.7 10.3-10.10
# If the "k" I left out is the days with smoke on the map, MAE should be higher 

plot(denver.max[,c("pm10","o3")])   
plot(sweetwater.max$o3[-37],type="l")
par(new=T)
plot(sweetwater.max$pm10[-37],type="l",col="red")

plot(denver.max$o3,type="l")
par(new=T)
plot(denver.max$pm10,type="l",col="red")
acf(sweetwater.max$pm10)
