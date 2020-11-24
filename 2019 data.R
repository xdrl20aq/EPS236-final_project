setwd("C:/Users/yueyi/Documents/FinalProject/air quality/2019")

denver.2019=read.csv("2019.co.denver.csv")
denver.2018=read.csv("2018.co.denver.csv")

denver.o3.index = denver.2019$parameter == "o3"
denver.no2.index = denver.2019$parameter == "no2"
denver.pm25.index = denver.2019$parameter == "pm25"
denver.pm10.index = denver.2019$parameter == "pm10"
denver.o3.index.2018 = denver.2018$parameter == "o3"
denver.no2.index.2018 = denver.2018$parameter == "no2"
denver.pm25.index.2018 = denver.2018$parameter == "pm25"
denver.pm10.index.2018 = denver.2018$parameter == "pm10"

denver.o3.dailymax.2019=tapply(denver.2019[denver.o3.index,"value"],as.Date(denver.2019[denver.o3.index,"utc"]),max)
denver.o3.dailymax.2018=tapply(denver.2018[denver.o3.index.2018,"value"],as.Date(denver.2018[denver.o3.index.2018,"utc"]),max)
plot(unique(as.Date(denver.2019[denver.o3.index,"utc"]))[4:75],denver.o3.dailymax.2019[4:75],type="l",ylim=c(0.02,0.11))
par(new=T)
plot(unique(as.Date(denver.2018[denver.o3.index.2018,"utc"]))[4:68],denver.o3.dailymax.2018[4:68],type="l",col="blue",ylim=c(0.02,0.11))
par(new=T)
plot(unique(as.Date(denver.2020[o3.index,"utc"]))[1:65],o3.dailymax[1:65],type="l",col="red",ylim=c(0.02,0.11))
