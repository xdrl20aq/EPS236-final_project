setwd("C:/Users/yueyi/Documents/FinalProject/air quality/greenline.alltimedata.august")

library(lubridate)

countylist=c("ca.placer", "nv.carson","nv.churchill","id.bannock",
             "wy.sublette","wy.sweetwater","wy.albany","co.denvera")
for (i in 1:8){
  assign(paste(countylist[i],".o3",sep=""),read.csv(paste("2020.",countylist[i],".o3.csv",sep="")))
  assign(paste(countylist[i],".o3",sep=""),cbind(get(paste(countylist[i],".o3",sep="")),
                                                          strptime(get(paste(countylist[i],".o3",sep=""))$utc, "%Y-%m-%dT%H:%M:%SZ")))
  assign(paste(countylist[i],".o3.hourlymean",sep=""),
         tapply(get(paste(countylist[i],".o3",sep=""))$value,get(paste(countylist[i],".o3",sep=""))[,12],mean))
      }


Sys.setlocale("LC_TIME", "English")
#
#ca.placer.o3.hourlymean=tapply(ca.placer.o3$value,ca.placer.o3[,12],mean)
plot(unique(ca.placer.o3[,12]),ca.placer.o3.hourlymean,type="l" )  #non-evenly spacing
plot(ca.placer.o3)

library(RColorBrewer)
color=brewer.pal(8,"RdBu")
plot(co.denvera.o3.new$Date,co.denvera.o3.new$o3,type="l" ) # evenly spacing
#plot(unique(co.denvera.o3[,12]),co.denvera.o3.hourlymean,type="l")
#plot(unique(ca.placer.o3[,12]),ca.placer.o3.hourlymean,type="l",ylim=c(0,0.1))
#plot(unique(co.denvera.o3[,12]),co.denvera.o3.hourlymean,type="l",ylim=c(0,0.1))

plot(unique(as.Date(co.denvera.o3[,4])),co.denvera.o3.dailymax,type="l" )
# plot with daily max
ca.placer.o3.dailymax=tapply(ca.placer.o3$value,as.Date(ca.placer.o3[,4]),max)
plot(unique(as.Date(ca.placer.o3[,4])),ca.placer.o3.dailymax,type="l" ,ylim=c(0.035,0.14),
     xlab = "Date",ylab = "Daily Maximum Ozone Concentration")
maxdate.max=NULL
for (i in 1:8){  
  assign(paste(countylist[i],".o3.dailymax",sep=""),
         tapply(get(paste(countylist[i],".o3",sep=""))$value,as.Date(get(paste(countylist[i],".o3",sep=""))[,4]),max))
    points(unique(as.Date(get(paste(countylist[i],".o3",sep=""))[,4])),get(paste(countylist[i],".o3.dailymax",sep="")),col=color[i],type="l",lwd=2)
    maxdate.max[i]= unique(as.Date(get(paste(countylist[i],".o3",sep=""))[,4]))[which.max(get(paste(countylist[i],".o3.dailymax",sep="")))]
}

print(as.Date(maxdate.max,origin= '1970-01-01'))
abline(h=0.1,lwd=2,col="chartreuse3",lty=2)
legend("topleft",legend=c("CA Placer","NV Carson","NV Churchill","ID Bannock", 
                          "WY Sublette", "WY Sweetwater","WY Albany","CO Denver-Aurora") ,
       col=color[1:8],lty=rep(1,8),lwd=2,cex=0.7)
legend("topright",legend="1-hour Limit", lwd=2,cex=0.8,lty=2,col="chartreuse3")

plot(unique(as.Date(ca.placer.o3[,4])),ca.placer.o3.dailymax,type="l" ,ylim=c(0.03,0.14))
abline(h=mean(quantile(ca.placer.o3.dailymax,probs=c(0.25,0.975))))




#plot with daily mean
ca.placer.o3.dailymean=tapply(ca.placer.o3$value,as.Date(ca.placer.o3[,4]),mean)
plot(unique(as.Date(ca.placer.o3[,4])),ca.placer.o3.dailymean,type="l" ,ylim=c(0.025,0.08))
maxdate.mean=NULL
for (i in 1:8){  
  assign(paste(countylist[i],".o3.dailymean",sep=""),
         tapply(get(paste(countylist[i],".o3",sep=""))$value,as.Date(get(paste(countylist[i],".o3",sep=""))[,4]),mean))
  points(unique(as.Date(get(paste(countylist[i],".o3",sep=""))[,4])),get(paste(countylist[i],".o3.dailymean",sep="")),col=color[i],type="l",lwd=2)
  maxdate.mean[i]= unique(as.Date(get(paste(countylist[i],".o3",sep=""))[,4]))[which.max(get(paste(countylist[i],".o3.dailymax",sep="")))]
  }
print(as.Date(maxdate.mean,origin= '1970-01-01'))



# create envenly-spaced data
mxx=list(NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL)
for (i in 1:8){ 
  my.ymd=as.Date(get(paste(countylist[i],".o3",sep=""))[,4])
  N=as.numeric(diff(range(my.ymd)))
  dd=my.ymd[1]+(0:N)
  hh=ymd_h(paste(rep(dd,each=24), 0:23,sep="-")) # standard ymdh
  YYY=year(hh)  ;  MMM = month(hh)  ; DDD=day(hh)
  assign(paste(countylist[i],".o3.new",sep=""),
         data.frame("Date"=hh,"yr.dec"=decimal_date(dd),YYY,MMM,DDD,"o3" = rep(NA,length(hh)) ))
  
  mxx[[i]]=match(unique(as.POSIXct(as.vector(unlist(get(paste(countylist[i],".o3",sep=""))[,12])),origin= '1970-01-01 00:00.00 UTC')),as.POSIXct(hh))

  }

countylist=c("ca.placer", "nv.carson","nv.churchill","id.bannock",
             "wy.sublette","wy.sweetwater","wy.albany","co.denvera")
ca.placer.o3.new[mxx[[1]],"o3"]=ca.placer.o3.hourlymean
nv.carson.o3.new[mxx[[2]],"o3"]=nv.carson.o3.hourlymean
nv.churchill.o3.new[mxx[[3]],"o3"]=nv.churchill.o3.hourlymean
id.bannock.o3.new[mxx[[4]],"o3"]=id.bannock.o3.hourlymean
wy.sublette.o3.new[mxx[[5]],"o3"]=wy.sublette.o3.hourlymean
wy.sweetwater.o3.new[mxx[[6]],"o3"]=wy.sweetwater.o3.hourlymean
wy.albany.o3.new[mxx[[7]],"o3"]=wy.albany.o3.hourlymean
co.denvera.o3.new[mxx[[8]],"o3"]=co.denvera.o3.hourlymean

# -------------------------------------------

# plot
plot(ca.placer.o3.new$Date,ca.placer.o3.new$o3,type="l" ) 
abline(h=mean(quantile(ca.placer.o3.hourlymean,probs=c(0.025,0.975))))

plot(nv.carson.o3.new$Date,nv.carson.o3.new$o3,type="l" )
plot(nv.churchill.o3.new$Date,nv.churchill.o3.new$o3,type="l" )
plot(id.bannock.o3.new$Date,id.bannock.o3.new$o3,type="l" )
plot(wy.sublette.o3.new$Date,wy.sublette.o3.new$o3,type="l" )
plot(wy.sweetwater.o3.new$Date,wy.sweetwater.o3.new$o3,type="l" )


# write a function to find baseline
hist(ca.placer.o3.new$o3)
hist(nv.carson.o3.new$o3)



find_baseline = function(data=ca.placer,){
  
}
# compare peak heights in different places
# download ozone data from 2018,2019, use tapply
# find dates with abnormal values, build model with pm2.5 (regression!)
# with pm2.5 in the places which is at the beginning of the trajectory
# 11.21 try pm2.5!!!

#####

my.ymd=as.Date(wy.sublette.o3$utc)
N=as.numeric(diff(range(my.ymd)))
dd=my.ymd[1]+(0:N)
hh=ymd_h(paste(rep(dd,each=24), 0:23,sep="-")) # standard ymdh
mxx=match(unique(as.POSIXct(as.vector(unlist(wy.sublette.o3[,12])),origin= '1970-01-01 00:00.00 UTC')),as.POSIXct(hh))

YYY=year(hh)  ;  MMM = month(hh)  ; DDD=day(hh)
wy.sublette.o3.new=data.frame("Date"=hh,"yr.dec"=decimal_date(dd),YYY,MMM,DDD,"o3" = rep(NA,length(hh)) )
wy.sublette.o3.new[mxx,"o3"]=wy.sublette.o3.hourlymean # standard dataframe


time=unique(as.vector(unlist(wy.sublette.o3[,12])))
loess.sublette=loess(wy.sublette.o3.hourlymean[45:550] ~ time[45:50], span=.2) #original
plot(wy.sublette.o3.new$Date,wy.sublette.o3.new$o3 ,type="l") # evenly spacing
lines(loess.sublette$x[45:550],loess.sublette$fitted[45:550],col=2,lwd=3) 

#find baseline 











library(pspline)
plot(wy.sublette.o3.new$Date,wy.sublette.o3.new$o3,type="l" )
my.psp=sm.spline(as.POSIXlt(as.vector(unlist(wy.sublette.o3[,12])),origin= '1970-01-01 00:00.00 UTC',tz="UTC"),wy.sublette.o3$value)  # uses Leave One Out Cross Validation to select the knots for the spline, and the penalty "spar"
points(my.psp$x,my.psp$ysmth,col="green",cex=.4,type="l")
match.psp=match(as.POSIXct(my.psp$x,tz="UTC"),wy.sublette.o3.new$Date)
wy.sublette.o3.new[match.psp,"o3.filled"]=wy.sublette.o3.new[match.psp,"o3"]
wy.sublette.o3.new[match.psp,"o3.filled"]=my.psp$ysmth
lines(wy.sublette.o3.new$Date,wy.sublette.o3.new$o3.filled,type="l",col=2)

library(FTICRMS)
bs=baseline(spect=wy.sublette.o3$value,zero.rm=TRUE,halve.search=TRUE,init.bd=0.04)
library(baseline)





