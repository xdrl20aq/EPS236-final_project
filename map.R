# plot maps
library(ggplot2)
library(usmap)
library(ggthemes)
library(geo)

setwd("C:/Users/yueyi/Documents/FinalProject/air quality/map")

# hysplit: prepare for trajectory plotting
hysplit=read.csv("trj.24.6.16.csv")

for (i in 1:16){
  assign(paste("trj",i,sep=""),NULL)
  trj.index= which(hysplit[,1]==i)
  assign(paste("trj",i,sep=""),hysplit[trj.index,])
  assign(paste("trj",i,".ll",sep="") ,
         data.frame(lon=get(paste("trj",i,sep=""))[,9],lat=get(paste("trj",i,sep=""))[,8]))
  assign(paste("trans.trj",i,".ll",sep="") ,usmap_transform(get(paste("trj",i,".ll",sep=""))))
}


test_data <- data.frame(lon = -96.70, lat = 43.55)

transformed_data <- usmap_transform(test_data)


ca=read.csv("california.csv")
nv=read.csv("nevada.csv")
id=read.csv("idaho.csv")
wy=read.csv("wyoming.csv")
co=read.csv("colorado.csv")
ut=read.csv("utah.csv")



for (i in 4:9){
  date.index.ca=which(ca$Date==paste("08/",as.character(14+i),"/2020",sep=""))
  date.index.nv=which(nv$Date==paste("08/",as.character(14+i),"/2020",sep=""))
  date.index.id=which(id$Date==paste("08/",as.character(14+i),"/2020",sep=""))
  date.index.wy=which(wy$Date==paste("08/",as.character(14+i),"/2020",sep=""))
  date.index.co=which(co$Date==paste("08/",as.character(14+i),"/2020",sep=""))
  date.index.ut=which(ut$Date==paste("08/",as.character(14+i),"/2020",sep=""))
  
  
  fips.id=fips(state=unique(id$STATE[date.index.id]),county=unique(id$COUNTY[date.index.id]))
  fips.wy=fips(state=unique(wy$STATE[date.index.wy]),county=unique(wy$COUNTY[date.index.wy]))
  fips.ca=fips(state=unique(ca$STATE[date.index.ca]),county=unique(ca$COUNTY[date.index.ca]))
  fips.nv=fips(state=unique(nv$STATE[date.index.nv]),county=unique(nv$COUNTY[date.index.nv]))
  fips.co=fips(state=unique(co$STATE[date.index.co]),county=unique(co$COUNTY[date.index.co]))
  fips.ut=fips(state=unique(ut$STATE[date.index.ut]),county=unique(ut$COUNTY[date.index.ut]))
  my.fips=c(fips.id , fips.wy,fips.ca,fips.nv,fips.co,fips.ut)
  
  id.countymean=tapply(id$Daily.Max.8.hour.Ozone.Concentration[date.index.id],id$COUNTY[date.index.id],mean)
  wy.countymean=tapply(wy$Daily.Max.8.hour.Ozone.Concentration[date.index.wy],wy$COUNTY[date.index.wy],mean)
  ca.countymean=tapply(ca$Daily.Max.8.hour.Ozone.Concentration[date.index.ca],ca$COUNTY[date.index.ca],mean)
  nv.countymean=tapply(nv$Daily.Max.8.hour.Ozone.Concentration[date.index.nv],nv$COUNTY[date.index.nv],mean)
  co.countymean=tapply(co$Daily.Max.8.hour.Ozone.Concentration[date.index.co],co$COUNTY[date.index.co],mean)
  ut.countymean=tapply(ut$Daily.Max.8.hour.Ozone.Concentration[date.index.ut],ut$COUNTY[date.index.ut],mean)
  
  
  ozone=data.frame("fips"=my.fips, "value"=c(id.countymean,wy.countymean,ca.countymean,nv.countymean,co.countymean,ut.countymean))
  
  plot_usmap(include = c( "ID","WY","CA","NV","CO","UT"),"counties",
             data=ozone,values = "value") +
    scale_fill_continuous(limits=c(0.04,0.09),low = "green", high = "red",
                          name = "Ozone/ppm", label = scales::comma)+
    labs(title = "Ozone Concentration During August Wildfire", 
         subtitle = paste("08/",as.character(15+i),"/2020",sep=""),size=12) +
    theme(legend.position = "right")+
    geom_path(data=trans.trj3.ll, aes(lon.1,lat.1),col="white",lwd=.7)+
    geom_path(data=trans.trj6.ll, aes(lon.1,lat.1),col="white",lwd=.7)+
    geom_path(data=trans.trj10.ll, aes(lon.1,lat.1),col="white",lwd=.7)+
    geom_path(data=trans.trj12.ll, aes(lon.1,lat.1),col="white",lwd=.7)+

  ggsave(paste("trial", i, ".png", sep=""))
}


