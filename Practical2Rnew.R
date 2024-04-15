#Practical 2 (8/March/2023)

#install the packages of NETCDF Reader
#install.packages("RNetCDF")

#open the library of the installed package 
library(RNetCDF)
rm(list=ls())

#String for the Path
inputpath<-"C:/Users/DELL/Documents/MER/Semester 2/Satellite oceanography and meteology/Envisat/"
#Concatenate PATH+NAME OF THE FILE
#command : "paste"
inputfile<-paste(inputpath,"GW_L2P_ALT_ENVI_GDR_20120101_145505_20120101_154510_110_155.nc",sep="")

SATALT_ini<-open.nc(inputfile)
print.nc(SATALT_ini)

#READ THE CONTENT OF THE VARIABLES
#1st variable : time
time1<-var.get.nc(SATALT_ini,"time")
View(time1)

#transform this into a more familiar time reference:command 'utcal.nc'
time2<-utcal.nc("seconds since 1985-01-01 00:00:00.0",time1,type="n")
#alternative way
timeunits<-att.get.nc(SATALT_ini, "time", "units") #extract initial date from data

#2nd variable:latitude
latitude<-var.get.nc(SATALT_ini,"lat",na.mode=1)

#3rd variable: longitude
longitude<-var.get.nc(SATALT_ini,"lon",na.mode=1)

#4rd variable: swh
swh1<-var.get.nc(SATALT_ini,"swh",na.mode=1) #need to be transformed for swh with scale factor from print.nc
swh2<-swh1*0.001
swh2<-var.get.nc(SATALT_ini,"swh",na.mode=1,unpack=T) #alternative way to transform SWH

#5th variable: swh_quality (refer to the value of the quality)
swhqual<-var.get.nc(SATALT_ini,"swh_quality",na.mode=1)
swhqual # 0 high quality data , 2 very low quality data

#6th variable:sigma0 (backscattering coefficient)
sigma02<-var.get.nc(SATALT_ini,"sigma0",na.mode=1,unpack=T)

#7th variable:sigma0_quality
sigma0qual<-var.get.nc(SATALT_ini,"sigma0_quality",na.mode=1,unpack=T)


#command : "cbind" to merge

envisat_alt<-cbind(time2,latitude,longitude,swh2,swhqual,sigma02,sigma0qual)

envisat_alt[1:3,] #first three rows

#calculate the tz using sigma0 and add the new column
#ESA formula

tz<-(envisat_alt[,11]*(envisat_alt[,9]^2))**0.25
envisat_alt2<-cbind(envisat_alt,tz)
envisat_alt2[5:10,]

wef<-0.49*(envisat_alt2[,9]^2)*(envisat_alt2[,13])
envisat_alt3<-cbind(envisat_alt2,wef)
envisat_alt3[5:10,]

#Select high quality data = 0 col#10--0 AND col#12--0
#can use loop
#it is better for subsetting

envisat_alt4<-envisat_alt3[envisat_alt3[,10] == 0
                           & (envisat_alt3[,12] == 0),]
envisat_alt4[5:10,]
View(envisat_alt4)

#start to transform the coordinate.
for (xxx in (1:nrow(envisat_alt4))){
  #condition
  if (envisat_alt4[xxx,8]>180) 
    #what to do
  {
    envisat_alt4[xxx,8]<-envisat_alt3[xxx,8]-360
  }
  
}

library(sp)
library(maps)
library(mapdata)
library(shape)
library(reshape2)

#Divide the plotting area
x11()
layout(matrix(1:2,ncol=2),width=c(1,4))
#Create a color palette

coll<-colorRampPalette(c("blue","green","yellow","orange","red"))(100)

#create a scale for WEF in ID sea

min(envisat_alt4[,14],na.rm=T) #0.215
max(envisat_alt4[,14],na.rm=T) #145.16=150

#Regression line COLOR#=F(WEF) for Indonesian Sea
rescalecolor<-1+((100/150*envisat_alt4[,14])) #For the homework, we also have to change the linear expression


#Colorscale goes to the left part
colorlegend(zlim=c(0,150),
            zval = seq(0,150,25),
            col = coll[1:100],main = "WEF\n kW/m\n\n",
            main.cex = 0.8,
            posx=c(0.2,0.35),
            posy = c(0.05,0.9))

#RIGHT PART
#Background

# Get the map data within the specified limits Indonesia

map("worldHires", xlim = c(-20, 20), ylim = c(0,25),fill=T, col="grey")

box();axis(1);axis(2)
title (main="ENVISAT: Average WEF[kW/m] \n Cycle:90 Orbit :78",
       xlab="ºE", ylab="ºN")


points(envisat_alt4[,8],
       envisat_alt4[,7], 
       col=coll[rescalecolor],
       pch=16,
       cex=1.5
)

map("worldHires",regions = "Nigeria",  xlim = c(-20, 20), ylim = c(0,25),fill=T, col="burlywood",add=T)

box();axis(1);axis(2)
#title (main="Envisat: Average WEF[kW/m] \n 2010 05 02",
      # xlab="ºE", ylab="ºN")

