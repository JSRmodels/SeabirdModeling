### Data Files Needed

e <-read.csv("E:\\BirdlifeCPTMtg\\combineddataset.csv")# Has all the bird bycatch data
f<-read.csv("E:\\BirdlifeCPTMtg\\totRFMO5x5effort.csv")# HAs all the effort data on 5*5 res

####Libraries u need to load 
library(maptools)
library(mapplots)
# read shape file
library(shapefiles)
library(maps)


####This section of the code computes the seabird bycatch rate data across all observations after 2011
par(mfrow=c(2,1))
dec<-2015 ### need to change to year you want the data for 2012, 2013, 2014 or 2015
breeding<-1 ### need to change depending if breeding (1) or not breeding season (0)

tmp<-e
tmp[tmp$hook_set>300,]
tmp[tmp$yy>2011,]
tmp<-tmp[tmp$breeding==breeding,]
latBounds = c(-47.5, -20.5) 
lonBounds = c(-100, 40)
plot(0, xlim = lonBounds, ylim = latBounds, cex.main = 1, main="",
     xlab = "", ylab = "", asp = 1, xaxs = "i", yaxs = "i", yaxt="n", xaxt="n")
axis(1, at=c(-40,-15,-10,-5,0,5,10,20,40), labels=F)
axis(1, at=c(-40,-15,-10,-5,0,5,10,20,40), labels=c( "40W", "15W","10W","5W","0", "5E", "10E", "20E","40E"), cex.axis=1)

####Parsing out seabird bpue data   
ylim=c(-47.5,-20.5)
xlim=c(-52.5,107.5)
ypl = c(-47,-20);xpl = c(-52,108)
byx = byy =5 # Set grid size (here 0.5*degree)

Par = list(mfrow=c(2,1),mai=c(0.3,0.2,.2,0.2),omi = c(0.3,0.25,0.2,0) + 0.1,mgp=c(3,0.7,0), tck = -0.02,cex=0.9,las=0)
par(Par)
breaks.z =c(0,2.5,5,10,25,50,100,250,500,1000)/1000000
x = tmp$lond # dE$set_start_longitude
y = tmp$latd #set_start_latitude
z = tmp$BIRD_NUM/tmp$hook_set #dM$Nhooks/1000/5

grd <- make.grid(x,y,z, byx, byy, xlim, ylim,fun=mean)
draw.grid(grd,breaks.z)
axis(2, at=c(-60, -50, -40, -30, -20,0), labels=F)
axis(2, at=c(-60, -50, -40, -30, -20,0), labels=c( "60S", "50S", "40S", "30S", "20S","0"), cex.axis=1)
map(database = "world", resolution = 0.1, add = T, 
    col = rgb(0,0,0), fill = T)
mtext(paste("",dec," BPUE", " breeding",breeding sep=''),side=3,line=-1.5,col="red",font=2)
legend.grid("bottomleft", breaks=breaks.z, type=2, inset=0.01, title="Bycatch/1000 hooks",bg="cornsilk1",cex=0.7)


#####This section of the code computes all the effort data by year

tmp1 <- f[f$year==dec,]
tmp1 <- tmp1[tmp1$breeding==breeding,]


CByArea <- aggregate(tmp1$hooks, by=list(tmp1$lat5,tmp1$lon5), FUN=sum, na.rm=T)
totalE <- sum(CByArea$x)
points(x=CByArea$Group.2,y=CByArea$Group.1,cex=CByArea$x/2500000,pch=19, col="#0000EE69")

ylim=c(-47.5,-20.5)
xlim=c(-52.5,107.5)
ypl = c(-47,-20);xpl = c(-52,108)
byx = byy =5 # Set grid size (here 0.5*degree)

breaks.z =c(0,2.5,5,10,25,50,100,250,500,1000)*1000
x = tmp1$lon5 # dE$set_start_longitude
y = tmp1$lat5 #set_start_latitude
z = tmp1$hooks #dM$Nhooks/1000/5  

grd2 <- make.grid(x,y,z, byx, byy, xlim, ylim,fun=sum)


#####Figure 2. This computes the figures as well as the seperate components of the observed pooled data*effort as each year data is too sparse
####BPUE*Effort

latBounds = c(-47.5, -20.5) 
lonBounds = c(-100, 40)
plot(0, xlim = lonBounds, ylim = latBounds, cex.main = 1, main="",
     xlab = "", ylab = "", asp = 1, xaxs = "i", yaxs = "i", yaxt="n", xaxt="n")

grd3<-grd*grd2
breaks.z =c(0,2.5,5,10,25,50,100,250,750,2000)
draw.grid(grd3,breaks.z)
axis(1, at=c(-40,-15,-10,-5,0,5,10,20,40), labels=F)
axis(1, at=c(-40,-15,-10,-5,0,5,10,20,40), labels=c( "40W", "15W","10W","5W","0", "5E", "10E", "20E","40E"), cex.axis=1)
axis(2, at=c(-60, -50, -40, -30, -20,0), labels=F)
axis(2, at=c(-60, -50, -40, -30, -20,0), labels=c( "60S", "50S", "40S", "30S", "20S","0"), cex.axis=1)
map(database = "world", resolution = 0.1, add = T, 
    col = rgb(0,0,0), fill = T)
mtext(paste("",dec," Birds Killed breeding = ",breeding, sep=''),side=3,line=-1.5,col="red",font=2)
legend.grid("bottomleft", breaks=breaks.z, type=2, inset=0.01, title="Predicted Bycatch",bg="cornsilk1",cex=0.7)

tmp <- tmp[tmp$yy==dec,]
#tmp<-tmp[tmp$Flag=="EUPS",]
latBounds = c(-47.5, -20.5) 
lonBounds = c(-100, 40)
plot(0, xlim = lonBounds, ylim = latBounds, cex.main = 1, main="",
     xlab = "", ylab = "", asp = 1, xaxs = "i", yaxs = "i", yaxt="n", xaxt="n")
axis(1, at=c(-40,-15,-10,-5,0,5,10,20,40), labels=F)
axis(1, at=c(-40,-15,-10,-5,0,5,10,20,40), labels=c( "40W", "15W","10W","5W","0", "5E", "10E", "20E","40E"), cex.axis=1)

#### This section Parsing out seabird bpue data for particular year and strata (breeding non breeding). It shows coverage in any given strata is poor. hence pooling by year and strata   
ylim=c(-47.5,-20.5)
xlim=c(-52.5,107.5)
ypl = c(-47,-20);xpl = c(-52,108)
byx = byy =5 # Set grid size (here 0.5*degree)

Par = list(mfrow=c(2,1),mai=c(0.3,0.2,.2,0.2),omi = c(0.3,0.25,0.2,0) + 0.1,mgp=c(3,0.7,0), tck = -0.02,cex=0.9,las=0)
par(Par)
breaks.z =c(0,2.5,5,10,25,50,100,250,500,1000)/10000000
x = tmp$lond # dE$set_start_longitude
y = tmp$latd #set_start_latitude
z = tmp$BIRD_NUM/tmp$hook_set #dM$Nhooks/1000/5

grd <- make.grid(x,y,z, byx, byy, xlim, ylim,fun=mean)
draw.grid(grd,breaks.z)
axis(2, at=c(-60, -50, -40, -30, -20,0), labels=F)
axis(2, at=c(-60, -50, -40, -30, -20,0), labels=c( "60S", "50S", "40S", "30S", "20S","0"), cex.axis=1)
map(database = "world", resolution = 0.1, add = T, 
    col = rgb(0,0,0), fill = T)
mtext(paste("",dec," BPUE",sep=''),side=3,line=-1.5,col="red",font=2)
tmp1 <- f[f$year==dec,]

CByArea <- aggregate(tmp1$hooks, by=list(tmp1$lat5,tmp1$lon5), FUN=sum, na.rm=T)
totalE <- sum(CByArea$x)
points(x=CByArea$Group.2,y=CByArea$Group.1,cex=CByArea$x/1000000,pch=19, col="#0000EE69")

####Finally to input to Excel
### 1. For the year and breeding season we can observe the values used

grd2  ### <-effort used
grd3  ### <-estimates of seabird bycatch

##### 2. Take these values into excel for the year and breeding strata. 
##### 3. Then check if there are effort obs in a cell/orn not (if statement in excel). If there is an there is no estimate, we apply the catch rates from one of teh fleets!

#####README for EXCEL

1. Take grd2 and grd3 data and paste in year and strata in worksheets called "yyyy_eff" for effort observed and strata (one for breeding, one for non breeding)
2. Copy same values in "yyyy_country(JPN/BZL/ZAF/KOR)". In addition processed BPUE/1000 hooks values from raw data in cells "Q1:U3".
3. Values for each strata are computed and seen in cells B69 and J69 respectively.
