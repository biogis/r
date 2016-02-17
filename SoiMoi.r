try(source('D:/erey/CloudStation/Script/R/Template.r'),silent=T)
try(source('/Volumes/Data/PhD/Script/R/Template.r'),silent=T)

try(setwd('/Volumes/Data/PhD/R/DATA.PhD'),silent=T)
try(setwd('D:/erey/CloudStation/R/DATA.PhD'),silent=T)


####################################################################################################
####################################################################################################
####################################################################################################
#Soil Moisture
####################################################################################################
####################################################################################################

# soil <- read.csv('Soil.csv',sep=';',header=T);names(soil)
# head(soil)
# 
# lause <- unique(soil$LandUse)
# depth <- unique(soil$Depth)
# 
# result <- read.csv(text='lulc,depth,sat,fc,wp,awc')
# 
# for(lu in lause){
#   for(dpth in depth){print(lu)
#     print(dpth)
#     index <- which(soil$LandUse==lu & soil$Depth==dpth)
#     sat.f <- max(soil[index,8],na.rm=T)
#     fc.f <- median(soil[index,7],na.rm=T)
#     wp.f <- min(soil[index,9],na.rm=T)
#     awc.f <- fc.f-wp.f
#     dtf.sfp <- data.frame(lu,dpth,sat.f,fc.f,wp.f,awc.f)
#     result <- rbind(result,dtf.sfp)}}
# print(result)
# write.csv(result,'D:/erey/Dropbox/Sat.FC.WP.csv')

soil <- read.csv('D:/erey/Dropbox/Sat.FC.WP.csv',sep=';',header=T)
print(soil)



listFile <- c( "ec1.PU.D.csv","ec2.Alp.D.csv","ec4.VG.VNG.D.csv","ec7.PL.ML.D.csv")

dtf.plml <- read.csv('ec7.PL.ML.D.csv',sep=',',header=T);names(dtf.plml)
dtf.pu <- read.csv('ec1.PU.D.csv',sep=',',header=T);names(dtf.pu)
dtf.wine <- read.csv('ec4.VG.VNG.D.csv',sep=',',header=T);names(dtf.wine)
dtf.alp <- read.csv('ec2.Alp.D.csv',sep=',',header=T);names(dtf.alp)


####
#Saturation Index
PhiSatALP<- max(dtf.alp$Alp_5TE_5cm,na.rm=T)*1+
  max(dtf.alp$Alp_EC5_15cm,na.rm=T)*1.25+
  max(dtf.alp$Alp_5TE_30cm,na.rm=T)*1.5+
  max(dtf.alp$Alp_EC5_45cm,na.rm=T)*2.25+
  max(dtf.alp$Alp_EC5_60cm,na.rm=T)*2.5

####
#SoiMoi Index
alp<- dtf.alp$Alp_5TE_5cm*1+dtf.alp$Alp_EC5_15cm*1.25+dtf.alp$Alp_5TE_30cm*1.5+dtf.alp$Alp_EC5_45cm*2.25+dtf.alp$Alp_EC5_60cm*2.5
SatIndexALP<- (alp-147.56)/130.42
AWC.ALP<- alp-147.56


####
#Saturation Index
PhiSatPU<- max(dtf.pu$PU_5TE_5cm,na.rm=T)*1+
  max(dtf.pu$PU_EC5_15cm,na.rm=T)*1.25+
  max(dtf.pu$PU_ECTM_30cm,na.rm=T)*1.5+
  max(dtf.pu$PU_EC5_45cm,na.rm=T)*2.25+
  max(dtf.pu$PU_EC5_60cm,na.rm=T)*2.5

####
#SoiMoi Index
pu<- dtf.pu$PU_5TE_5cm*1+dtf.pu$PU_EC5_15cm*1.25+dtf.pu$PU_ECTM_30cm*1.5+dtf.pu$PU_EC5_45cm*2.25+dtf.pu$PU_EC5_60cm*2.5
SatIndexPU<- (pu-10.68)/219.84
AWC.PU <- pu-10.68

####
#Saturation Index
PhiSatPL<- max(dtf.plml$PL_5TE_5cm,na.rm=T)*1+
  max(dtf.plml$PL_EC5_15cm,na.rm=T)*1.25+
  max(dtf.plml$PL_5TM_30cm,na.rm=T)*1.5+
  max(dtf.plml$PL_EC5_45cm,na.rm=T)*2.25+
  max(dtf.plml$PL_EC5_60cm,na.rm=T)*2.5

####
#SoiMoi Index
pl <- dtf.plml$PL_5TE_5cm*1+dtf.plml$PL_EC5_15cm*1.25+dtf.plml$PL_5TM_30cm*1.5+dtf.plml$PL_EC5_45cm*2.25+dtf.plml$PL_EC5_60cm*2.5
SatIndexPL<- (pl-22.18)/175.46
AWC.PL <- pl-22.18


####
#Saturation Index
PhiSatML<- max(dtf.plml$ML_5TE_5cm,na.rm=T)*1+
  max(dtf.plml$ML_EC5_15cm,na.rm=T)*1.25+
  max(dtf.plml$ML_5TE_30cm,na.rm=T)*1.5+
  max(dtf.plml$ML_EC5_45cm,na.rm=T)*2.25+
  max(dtf.plml$ML_EC5_60cm,na.rm=T)*2.5

####
#SoiMoi Index
ml<- dtf.plml$ML_5TE_5cm*1+dtf.plml$ML_EC5_15cm*1.25+dtf.plml$ML_5TE_30cm*1.5+dtf.plml$ML_EC5_45cm*2.25+dtf.plml$ML_EC5_60cm*2.5
SatIndexML <- (ml-35.00)/271.36
AWC.ML <- ml-35.00


####
#Saturation Index
PhiSatVG<- max(dtf.wine$VG_5TE_5cm,na.rm=T)*1+
  max(dtf.wine$VG_EC5_15cm,na.rm=T)*1.25+
  max(dtf.wine$VG_ECTM_30cm,na.rm=T)*2.25+
  max(dtf.wine$VG_EC5_60cm,na.rm=T)*3+
  max(dtf.wine$VG_EC5_90cm,na.rm=T)*2.5

####
#SoiMoi Index
vg<- dtf.wine$VG_5TE_5cm*1+dtf.wine$VG_EC5_15cm*1.25+dtf.wine$VG_ECTM_30cm*2.25+dtf.wine$VG_EC5_60cm*3+dtf.wine$VG_EC5_90cm*2.5
SatIndexVG <- (vg-35.95)/186.64
AWC.VG <- vg-39.95

####
#Saturation Index
PhiSatVNG<- max(dtf.wine$VNG_5TE_5cm,na.rm=T)*1+
  max(dtf.wine$VNG_EC5_15cm,na.rm=T)*1.25+
  max(dtf.wine$VNG_ECTM_30cm,na.rm=T)*2.25+
  max(dtf.wine$VNG_EC5_60cm,na.rm=T)*3+
  max(dtf.wine$VNG_EC5_90cm,na.rm=T)*2.5

####
#SoiMoi Index
vng<- dtf.wine$VNG_5TE_5cm*1+dtf.wine$VNG_EC5_15cm*1.25+dtf.wine$VNG_ECTM_30cm*2.25+dtf.wine$VNG_EC5_60cm*3+dtf.wine$VNG_EC5_90cm*2.5
SatIndexVNG <- (vng-62.03)/155.59
AWC.VNG <- vng-62.03

PhiSatML
PhiSatPL
PhiSatPU
PhiSatVG
PhiSatVNG

max(c(SatIndexALP,
      SatIndexPU,
      SatIndexML,
      SatIndexPL,
      SatIndexVG,
      SatIndexVNG),na.rm=T)

# SatIndexVNG<-dtf.wine$SoiMoivngMed/PhiSatVNG
dtf.wine$SatIndexVNG.F<-SatIndexVNG
dtf.wine$AWC.VNG <- AWC.VNG

# SatIndexVG<-dtf.wine$SoiMoivgMed/PhiSatVG
dtf.wine$SatIndexVG.F<-SatIndexVG
dtf.wine$AWC.VG <- AWC.VG

# SatIndexPL<-dtf.plml$SoiMoiplMed/PhiSatPL
dtf.plml$SatIndexPL.F<-SatIndexPL
dtf.plml$AWC.PL <- AWC.PL

# SatIndexML<-dtf.plml$SoiMoimlMed/PhiSatML
dtf.plml$SatIndexML.F<-SatIndexML
dtf.plml$AWC.ML <- AWC.ML

# SatIndexPU<-dtf.pu$SoiMoipuMed/PhiSatPU
dtf.pu$SatIndexPU.F<-SatIndexPU
dtf.pu$AWC.PU <- AWC.PU

# SatIndexALP<-dtf.alp$SoiMoialpMed/PhiSatALP
dtf.alp$SatIndexALP.F<-SatIndexALP
dtf.alp$AWC.ALP <- AWC.ALP


dt.awc <- data.frame(AWC.ALP,AWC.PU,AWC.ML,AWC.PL,AWC.VNG,AWC.VG)


if(any(file.exists('D:/'))){pdfname <- 'D:/erey/Dropbox/SoiMoi.pdf'}
if(any(file.exists('/users'))){pdfname <- '/Users/erey/Dropbox/SoiMoi.pdf'}

index <- which(dtf.alp$Y==2011)
dtf.alp11 <- dtf.alp[index,c(1:7,9,24,25,26)];names(dtf.alp11)
index <- which(dtf.wine$Y==2011)
dtf.wine11 <- dtf.wine[index,c(1:7,9,24,36,37,38,39,40)];names(dtf.wine11)
index <- which(dtf.plml$Y==2011)
dtf.plml11 <- dtf.plml[index,c(1:7,9,26,38,39,40,41,42)];names(dtf.plml11)
index <- which(dtf.pu$Y==2011)
dtf.pu11 <- dtf.pu[index,c(1:7,11,26,27,28)];names(dtf.pu11)

index <- which(dtf.alp$Y==2012)
dtf.alp12 <- dtf.alp[index,c(1:7,9,24,25,26)];names(dtf.alp12)
index <- which(dtf.wine$Y==2012)
dtf.wine12 <- dtf.wine[index,c(1:7,9,24,36,37,38,39,40)];names(dtf.wine12)
index <- which(dtf.plml$Y==2012)
dtf.plml12 <- dtf.plml[index,c(1:7,9,26,38,39,40,41,42)];names(dtf.plml12)
index <- which(dtf.pu$Y==2012)
dtf.pu12 <- dtf.pu[index,c(1:7,11,26,27,28)];names(dtf.pu12)

lbls.2011 <- unique(as.character(paste(dtf.plml11[,5],dtf.plml11[,6],sep='-')))
axesAT.2011 <- c()
axesLabel.2011 <- c()
for(lbl in lbls.2011){
  print(lbl)
  index <- which(lbl==as.character(paste(dtf.plml11[,5],dtf.plml11[,6],sep='-')))
  print(index[1])
  axesAT.2011 <- c(axesAT.2011,index[1])
  print(mean(index))
  axesLabel.2011 <- c(axesLabel.2011,as.integer(mean(index)))}
axesAT.2011
axesLabel.2011

lbls.2012 <- unique(as.character(paste(dtf.plml12[,5],dtf.plml12[,6],sep='-')))
axesAT.2012 <- c()
axesLabel.2012 <- c()
for(lbl in lbls.2012){
  print(lbl)
  index <- which(lbl==as.character(paste(dtf.plml12[,5],dtf.plml12[,6],sep='-')))
  print(index[1])
  axesAT.2012 <- c(axesAT.2012,index[1])
  print(mean(index))
  axesLabel.2012 <- c(axesLabel.2012,as.integer(mean(index)))}
axesAT.2012 <- c(axesAT.2012,365)
axesAT.2012
axesLabel.2012

ifelse(any(fonts()=='Akkurat'),ft <- 'Akkurat',ft <- 'Arial');print(ft)

pdf(pdfname,family=ft,paper='a4',width=8.5,height=11)
par(mfrow=c(4,2),mar=c(5,4,4,5))

###################
#Alpage
plot(dtf.alp11$ec2.P,cex=0.2,ylim=c(60,0),col='steelblue',type='h',ann=F,axes=F,bty='n',cex.lab=0.8,cex.axis=0.8)
axis(4,cex.axis=0.8,las=2)
mtext('2011', side=3, line=2,cex=1)


par(new=T)
plot(dtf.alp11$SatIndexALP.F, cex=0.2, ylim=c(0,1.6), col= 'grey5', type='l', pch=16,bty='n',cex.lab=0.8,cex.axis=0.8,axes=F,ann=F)
# mtext(expression(paste('Saturation Index: ', phi/phi, 'sat')), side=2, line=3,cex=0.6)
mtext(expression(paste('Soil moisture Index: ', '(',phi-phi,'WP)',' / ',phi,'AWC')), side=2, line=3,cex=0.6)
mtext('Upper area 2200 m asl - Alpine pasture', side=1, line=3,cex=0.8, at=-60, adj = 0)
axis(1,at=axesAT.2012,tick=T,labels=c('','','','','','','','','','','','',''),las=2,cex.axis=0.8)
axis(1,at=axesAT.2012+15,tick=F,labels=c('J','F','M','A','M','J','J','A','S','O','N','D',''),las=1,cex.axis=0.8)
axis(2,cex.axis=0.8,las=2)


plot(dtf.alp12$ec2.P,cex=0.2,ylim=c(60,0),col='steelblue',type='h',ann=F,axes=F,bty='n',cex.lab=0.8,cex.axis=0.8,main='2012')
axis(4,cex.axis=0.8,las=2)
mtext('Precipitation [mm]', side=4, line=3,cex=0.6)
mtext('2012', side=3, line=2,cex=1)


par(new=T)
plot(dtf.alp12$SatIndexALP.F, cex=0.2, ylim=c(0,1.6), col= 'grey5', type='l', pch=16,bty='n',cex.lab=0.8,cex.axis=0.8,axes=F,ann=F)
axis(1,at=axesAT.2012,tick=T,labels=c('','','','','','','','','','','','',''),las=2,cex.axis=0.8)
axis(1,at=axesAT.2012+15,tick=F,labels=c('J','F','M','A','M','J','J','A','S','O','N','D',''),las=1,cex.axis=0.8)
axis(2,cex.axis=0.8,las=2)


###################
#Pasture
plot(dtf.plml11$ec7.P,cex=0.2,ylim=c(60,0),col='steelblue',type='h',ann=F,axes=F,bty='n',cex.lab=0.8,cex.axis=0.8)
axis(4,cex.axis=0.8,las=2)



par(new=T)
plot(dtf.plml11$SatIndexPL.F, cex=0.2, ylim=c(0,1.6), col= 'limegreen', type='l', pch=16,bty='n',cex.lab=0.8,cex.axis=0.8,axes=F,ann=F)
points(dtf.plml11$SatIndexML.F, cex=0.2, col='forestgreen', type='l', pch=16)
mtext(expression(paste('Soil moisture Index: ', '(',phi-phi,'WP)',' / ',phi,'AWC')), side=2, line=3,cex=0.6)
mtext('Middle area 900 m asl - Grassland', side=1, line=3,cex=0.8, at=-60, adj = 0)
axis(1,at=axesAT.2012,tick=T,labels=c('','','','','','','','','','','','',''),las=2,cex.axis=0.8)
axis(1,at=axesAT.2012+15,tick=F,labels=c('J','F','M','A','M','J','J','A','S','O','N','D',''),las=1,cex.axis=0.8)
axis(2,cex.axis=0.8,las=2)
legend('bottomleft', horiz=F,bty='n', cex=0.8, c('irrigated', 'not irrigated'), lty=0, text.col=c('forestgreen','limegreen'))

plot(dtf.plml12$ec7.P,cex=0.2,ylim=c(60,0),col='steelblue',type='h',ann=F,axes=F,bty='n',cex.lab=0.8,cex.axis=0.8)
axis(4,cex.axis=0.8,las=2)
mtext('Precipitation [mm]', side=4, line=3,cex=0.6)

par(new=T)
plot(dtf.plml12$SatIndexPL.F, cex=0.2, ylim=c(0,1.6), col= 'limegreen', type='l', pch=16,bty='n',cex.lab=0.8,cex.axis=0.8,axes=F,ann=F)
points(dtf.plml12$SatIndexML.F, cex=0.2, col='forestgreen', type='l', pch=16)
axis(1,at=axesAT.2012,tick=T,labels=c('','','','','','','','','','','','',''),las=2,cex.axis=0.8)
axis(1,at=axesAT.2012+15,tick=F,labels=c('J','F','M','A','M','J','J','A','S','O','N','D',''),las=1,cex.axis=0.8)
axis(2,cex.axis=0.8,las=2)


###################
#Vineyards
plot(dtf.wine11$ec4.P,cex=0.2,ylim=c(60,0),col='steelblue',type='h',ann=F,axes=F,bty='n',cex.lab=0.8,cex.axis=0.8)
axis(4,cex.axis=0.8,las=2)

par(new=T)
plot(dtf.wine11$SatIndexVG.F, cex=0.2, ylim=c(0,1.6), col= 'red', type='l',lty=3, pch=16,bty='n',cex.lab=0.8,cex.axis=0.8,axes=F,ann=F)
points(dtf.wine11$SatIndexVNG.F, cex=0.5, col='darkred',lty=2, type='l', pch=16)
mtext(expression(paste('Soil moisture Index: ', '(',phi-phi,'WP)',' / ',phi,'AWC')), side=2, line=3,cex=0.6)
mtext('Lower area 700 m asl - Vineyards', side=1, line=3,cex=0.8, at=-60, adj = 0)
axis(1,at=axesAT.2012,tick=T,labels=c('','','','','','','','','','','','',''),las=2,cex.axis=0.8)
axis(1,at=axesAT.2012+15,tick=F,labels=c('J','F','M','A','M','J','J','A','S','O','N','D',''),las=1,cex.axis=0.8)
axis(2,cex.axis=0.8,las=2)
legend('bottomleft', horiz=F,bty='n', cex=0.8, c('no vegetation', 'with vegetation'), lty=0, text.col = c('darkred','red'))


plot(dtf.wine12$ec4.P,cex=0.2,ylim=c(60,0),col='steelblue',type='h',ann=F,axes=F,bty='n',cex.lab=0.8,cex.axis=0.8)
axis(4,cex.axis=0.8,las=2)
mtext('Precipitation [mm]', side=4, line=3,cex=0.6)

par(new=T)
plot(dtf.wine12$SatIndexVG.F, cex=0.2, ylim=c(0,1.6), col= 'red',lty=3, type='l', pch=16,bty='n',cex.lab=0.8,cex.axis=0.8,axes=F,ann=F)
points(dtf.wine12$SatIndexVNG.F, cex=0.2, col='darkred',lty=2, type='l', pch=16)
axis(1,at=axesAT.2012,tick=T,labels=c('','','','','','','','','','','','',''),las=2,cex.axis=0.8)
axis(1,at=axesAT.2012+15,tick=F,labels=c('J','F','M','A','M','J','J','A','S','O','N','D',''),las=1,cex.axis=0.8)
axis(2,cex.axis=0.8,las=2)


###################
#3 systems

plot(dtf.wine11$SatIndexVNG.F, cex=0.2, ylim=c(0,1.6), col= 'darkred', type='l',lty=2, pch=16,bty='n',cex.lab=0.8,cex.axis=0.8,axes=F,ann=F)
mtext(expression(paste('Soil moisture Index: ', '(',phi-phi,'WP)',' / ',phi,'AWC')), side=2, line=3,cex=0.6)
axis(2,cex.axis=0.8,las=2)
points(dtf.plml11$SatIndexML.F, cex=0.2, col='forestgreen', type='l', pch=16,lty=1)
points(dtf.plml11$SatIndexPL.F, cex=0.2, col='limegreen', type='l', pch=16,lty=1)
points(dtf.alp11$SatIndexALP.F, cex=0.2, col='grey5', type='l', pch=16,lty=1)
axis(1,at=axesAT.2012,tick=T,labels=c('','','','','','','','','','','','',''),las=2,cex.axis=0.8)
axis(1,at=axesAT.2012+15,tick=F,labels=c('J','F','M','A','M','J','J','A','S','O','N','D',''),las=1,cex.axis=0.8)
legend('bottomleft', horiz=F,bty='n', cex=0.8, c('Alpine pasture', 'Grassland - irrigated','Grassland -  not irrigated','Vineyards - no vegetation'), lty=0, text.col=c('grey5','forestgreen','limegreen','darkred'))



plot(dtf.wine12$SatIndexVNG.F, cex=0.2, ylim=c(0,1.6), col= 'darkred', type='l',lty=2, pch=16,bty='n',cex.lab=0.8,cex.axis=0.8,axes=F,ann=F)
axis(2,cex.axis=0.8,las=2)
points(dtf.plml12$SatIndexML.F, cex=0.2, col='forestgreen', type='l', pch=16,lty=1)
points(dtf.plml12$SatIndexPL.F, cex=0.2, col='limegreen', type='l', pch=16,lty=1)
points(dtf.alp12$SatIndexALP.F, cex=0.2, col='grey5', type='l', pch=16,lty=1)
axis(1,at=axesAT.2012,tick=T,labels=c('','','','','','','','','','','','',''),las=2,cex.axis=0.8)
axis(1,at=axesAT.2012+15,tick=F,labels=c('J','F','M','A','M','J','J','A','S','O','N','D',''),las=1,cex.axis=0.8)

# dev.off()
# embed_fonts(pdfname,outfile=pdfname)







####################################################################################################
####################################################################################################
####################################################################################################
#ET
####################################################################################################
####################################################################################################


dtf.et <- read.csv('ML.PL.PU.VG.VNG.ALP.D.csv',sep=',',header=T);names(dtf.et)
# dtf.et <- read.csv('ML.PL.PU.VG.VNG.ALP.D.csv',sep=';',header=T);names(dtf.et)
# write.csv(dtf.et,'ML.PL.PU.VG.VNG.ALP.D.csv',row.names=F)
# 

colList <- unique(grep('ETp|ETa',colnames(dtf.et),value=T));colList
colList <- c(colnames(dtf.et)[1:8],colList);colList

index <- which(dtf.et$Y==2011)
dtf.et11 <- dtf.et[index,colList];names(dtf.et11)
#dtf.et11 <- dtf.et[index,];names(dtf.et11)
index <- which(dtf.et$Y==2012)
dtf.et12 <- dtf.et[index,colList];names(dtf.et12)
#dtf.et12 <- dtf.et[index,];names(dtf.et12)
head(dtf.et11)
head(dtf.et12)




ETa.alp.11 <- aggregate(dtf.et11$ETaALP~dtf.et11$M,data=dtf.et11,FUN="sum")
ETp.ec2.11 <- aggregate(dtf.et11$ec2.ETp~dtf.et11$M,data=dtf.et11,FUN="sum")

ETa.pl.11 <- aggregate(dtf.et11$ETaPL~dtf.et11$M,data=dtf.et11,FUN="sum")
ETa.ml.11 <- aggregate(dtf.et11$ETaML~dtf.et11$M,data=dtf.et11,FUN="sum")
ETp.ec7.11 <- aggregate(dtf.et11$ec7.ETp~dtf.et11$M,data=dtf.et11,FUN="sum")

ETa.vg.11 <- aggregate(dtf.et11$ETaVG~dtf.et11$M,data=dtf.et11,FUN="sum")
ETa.vng.11 <- aggregate(dtf.et11$ETaVNG~dtf.et11$M,data=dtf.et11,FUN="sum")
ETp.ec4.11 <- aggregate(dtf.et11$ec4.ETp~dtf.et11$M,data=dtf.et11,FUN="sum")

ETa.alp.12 <- aggregate(dtf.et12$ETaALP~dtf.et12$M,data=dtf.et12,FUN="sum")
ETp.ec2.12 <- aggregate(dtf.et12$ec2.ETp~dtf.et12$M,data=dtf.et12,FUN="sum")

ETa.pl.12 <- aggregate(dtf.et12$ETaPL~dtf.et12$M,data=dtf.et12,FUN="sum")
ETa.ml.12 <- aggregate(dtf.et12$ETaML~dtf.et12$M,data=dtf.et12,FUN="sum")
ETp.ec7.12 <- aggregate(dtf.et12$ec7.ETp~dtf.et12$M,data=dtf.et12,FUN="sum")

ETa.vg.12 <- aggregate(dtf.et12$ETaVG~dtf.et12$M,data=dtf.et12,FUN="sum")
ETa.vng.12 <- aggregate(dtf.et12$ETaVNG~dtf.et12$M,data=dtf.et12,FUN="sum")
ETp.ec4.12 <- aggregate(dtf.et12$ec4.ETp~dtf.et12$M,data=dtf.et12,FUN="sum")

dt.et <- data.frame(max(ETa.alp.11[2]),max(ETp.ec2.11[2]),
                 max(ETa.alp.12[2]),max(ETp.ec2.12[2]),
max(ETa.pl.11[2]),max(ETa.ml.11[2]),max(ETp.ec7.11[2]),
max(ETa.pl.12[2]),max(ETa.ml.12[2]),max(ETp.ec7.12[2]),
max(ETa.vg.11[2]),max(ETa.vng.11[2]),max(ETp.ec4.11[2]),
max(ETa.vg.12[2]),max(ETa.vng.12[2]),max(ETp.ec4.12[2]))


ylm <- floor(max(dt.et,dt.awc,na.rm=T)/10+1)*10

dET.pl.11 <- ETa.pl.11-ETp.ec7.11
dET.pl.11[1] <- c(1:12)
dET.ml.11 <- ETa.ml.11-ETp.ec7.11
dET.ml.11[1] <- c(1:12)
dET.pl.12 <- ETa.pl.12-ETp.ec7.12
dET.pl.12[1] <- c(1:12)
dET.ml.12 <- ETa.ml.12-ETp.ec7.12
dET.ml.12[1] <- c(1:12)



# pdf(pdfname,family=ft,paper='a4',width=8.5,height=11)
# par(mfrow=c(4,2),mar=c(5,4,4,5))


###################
#Alpage

plot(ETp.ec2.11,col='steelblue',type='h',lwd=3,ann=F,axes=F,xlim=c(0,13),ylim=c(-5,ylm))
axis(4,cex.axis=0.8,las=2,at=seq(0,ylm+10,50),labels=seq(0,ylm+10,50))


# par(new=T)
# plot(ETa.alp.11,col='grey5',type='l',lwd=2,ann=F,axes=F,ylim=c(-5,ylm))
points(ETa.alp.11,col='grey5',type='l',lwd=2)
mtext('2011', side=3, line=2,cex=1)
mtext('Evapotranspiration [mm]', side=2, line=3,cex=0.6)
# mtext('Actual evapotranspiration [mm]', side=2, line=3,cex=0.6)
axis(2,cex.axis=0.8,las=2,at=seq(0,ylm+10,50),labels=seq(0,ylm+10,50))
axis(1,at=seq(0.5,12.5,1),tick=T,labels=c('','','','','','','','','','','','',''),las=2,cex.axis=0.8)
axis(1,at=seq(1,12,1),tick=F,labels=c('J','F','M','A','M','J','J','A','S','O','N','D'),las=1,cex.axis=0.8)
mtext('Upper area 2200 m asl - Alpine pasture', side=1, line=3,cex=0.8, at=-1, adj = 0)


plot(ETp.ec2.12,col='steelblue',type='h',lwd=3,ann=F,axes=F,xlim=c(0,13),ylim=c(-5,ylm))
axis(4,cex.axis=0.8,las=2,at=seq(0,ylm+10,50),labels=seq(0,ylm+10,50))
# mtext('potential evapotranspiration [mm]', side=4, line=3,cex=0.6)


par(new=T)
plot(ETa.alp.12,col='grey5',type='l',lwd=2,ann=F,axes=F,ylim=c(-5,ylm))
mtext('2012', side=3, line=2,cex=1)
axis(2,cex.axis=0.8,las=2,at=seq(0,ylm+10,50),labels=seq(0,ylm+10,50))
axis(1,at=seq(0.5,12.5,1),tick=T,labels=c('','','','','','','','','','','','',''),las=2,cex.axis=0.8)
axis(1,at=seq(1,12,1),tick=F,labels=c('J','F','M','A','M','J','J','A','S','O','N','D'),las=1,cex.axis=0.8)



###################
#Pasture

plot(ETp.ec7.11,col='steelblue',type='h',lwd=3,ann=F,axes=F,xlim=c(0,13),ylim=c(-5,ylm))
axis(4,cex.axis=0.8,las=2,at=seq(0,ylm+10,50),labels=seq(0,ylm+10,50))


par(new=T)
plot(ETa.pl.11,col='limegreen',type='l',lwd=2,ann=F,axes=F,ylim=c(-5,ylm))
points(ETa.ml.11,col='forestgreen',type='l',lwd=2)
mtext('Evapotranspiration [mm]', side=2, line=3,cex=0.6)
# mtext('Actual evapotranspiration [mm]', side=2, line=3,cex=0.6)
axis(2,cex.axis=0.8,las=2,at=seq(0,ylm+10,50),labels=seq(0,ylm+10,50))
axis(1,at=seq(0.5,12.5,1),tick=T,labels=c('','','','','','','','','','','','',''),las=2,cex.axis=0.8)
axis(1,at=seq(1,12,1),tick=F,labels=c('J','F','M','A','M','J','J','A','S','O','N','D'),las=1,cex.axis=0.8)
legend('topleft', horiz=F,bty='n', cex=0.8, c('irrigated', 'not irrigated'), lty=0, text.col=c('forestgreen','limegreen'))
mtext('Middle area 900 m asl - Grassland', side=1, line=3,cex=0.8, at=-1, adj = 0)


plot(ETp.ec7.12,col='steelblue',type='h',lwd=3,ann=F,axes=F,xlim=c(0,13),ylim=c(-5,ylm))
axis(4,cex.axis=0.8,las=2,at=seq(0,ylm+10,50),labels=seq(0,ylm+10,50))
# mtext('potential evapotranspiration [mm]', side=4, line=3,cex=0.6)

par(new=T)
plot(ETa.pl.12,col='limegreen',type='l',lwd=2,ann=F,axes=F,ylim=c(-5,ylm))
points(ETa.ml.12,col='forestgreen',type='l',lwd=2)
axis(2,cex.axis=0.8,las=2,at=seq(0,ylm+10,50),labels=seq(0,ylm+10,50))
axis(1,at=seq(0.5,12.5,1),tick=T,labels=c('','','','','','','','','','','','',''),las=2,cex.axis=0.8)
axis(1,at=seq(1,12,1),tick=F,labels=c('J','F','M','A','M','J','J','A','S','O','N','D'),las=1,cex.axis=0.8)




###################
#Vineyards

plot(ETp.ec4.11,col='steelblue',type='h',lwd=3,ann=F,axes=F,xlim=c(0,13),ylim=c(-5,ylm))
axis(4,cex.axis=0.8,las=2,at=seq(0,ylm+10,50),labels=seq(0,ylm+10,50))

par(new=T)
plot(ETa.vg.11,col='red',type='l',lwd=2,ann=F,axes=F,ylim=c(-5,ylm))
points(ETa.vng.11,col='darkred',type='l',lwd=2)
mtext('Evapotranspiration [mm]', side=2, line=3,cex=0.6)
# mtext('Actual evapotranspiration [mm]', side=2, line=3,cex=0.6)
axis(2,cex.axis=0.8,las=2,at=seq(0,ylm+10,50),labels=seq(0,ylm+10,50))
axis(1,at=seq(0.5,12.5,1),tick=T,labels=c('','','','','','','','','','','','',''),las=2,cex.axis=0.8)
axis(1,at=seq(1,12,1),tick=F,labels=c('J','F','M','A','M','J','J','A','S','O','N','D'),las=1,cex.axis=0.8)
legend('topleft', horiz=F,bty='n', cex=0.8, c('no vegetation', 'with vegetation'), lty=0, text.col = c('darkred','red'))
mtext('Lower area 700 m asl - Vineyards', side=1, line=3,cex=0.8, at=-1, adj = 0)


plot(ETp.ec4.12,col='steelblue',type='h',lwd=3,ann=F,axes=F,xlim=c(0,13),ylim=c(-5,ylm))
axis(4,cex.axis=0.8,las=2,at=seq(0,ylm+10,50),labels=seq(0,ylm+10,50))
# mtext('potential evapotranspiration [mm]', side=4, line=3,cex=0.6)


par(new=T)
plot(ETa.vg.12,col='red',type='l',lwd=2,ann=F,axes=F,ylim=c(-5,ylm))
points(ETa.vng.12,col='darkred',type='l',lwd=2)
axis(2,cex.axis=0.8,las=2,at=seq(0,ylm+10,50),labels=seq(0,ylm+10,50))
axis(1,at=seq(0.5,12.5,1),tick=T,labels=c('','','','','','','','','','','','',''),las=2,cex.axis=0.8)
axis(1,at=seq(1,12,1),tick=F,labels=c('J','F','M','A','M','J','J','A','S','O','N','D'),las=1,cex.axis=0.8)


###################
#3 systems

plot(ETa.alp.11,col='grey5',type='l',lwd=2,ann=F,axes=F,xlim=c(0,13),ylim=c(-5,ylm))
points(ETa.ml.11,col='forestgreen',type='l',lwd=2)
points(ETa.pl.11,col='limegreen',type='l',lwd=2)
points(ETa.vng.11,col='darkred',type='l',lwd=2)
mtext('Evapotranspiration [mm]', side=2, line=3,cex=0.6)
# mtext('Actual evapotranspiration [mm]', side=2, line=3,cex=0.6)
axis(2,cex.axis=0.8,las=2,at=seq(0,ylm+10,50),labels=seq(0,ylm+10,50))
axis(1,at=seq(0.5,12.5,1),tick=T,labels=c('','','','','','','','','','','','',''),las=2,cex.axis=0.8)
axis(1,at=seq(1,12,1),tick=F,labels=c('J','F','M','A','M','J','J','A','S','O','N','D'),las=1,cex.axis=0.8)
legend('topleft', horiz=F,bty='n', cex=0.8, c('Alpine pasture', 'Grassland - irrigated','Grassland - not irrigated','Vineyards - no vegetation'), lty=0, text.col=c('grey5','forestgreen','limegreen','darkred'))


plot(ETa.alp.12,col='grey5',type='l',lwd=2,ann=F,axes=F,xlim=c(0,13),ylim=c(-5,ylm))
points(ETa.ml.12,col='forestgreen',type='l',lwd=2)
points(ETa.pl.12,col='limegreen',type='l',lwd=2)
points(ETa.vng.12,col='darkred',type='l',lwd=2)
axis(2,cex.axis=0.8,las=2,at=seq(0,ylm+10,50),labels=seq(0,ylm+10,50))
axis(1,at=seq(0.5,12.5,1),tick=T,labels=c('','','','','','','','','','','','',''),las=2,cex.axis=0.8)
axis(1,at=seq(1,12,1),tick=F,labels=c('J','F','M','A','M','J','J','A','S','O','N','D'),las=1,cex.axis=0.8)
# 
# dev.off()
# embed_fonts(pdfname,outfile=pdfname)
# 
# 
# 
# if(any(file.exists('D:/'))){pdfname <- 'D:/erey/Dropbox/SoiMoiET.pdf'}
# if(any(file.exists('/users'))){pdfname <- '/Users/erey/Dropbox/SoiMoiET.pdf'}
# 
# 
# 
# ifelse(any(fonts()=='Akkurat'),ft <- 'Akkurat',ft <- 'Arial');print(ft)
# 
# pdf(pdfname,family=ft,paper='a4',width=8.5,height=11)
# par(mfrow=c(4,2),mar=c(5,4,4,5))

###################
#Alpage
#2011
#Soil Moisture
plot(dtf.alp11$ec2.P,cex=0.2,ylim=c(60,0),col='steelblue',type='h',ann=F,axes=F,bty='n',cex.lab=0.8,cex.axis=0.8)
axis(4,cex.axis=0.8,las=2)
mtext('Precipitation [mm]', side=4, line=3,cex=0.6)


par(new=T)
plot(dtf.alp11$AWC.ALP, cex=0.2, ylim=c(-5,ylm), col= 'grey5', type='l', pch=16,bty='n',cex.lab=0.8,cex.axis=0.8,axes=F,ann=F)
mtext('Available Water Content [mm]', side=2, line=3,cex=0.6)
axis(1,at=axesAT.2012,tick=T,labels=c('','','','','','','','','','','','',''),las=2,cex.axis=0.8)
axis(1,at=axesAT.2012+15,tick=F,labels=c('J','F','M','A','M','J','J','A','S','O','N','D',''),las=1,cex.axis=0.8)
axis(2,cex.axis=0.8,las=2,at=seq(0,ylm+10,50),labels=seq(0,ylm+10,50))
mtext('2011 - Soil moisture', side=3, line=2,cex=1)
mtext('Upper area 2200 m asl - Alpine pasture', side=1, line=3,cex=0.8, at=-60, adj = 0)



#####
#ETa/ETp
plot(ETp.ec2.11,col='steelblue',type='h',lwd=3,ann=F,axes=F,xlim=c(0,13),ylim=c(-5,ylm))
axis(4,cex.axis=0.8,las=2,at=seq(0,ylm+10,50),labels=seq(0,ylm+10,50))
# mtext('potential evapotranspiration [mm]', side=4, line=3,cex=0.6)


# par(new=T)
# plot(ETa.alp.11,col='grey5',type='l',lwd=2,ann=F,axes=F,ylim=c(-5,ylm))
points(ETa.alp.11,col='grey5',type='l',lwd=2)
mtext('2011 - Evapotranspiration', side=3, line=2,cex=1)
mtext('Evapotranspiration [mm]', side=2, line=3,cex=0.6)
# mtext('Actual evapotranspiration [mm]', side=2, line=3,cex=0.6)
axis(2,cex.axis=0.8,las=2,at=seq(0,ylm+10,50),labels=seq(0,ylm+10,50))
axis(1,at=seq(0.5,12.5,1),tick=T,labels=c('','','','','','','','','','','','',''),las=2,cex.axis=0.8)
axis(1,at=seq(1,12,1),tick=F,labels=c('J','F','M','A','M','J','J','A','S','O','N','D'),las=1,cex.axis=0.8)




###################
#Pasture
#2011
#Soil Moisture
plot(dtf.plml11$ec7.P,cex=0.2,ylim=c(60,0),col='steelblue',type='h',ann=F,axes=F,bty='n',cex.lab=0.8,cex.axis=0.8)
axis(4,cex.axis=0.8,las=2)
mtext('Precipitation [mm]', side=4, line=3,cex=0.6)


par(new=T)
plot(dtf.plml11$AWC.PL, cex=0.2, ylim=c(-5,ylm), col= 'limegreen', type='l', pch=16,bty='n',cex.lab=0.8,cex.axis=0.8,axes=F,ann=F)
points(dtf.plml11$AWC.ML, cex=0.2, col='forestgreen', type='l', pch=16)
mtext('Available Water Content [mm]', side=2, line=3,cex=0.6)
mtext('Middle area 900 m asl - Grassland', side=1, line=3,cex=0.8, at=-60, adj = 0)
axis(1,at=axesAT.2012,tick=T,labels=c('','','','','','','','','','','','',''),las=2,cex.axis=0.8)
axis(1,at=axesAT.2012+15,tick=F,labels=c('J','F','M','A','M','J','J','A','S','O','N','D',''),las=1,cex.axis=0.8)
axis(2,cex.axis=0.8,las=2,at=seq(0,ylm+10,50),labels=seq(0,ylm+10,50))
legend('bottomleft', horiz=F,bty='n', cex=0.8, c('irrigated', 'not irrigated'), lty=0, text.col=c('forestgreen','limegreen'))


#####
#ETa/ETp
plot(ETp.ec7.11,col='steelblue',type='h',lwd=3,ann=F,axes=F,xlim=c(0,13),ylim=c(-5,ylm))
axis(4,cex.axis=0.8,las=2,at=seq(0,ylm+10,50),labels=seq(0,ylm+10,50))
# mtext('potential evapotranspiration [mm]', side=4, line=3,cex=0.6)



par(new=T)
plot(ETa.pl.11,col='limegreen',type='l',lwd=2,ann=F,axes=F,ylim=c(-5,ylm))
points(ETa.ml.11,col='forestgreen',type='l',lwd=2)
mtext('Evapotranspiration [mm]', side=2, line=3,cex=0.6)
# mtext('Actual evapotranspiration [mm]', side=2, line=3,cex=0.6)
axis(2,cex.axis=0.8,las=2,at=seq(0,ylm+10,50),labels=seq(0,ylm+10,50))
axis(1,at=seq(0.5,12.5,1),tick=T,labels=c('','','','','','','','','','','','',''),las=2,cex.axis=0.8)
axis(1,at=seq(1,12,1),tick=F,labels=c('J','F','M','A','M','J','J','A','S','O','N','D'),las=1,cex.axis=0.8)



###################
#Vineyards
#2011
#Soil Moisture
plot(dtf.wine11$ec4.P,cex=0.2,ylim=c(60,0),col='steelblue',type='h',ann=F,axes=F,bty='n',cex.lab=0.8,cex.axis=0.8)
axis(4,cex.axis=0.8,las=2)
mtext('Precipitation [mm]', side=4, line=3,cex=0.6)


par(new=T)
plot(dtf.wine11$AWC.VG, cex=0.2, ylim=c(-5,ylm), col= 'red',lty=3, type='l', pch=16,bty='n',cex.lab=0.8,cex.axis=0.8,axes=F,ann=F)
points(dtf.wine11$AWC.VNG, cex=0.2, col='darkred',lty=2, type='l', pch=16)
mtext('Available Water Content [mm]', side=2, line=3,cex=0.6)
mtext('Lower area 700 m asl - Vineyards', side=1, line=3,cex=0.8, at=-60, adj = 0)
axis(1,at=axesAT.2012,tick=T,labels=c('','','','','','','','','','','','',''),las=2,cex.axis=0.8)
axis(1,at=axesAT.2012+15,tick=F,labels=c('J','F','M','A','M','J','J','A','S','O','N','D',''),las=1,cex.axis=0.8)
axis(2,cex.axis=0.8,las=2,at=seq(0,ylm+10,50),labels=seq(0,ylm+10,50))
legend('bottomleft', horiz=F,bty='n', cex=0.8, c('no vegetation', 'with vegetation'), lty=0, text.col = c('darkred','red'))


#####
#ETa/ETp
plot(ETp.ec4.11,col='steelblue',type='h',lwd=3,ann=F,axes=F,xlim=c(0,13),ylim=c(-5,ylm))
axis(4,cex.axis=0.8,las=2,at=seq(0,ylm+10,50),labels=seq(0,ylm+10,50))
# mtext('potential evapotranspiration [mm]', side=4, line=3,cex=0.6)


par(new=T)
plot(ETa.vg.11,col='red',type='l',lwd=2,ann=F,axes=F,ylim=c(-5,ylm))
points(ETa.vng.11,col='darkred',type='l',lwd=2)
axis(2,cex.axis=0.8,las=2,at=seq(0,ylm+10,50),labels=seq(0,ylm+10,50))
axis(1,at=seq(0.5,12.5,1),tick=T,labels=c('','','','','','','','','','','','',''),las=2,cex.axis=0.8)
axis(1,at=seq(1,12,1),tick=F,labels=c('J','F','M','A','M','J','J','A','S','O','N','D'),las=1,cex.axis=0.8)
mtext('Evapotranspiration [mm]', side=2, line=3,cex=0.6)
# mtext('Actual evapotranspiration [mm]', side=2, line=3,cex=0.6)



###################
#3 systems
#2011
#Soil Moisture
plot(dtf.wine11$AWC.VNG, cex=0.2, ylim=c(-5,ylm), col= 'darkred', type='l',lty=2, pch=16,bty='n',cex.lab=0.8,cex.axis=0.8,axes=F,ann=F)
mtext('Available Water Content [mm]', side=2, line=3,cex=0.6)
points(dtf.plml11$AWC.ML, cex=0.2, col='forestgreen', type='l', pch=16,lty=1)
points(dtf.plml11$AWC.PL, cex=0.2, col='limegreen', type='l', pch=16,lty=1)
points(dtf.alp11$AWC.ALP, cex=0.2, col='grey5', type='l', pch=16,lty=1)
axis(1,at=axesAT.2012,tick=T,labels=c('','','','','','','','','','','','',''),las=2,cex.axis=0.8)
axis(1,at=axesAT.2012+15,tick=F,labels=c('J','F','M','A','M','J','J','A','S','O','N','D',''),las=1,cex.axis=0.8)
axis(2,cex.axis=0.8,las=2,at=seq(0,ylm+10,50),labels=seq(0,ylm+10,50))
legend('bottomleft', horiz=F,bty='n', cex=0.8, c('Alpine pasture', 'Grassland - irrigated','Grassland -  not irrigated','Vineyards - no vegetation'), lty=0, text.col=c('grey5','forestgreen','limegreen','darkred'))





#####
#ETa/ETp
plot(ETa.alp.11,col='grey5',type='l',lwd=2,ann=F,axes=F,xlim=c(0,13),ylim=c(-5,ylm))
points(ETa.ml.11,col='forestgreen',type='l',lwd=2)
points(ETa.pl.11,col='limegreen',type='l',lwd=2)
points(ETa.vng.11,col='darkred',type='l',lwd=2)
mtext('Evapotranspiration [mm]', side=2, line=3,cex=0.6)
# mtext('Actual evapotranspiration [mm]', side=2, line=3,cex=0.6)
axis(2,cex.axis=0.8,las=2,at=seq(0,ylm+10,50),labels=seq(0,ylm+10,50))
axis(1,at=seq(0.5,12.5,1),tick=T,labels=c('','','','','','','','','','','','',''),las=2,cex.axis=0.8)
axis(1,at=seq(1,12,1),tick=F,labels=c('J','F','M','A','M','J','J','A','S','O','N','D'),las=1,cex.axis=0.8)





###################
#Alpage
#2012
#Soil Moisture
plot(dtf.alp12$ec2.P,cex=0.2,ylim=c(60,0),col='steelblue',type='h',ann=F,axes=F,bty='n',cex.lab=0.8,cex.axis=0.8,main='2012')
axis(4,cex.axis=0.8,las=2)
mtext('Precipitation [mm]', side=4, line=3,cex=0.6)


par(new=T)
plot(dtf.alp12$AWC.ALP, cex=0.2, ylim=c(-5,ylm), col= 'grey5', type='l', pch=16,bty='n',cex.lab=0.8,cex.axis=0.8,axes=F,ann=F)
axis(1,at=axesAT.2012,tick=T,labels=c('','','','','','','','','','','','',''),las=2,cex.axis=0.8)
axis(1,at=axesAT.2012+15,tick=F,labels=c('J','F','M','A','M','J','J','A','S','O','N','D',''),las=1,cex.axis=0.8)
axis(2,cex.axis=0.8,las=2,at=seq(0,ylm+10,50),labels=seq(0,ylm+10,50))
mtext('Available Water Content [mm]', side=2, line=3,cex=0.6)
mtext('2012 - Soil moisture', side=3, line=2,cex=1)
mtext('Upper area 2200 m asl - Alpine pasture', side=1, line=3,cex=0.8, at=-1, adj = 0)



#####
#ETa/ETp
plot(ETp.ec2.12,col='steelblue',type='h',lwd=3,ann=F,axes=F,xlim=c(0,13),ylim=c(-5,ylm))
axis(4,cex.axis=0.8,las=2,at=seq(0,ylm+10,50),labels=seq(0,ylm+10,50))
# mtext('potential evapotranspiration [mm]', side=4, line=3,cex=0.6)


par(new=T)
plot(ETa.alp.12,col='grey5',type='l',lwd=2,ann=F,axes=F,ylim=c(-5,ylm))
axis(2,cex.axis=0.8,las=2,at=seq(0,ylm+10,50),labels=seq(0,ylm+10,50))
axis(1,at=seq(0.5,12.5,1),tick=T,labels=c('','','','','','','','','','','','',''),las=2,cex.axis=0.8)
axis(1,at=seq(1,12,1),tick=F,labels=c('J','F','M','A','M','J','J','A','S','O','N','D'),las=1,cex.axis=0.8)
mtext('Evapotranspiration [mm]', side=2, line=3,cex=0.6)
# mtext('Actual evapotranspiration [mm]', side=2, line=3,cex=0.6)
mtext('2012 - Evapotranspiration', side=3, line=2,cex=1)





###################
#Pasture
#2012
#Soil Moisture
plot(dtf.plml12$ec7.P,cex=0.2,ylim=c(60,0),col='steelblue',type='h',ann=F,axes=F,bty='n',cex.lab=0.8,cex.axis=0.8)
axis(4,cex.axis=0.8,las=2)
mtext('Precipitation [mm]', side=4, line=3,cex=0.6)

par(new=T)
plot(dtf.plml12$AWC.PL, cex=0.2, ylim=c(-5,ylm), col= 'limegreen', type='l', pch=16,bty='n',cex.lab=0.8,cex.axis=0.8,axes=F,ann=F)
points(dtf.plml12$AWC.ML, cex=0.2, col='forestgreen', type='l', pch=16)
axis(1,at=axesAT.2012,tick=T,labels=c('','','','','','','','','','','','',''),las=2,cex.axis=0.8)
axis(1,at=axesAT.2012+15,tick=F,labels=c('J','F','M','A','M','J','J','A','S','O','N','D',''),las=1,cex.axis=0.8)
axis(2,cex.axis=0.8,las=2,at=seq(0,ylm+10,50),labels=seq(0,ylm+10,50))
mtext('Available Water Content [mm]', side=2, line=3,cex=0.6)
mtext('Middle area 900 m asl - Grassland', side=1, line=3,cex=0.8, at=-60, adj = 0)
legend('bottomleft', horiz=F,bty='n', cex=0.8, c('irrigated', 'not irrigated'), lty=0, text.col=c('forestgreen','limegreen'))





#####
#ETa/ETp
plot(ETp.ec7.12,col='steelblue',type='h',lwd=3,ann=F,axes=F,xlim=c(0,13),ylim=c(-5,ylm))
axis(4,cex.axis=0.8,las=2,at=seq(0,ylm+10,50),labels=seq(0,ylm+10,50))
# mtext('potential evapotranspiration [mm]', side=4, line=3,cex=0.6)

par(new=T)
plot(ETa.pl.12,col='limegreen',type='l',lwd=2,ann=F,axes=F,ylim=c(-5,ylm))
points(ETa.ml.12,col='forestgreen',type='l',lwd=2)
axis(2,cex.axis=0.8,las=2,at=seq(0,ylm+10,50),labels=seq(0,ylm+10,50))
axis(1,at=seq(0.5,12.5,1),tick=T,labels=c('','','','','','','','','','','','',''),las=2,cex.axis=0.8)
axis(1,at=seq(1,12,1),tick=F,labels=c('J','F','M','A','M','J','J','A','S','O','N','D'),las=1,cex.axis=0.8)
mtext('Evapotranspiration [mm]', side=2, line=3,cex=0.6)
# mtext('Actual evapotranspiration [mm]', side=2, line=3,cex=0.6)









###################
#Vineyards
#2012
#Soil Moisture
plot(dtf.wine12$ec4.P,cex=0.2,ylim=c(60,0),col='steelblue',type='h',ann=F,axes=F,bty='n',cex.lab=0.8,cex.axis=0.8)
axis(4,cex.axis=0.8,las=2)
mtext('Precipitation [mm]', side=4, line=3,cex=0.6)

par(new=T)
plot(dtf.wine12$AWC.VG, cex=0.2, ylim=c(-5,ylm), col= 'red',lty=3, type='l', pch=16,bty='n',cex.lab=0.8,cex.axis=0.8,axes=F,ann=F)
points(dtf.wine12$AWC.VNG, cex=0.2, col='darkred',lty=2, type='l', pch=16)
axis(1,at=axesAT.2012,tick=T,labels=c('','','','','','','','','','','','',''),las=2,cex.axis=0.8)
axis(1,at=axesAT.2012+15,tick=F,labels=c('J','F','M','A','M','J','J','A','S','O','N','D',''),las=1,cex.axis=0.8)
axis(2,cex.axis=0.8,las=2,at=seq(0,ylm+10,50),labels=seq(0,ylm+10,50))
mtext('Available Water Content [mm]', side=2, line=3,cex=0.6)
mtext('Lower area 700 m asl - Vineyards', side=1, line=3,cex=0.8, at=-60, adj = 0)
legend('bottomleft', horiz=F,bty='n', cex=0.8, c('no vegetation', 'with vegetation'), lty=0, text.col = c('darkred','red'))





#####
#ETa/ETp
plot(ETp.ec4.12,col='steelblue',type='h',lwd=3,ann=F,axes=F,xlim=c(0,13),ylim=c(-5,ylm))
axis(4,cex.axis=0.8,las=2,at=seq(0,ylm+10,50),labels=seq(0,ylm+10,50))
# mtext('potential evapotranspiration [mm]', side=4, line=3,cex=0.6)

par(new=T)
plot(ETa.vg.12,col='red',type='l',lwd=2,ann=F,axes=F,ylim=c(-5,ylm))
points(ETa.vng.12,col='darkred',type='l',lwd=2)
axis(2,cex.axis=0.8,las=2,at=seq(0,ylm+10,50),labels=seq(0,ylm+10,50))
axis(1,at=seq(0.5,12.5,1),tick=T,labels=c('','','','','','','','','','','','',''),las=2,cex.axis=0.8)
axis(1,at=seq(1,12,1),tick=F,labels=c('J','F','M','A','M','J','J','A','S','O','N','D'),las=1,cex.axis=0.8)
mtext('Evapotranspiration [mm]', side=2, line=3,cex=0.6)
# mtext('Actual evapotranspiration [mm]', side=2, line=3,cex=0.6)





###################
#3 systems
#2012
#Soil Moisture
plot(dtf.wine12$AWC.VNG, cex=0.2, ylim=c(-5,ylm), col= 'darkred', type='l',lty=2, pch=16,bty='n',cex.lab=0.8,cex.axis=0.8,axes=F,ann=F)
points(dtf.plml12$AWC.ML, cex=0.2, col='forestgreen', type='l', pch=16,lty=1)
points(dtf.plml12$AWC.PL, cex=0.2, col='limegreen', type='l', pch=16,lty=1)
points(dtf.alp12$AWC.ALP, cex=0.2, col='grey5', type='l', pch=16,lty=1)
axis(1,at=axesAT.2012,tick=T,labels=c('','','','','','','','','','','','',''),las=2,cex.axis=0.8)
axis(1,at=axesAT.2012+15,tick=F,labels=c('J','F','M','A','M','J','J','A','S','O','N','D',''),las=1,cex.axis=0.8)
axis(2,cex.axis=0.8,las=2,at=seq(0,ylm+10,50),labels=seq(0,ylm+10,50))
mtext('Available Water Content [mm]', side=2, line=3,cex=0.6)
legend('bottomleft', horiz=F,bty='n', cex=0.8, c('Alpine pasture', 'Grassland - irrigated','Grassland -  not irrigated','Vineyards - no vegetation'), lty=0, text.col=c('grey5','forestgreen','limegreen','darkred'))




#####
#ETa/ETp
plot(ETa.alp.12,col='grey5',type='l',lwd=2,ann=F,axes=F,xlim=c(0,13),ylim=c(-5,ylm))
points(ETa.ml.12,col='forestgreen',type='l',lwd=2)
points(ETa.pl.12,col='limegreen',type='l',lwd=2)
points(ETa.vng.12,col='darkred',type='l',lwd=2)
axis(2,cex.axis=0.8,las=2,at=seq(0,ylm+10,50),labels=seq(0,ylm+10,50))
axis(1,at=seq(0.5,12.5,1),tick=T,labels=c('','','','','','','','','','','','',''),las=2,cex.axis=0.8)
axis(1,at=seq(1,12,1),tick=F,labels=c('J','F','M','A','M','J','J','A','S','O','N','D'),las=1,cex.axis=0.8)
mtext('Evapotranspiration [mm]', side=2, line=3,cex=0.6)
# mtext('Actual evapotranspiration [mm]', side=2, line=3,cex=0.6)





dev.off()
embed_fonts(pdfname,outfile=pdfname)




