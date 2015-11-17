source('/Volumes/Data/PhD/Script/R/Template.r')

setwd('~/Desktop/CurrentJob/DataBase/')

dtf <- read.csv('FRIbat_db_20151110.csv',header=T,sep=',')
names(dtf)
head(dtf)
summary(dtf)

# dtf <- read.csv('FRIbat_db_20151110.csv',header=T,sep=';')
# names(dtf)
# write.csv(dtf,'FRIbat_db_20151110.csv',row.names=F)

morph <- read.csv('FRIbat_DB_Morpho_20151110.csv',header=T,sep=',')
names(morph)

fn <- file.path("/Volumes/Data/GIS/swissadmin/Fribourg.shp")
shp.fr <- readShapeSpatial(fn)
proj4string(shp.fr) <- CRS(proj)
shp.ext <-  shp.fr@bbox 
plot(shp.fr)
 

fn <- file.path('/Volumes/Data/GIS/Hydro/Waterway_OSM_Frib.shp')
river <- readShapeSpatial(fn)
proj4string(river) <- CRS(proj)
shp.ext <-  river@bbox 
plot(river)

fn <- file.path('/Volumes/Data/GIS/Hydro/Lakes_REN.shp')
water <- readShapeSpatial(fn)
proj4string(water) <- CRS(proj)
shp.ext <-  water@bbox 
plot(water,add=T)

fn.srtm <- file.path('/Volumes/Data/GIS/ElevationMap/DEMSRTM_lv03.tif')
dem <- raster(fn.srtm);dem
projection(dem) <- proj
dem

xmin <- 544000
xmax <- 596000
ymin <- 134000
ymax <- 208000

extent <- extent(xmin,xmax,ymin,ymax)
dem.crop <- crop(dem,extent)
plot(dem.crop,ext=extent,col=BrBG(255),alpha=0.8,legend=F)
plot(shp.fr,add=T)

dem.fr <- mask(dem.crop,shp.fr,inverse=F)
plot(dem,col=grey(0:255 / 255),ext=extent,alpha=0.6,xlab='Coord X', ylab='Coord Y',legend=F,main=sp)
plot(dem.fr,add=T,col=BrBG(255),alpha=0.6,legend=F)
plot(river,add=T,col='royalblue4')
plot(water,add=T,col='steelblue',border='royalblue4')
plot(shp.fr,add=T)


# datas.srtm <- extract(x=dem,y=xy);summary(datas.srtm)
# dtf <- cbind(dtf,datas.srtm)
# 
# dem <- setMinMax(dem)
# minValue(dem)
# maxValue(dem)
# rcl <- data.frame(from=seq(50,4750,by=100),
#                   to=seq(149,4849,by=100),
#                   becomes=seq(100,4800,by=100))
# dem.rcl100 <- reclassify(dem,as.matrix(rcl));dem.rcl100
# datas.rcl100 <- extract(x=dem.rcl100,y=xy);summary(datas.rcl100)
# 
# dtf <- cbind(dtf,datas.rcl100)
# 
# 
# summary(dtf)
# write.csv(dtf,'/Users/erey/Desktop/FRIbat_Alt_ok.csv')
# write.csv(dtf,'FRIbat_db_20151110.csv',row.names=F)

dim(dtf)
names(dtf)

sp <- 'Chiroptera'
batSp <- dtf
batmorph <- morph

listSp <- c('Pipistrellus','Nyctalus','Myotis','Plecotus','Eptesicus')
 
 
listSp <- c('Barbastella barbastellus',
            'Eptesicus nilssonii','Eptesicus serotinus',
            'Myotis alcathoe','Myotis bechsteinii','Myotis blythii','Myotis brandtii',
            'Myotis daubentonii','Myotis myotis','Myotis mystacinus','Myotis nattereri',
            'Nyctalus leisleri','Nyctalus noctula',
            'Pipistrellus kuhlii','Pipistrellus nathusii','Pipistrellus pipistrellus','Pipistrellus pygmaeus',
            'Plecotus auritus','Plecotus austriacus',
            'Rhinolophus hipposideros',
            'Vespertilio murinus')
            #,'Miniopterus schreibersii','Rhinolophus ferrumequinum');listSp

# listSp <- as.character(unique(dtf$SP));print(listSp)

for(sp in listSp){
# sp <- listSp[21];sp
# sp <- listSp[10];sp
print(sp)
genreName <- strsplit(sp,split=' ')[[1]][1]
spName <- strsplit(sp,split=' ')[[1]][2]
pdfName <- paste('/Volumes/Data/Dropbox/Brochure FRIbat/Cartes',paste(paste(strsplit(genreName,split='')[[1]][1],strsplit(genreName,split='')[[1]][2],strsplit(genreName,split='')[[1]][3],
              strsplit(spName,split='')[[1]][1],strsplit(spName,split='')[[1]][2],strsplit(spName,split='')[[1]][3],
              sep=''),'pdf',sep='.'),sep='/')

# pdfName <- paste('/Volumes/Data/Dropbox/Brochure FRIbat/Cartes',paste(sp,'pdf',sep='.'),sep='/')

print(pdfName)

batSp<-subset(dtf,dtf$SP==sp);dim(batSp)
# batSp<-subset(dtf,dtf$Genre==sp);dim(batSp)
names(batSp)
dim(batSp)

batmorph <- subset(morph,morph$SP==sp);dim(batmorph)
names(batmorph)
dim(batmorph)

quantile(batSp$datas.rcl100)
quantile(batmorph$AB,na.rm=T)
quantile(batmorph$P,na.rm=T)
print(summary(batSp$TYP_COL))
print(summary(batSp$obs))


dtf.fc<-subset(batSp,batSp$TYP_COL=='FC');dim(dtf.fc)
table(dtf.fc$cxcykm)



nData <- dim(batSp)[1]
oldData <- min(batSp$A,na.rm=T)
lastData <- max(batSp$A,na.rm=T)
nSQkm <- length(unique(batSp$cxcykm))
nCol <- length(table(dtf.fc$cxcykm))
minAlt <- min(batSp$datas.dem2,na.rm=T)
maxAlt <- max(batSp$datas.dem2,na.rm=T)
meanAlt <- floor(mean(batSp$datas.dem2,na.rm=T))
medianAlt <- floor(median(batSp$datas.dem2,na.rm=T))
ABmin <- min(batmorph$AB,na.rm=T)
ABmax <- max(batmorph$AB,na.rm=T)
ABmedian <- median(batmorph$AB,na.rm=T)
ABmean <- mean(batmorph$AB,na.rm=T)
Pmin <- min(batmorph$P,na.rm=T)
Pmax <- max(batmorph$P,na.rm=T)
Pmedian <- median(batmorph$P,na.rm=T)
Pmean <- mean(batmorph$P,na.rm=T)
Pce <- median(batmorph$Pce,na.rm=T)
GrffPce <- median(batmorph$GrffPce,na.rm=T)

# ABmin <- NA
# ABmax <- NA
# ABmedian <- NA
# ABmean <- NA
# Pmin <- NA
# Pmax <- NA
# Pmedian <- NA
# Pmean <- NA
# Pce <- NA
# GrffPce <- NA

dtf.resum <- t(data.frame('nbre données' = nData,
                        'ancienne donnée' = oldData,
                        'donnée récente' = lastData,
                        'nbre km2' = nSQkm,
                        'nbre colonies' = nCol,
                        'altitude min' = minAlt,
                        'altitude max' = maxAlt,
                        'altitude moyenne' = meanAlt,
                        'altitude mediane' = medianAlt,
                        'AB min' =  ABmin,
                        'AB max' =  ABmax,
                        'AB median' =  ABmedian,
                        'AB moyen' = ABmean,
                        'Poid min' = Pmin,
                        'Poid max' = Pmax,
                        'Poid median' = Pmedian,
                        'Poid moyen' = Pmean,
                        'Pouce' = Pce,
                        'Griffe du pouce' = GrffPce))
print(dtf.resum)


#Create Point shapefile for the given dtf
xy <- data.frame('cx' = batSp[,20],'cy' = batSp[,21])
shp <- SpatialPointsDataFrame(coords = xy, data = batSp);shp
proj4string(shp) <- CRS(proj);shp

plot(shp)
head(batSp);tail(batSp)
shp
  
pdf(pdfName,family='AKKURAT',paper='a4r',width=11,height=8.5)
  
plot(dem,col=grey(0:255 / 255),ext=extent,alpha=0.6,xlab='Coord X', ylab='Coord Y',legend=F,main=sp)
plot(dem.fr,add=T,col=BrBG(255),alpha=0.6,legend=F)
plot(river,add=T,col='royalblue4')
plot(water,add=T,col='steelblue',border='royalblue4')
plot(shp.fr,add=T)
plot(shp.fr,add=T)
plot(shp,add=T,pch=4, col='grey10')
  
  
  #######
  #Type Year
  #######
  
  A <- table(batSp$A)
#   A <- floor(A/sum(A)*100)
  plot(A,bty='n',axes=F,ann=F,lwd=2,type='h',col='tomato')
  
  mtext("Nbre de données", side=2, line=3,cex=1.2,col="grey30")
  box(col="grey")
  
axis(1,at=seq(floor(min(batSp$A,na.rm=T)/5)*5,2015,by=5),cex.axis=1,col.axis='grey30',col='grey30',las=2)
# axis for RhiFer and MyoBly: 
# axis(1,at=min(batSp$A,na.rm=T),cex.axis=1,col.axis='grey30',col='grey30',las=2)
axis(2,cex.axis=1,col.axis='grey30',col='grey30')
  # mtext(sp,side=3,line=3,col='grey10')
  mtext("Années", side=3, line=2,cex=1.2,col="grey30")
  mtext(paste('Données la plus ancienne:',oldData,'--',
              'Données la plus récente:',lastData,sep=' '),
        side=3, line=1,col='grey10')
  
  
  text(min(batSp$A,na.rm=T),max(A)-(max(A/3)), paste(capture.output(dtf.resum), collapse='\n'), pos=4,cex=1)
  mtext(sp,side=3,line=3,col='grey10')
  
  
  
  #######
  #Type Mois
  #######
  
  M <- table(batSp$M)
#   M <- floor(M/sum(M)*100)
  plot(M,bty='n',axes=F,ann=F,lwd=5,type='h',col='tomato')
  
  mtext("Nbre de données", side=2, line=3,cex=1.2,col="grey30")
  box(col="grey")
  
  axis(1,at=c(1:12),labels=c('jan','fev','mar','avr','mai','jun','jui','aou','sep','oct','nov','dec')
       ,cex.axis=1,col.axis='grey30',col='grey30')
  axis(2,cex.axis=1,col.axis='grey30',col='grey30')
  # mtext(sp,side=3,line=3,col='grey10')
  mtext("Mois", side=3, line=2,cex=1.2,col="grey30")
  # mtext(paste('Données la plus courante:',floor(median(batSp$M,na.rm=T)),sep=' '),
  #       side=3, line=1,col='grey10')
  
  
  #######
  #Type altitude
  #######
  
  alt <- table(batSp$datas.rcl100) 
#   alt <- floor(alt/sum(alt)*100)
  plot(alt,bty='n',axes=F,ann=F,lwd=5,type='h',col='tomato')
  
  mtext("Nbre de données", side=2, line=3,cex=1.2,col="grey30")
  box(col="grey")
  
  axis(1,at=seq(min(batSp$datas.rcl100,na.rm=T),max(batSp$datas.rcl100,na.rm=T),by=100),cex.axis=1,col.axis='grey30',col='grey30',las=2)
  axis(2,cex.axis=1,col.axis='grey30',col='grey30')
  
  # mtext(sp,side=3, line=3,col='grey10')
  mtext("Altitude", side=3, line=2,cex=1.2,col="grey30")
  mtext(paste('Altitude la plus basse:',minAlt,'--',
              'Altitude moyenne',meanAlt,'--',
              'Altitude médiane',meanAlt,'--',
              'Altitude la plus élevée:',maxAlt,sep=' '),
        side=3, line=1,col='grey10')
  
  
  #######
  #Type obs
  #######
  obs <- table(batSp$obs,useNA='always')
#   obs <- floor(obs/sum(obs)*100)
  plot(obs,bty='n',axes=F,ann=F,lwd=5,type='h',col='tomato')
  rownames <- as.factor(as.data.frame(obs)$Var1)
  
  mtext("Nbre d'observation", side=2, line=3,cex=1,col="grey30")
  box(col="grey")
  
  axis(1,at=c(1:length(rownames)),label=rownames,cex.axis=0.8,las=2,col.axis='grey30',col='grey30')
  axis(2,cex.axis=1,col.axis='grey30',col='grey30')
  # mtext(sp,side=3, line=3,col='grey10')
  mtext("Type d'observation", side=3, line=2,cex=1.2,col="grey30")
  
  # obs <- table(batSp$obs,useNA='no')
  # propobs <- floor(obs/sum(obs)*100)
  # pie(propobs, clockwise=T,col=jet.colors(length(typCOL)),label=names(obs),cex=0.6)
  # mtext("Type d'observations", side=3, line=1,cex=1.2,col="grey30")
  
  
  #######
  #Type colonies
  #######
  
  typCOL <- table(batSp$TYP_COL,useNA='always')
#   typCOL <- floor(typCOL/sum(typCOL)*100)
  plot(typCOL,bty='n',axes=F,ann=F,lwd=5,type='h',col='tomato')
  rownames <- as.factor(as.data.frame(typCOL)$Var1)
  
  mtext("Nbre de données", side=2, line=3,cex=1.2,col="grey30")
  box(col="grey")
  
  axis(1,at=c(1:length(rownames)),label=rownames,cex.axis=1,col.axis='grey30',col='grey30')
  axis(2,cex.axis=1,col.axis='grey30',col='grey30')
  # mtext(sp,side=3, line=3,col='grey10')
  mtext("Type de gîte", side=3, line=2,cex=1.2,col="grey30")
  
  # typCOL <- table(batSp$TYP_COL,useNA='no')
  # proptypCOL <- floor(typCOL/sum(typCOL)*100)
  # pie(proptypCOL, clockwise=T,col=jet.colors(length(typCOL)),label=names(typCOL),cex=0.6)
  # mtext("Type de gîte", side=3, line=1,cex=1.2,col="grey30")
  # 
  
  
  #######
  #Type Land Use
  #######
  
  lause <- table(batSp$LaUse,useNA='always')
#   lause <- floor(lause/sum(lause)*100)
  plot(lause,bty='n',axes=F,ann=F,lwd=5,type='h',col='tomato')
  rownames <- as.factor(as.data.frame(lause)$Var1)
  
  mtext("Nbre de données", side=2, line=3,cex=1.2,col="grey30")
  box(col="grey")
  
  axis(1,at=c(1:length(rownames)),label=rownames,cex.axis=0.8,las=2,col.axis='grey30',col='grey30')
  axis(2,cex.axis=1,col.axis='grey30',col='grey30')
  # mtext(sp,side=3, line=3,col='grey10')
  mtext("Type de de statistique de la superficie 79-85 (=as85), 92-97 (=as97), 04-09 (=as09)", side=3, line=2,cex=1,col="grey30")
  mtext("classification selon: as85: 1973-1991, as97: 1992-2003, as09: 2004-2015", side=3, line=1,cex=1,col="grey30")
  
  dev.off()
  embed_fonts(pdfName,outfile=pdfName)
}

