#########################################################
#########################################################
# © eRey.ch | bioGIS; erey@biogis.ch
# created on 2015.11.17
# last modified on 2015.12.01
# source('/Volumes/Data/PhD/Script/R/brochure.r')
# https://github.com/biogis/r/blob/master/brochure.r
# Let the scipt work.

#########################################################
#########################################################
# Have Fun

rm(list=ls())

source('/Volumes/Data/PhD/Script/R/Template.r')

setwd('~/Desktop/CurrentJob/DataBase/')
#setwd('C:/Users/reyemman/Dropbox')

start <- Sys.time();start

dtf <- read.csv('FRIbat_db_20151202.csv',header=T,sep=',')
names(dtf)
dim(dtf)
head(dtf)
summary(dtf)

unique(dtf$obs)
index <- which(dtf$obs=='DAUD' | dtf$obs=='DBOX' | dtf$obs=='DCHA' | dtf$obs=='DCVU' | 
                 dtf$obs=='DUEXP' | dtf$obs=='DUFQ' | dtf$obs=='DUHET')
ac <- dtf[index,]
ac$obs.rcl <- as.factor('1-.Acoustique')
# head(ac)

index <- which(dtf$obs=='DCAP')
cap <- dtf[index,]
cap$obs.rcl <- as.factor('2-.Captures')
# head(cap)

index <- which(dtf$obs=='DTEL')
rtrck <- dtf[index,]
rtrck$obs.rcl <- as.factor('3-.R-tracking')
# head(rtrck)

index <- which(dtf$obs=='DCOL' | dtf$obs=='RCRO' | dtf$obs=='TNOU' | dtf$obs=='DPHO' |  dtf$obs=='DVUE' )
gite <- dtf[index,]
gite$obs.rcl <- as.factor('4-.Gites')
# head(gite)

index <- which(dtf$obs=='RCAD' | dtf$obs=='RCECR' | dtf$obs=='RCRA' | dtf$obs=='ROSS' | 
                 dtf$obs=='RPOI')
musee <- dtf[index,]
musee$obs.rcl <- as.factor('5-.Museum')
# head(musee)

# index <- which(dtf$obs=='DGENE' | dtf$obs=='DJUM' | dtf$obs=='DVID' | dtf$obs=='TPRE')
# autre <- dtf[index,]
# autre$obs.rcl <- as.factor('6-.Autre')
# head(autre)


index <- which(is.na(dtf$obs) | dtf$obs==unique(dtf$obs)[1])
naDATA <- dtf[index,]
naDATA$obs.rcl <- as.factor('6-.NA')
# head(naDATA)

dtf <- rbind(ac,
             cap,
             rtrck,
             gite,
             musee,
#              autre,
             naDATA)
names(dtf)
dim(dtf)

index <- which(dtf$DETE!='DCOL' | is.na(dtf$DETE));length(index)
naCol <- dtf[index,]
naCol$col.rcl <- NA

index <- which(dtf$DETE=='DCOL' & dtf$M==5 |
                 dtf$DETE=='DCOL' & dtf$M==6 |
                 dtf$DETE=='DCOL' & dtf$M==7 |
                 dtf$DETE=='DCOL' & dtf$M==8);length(index)
ete <- dtf[index,]
ete$col.rcl <- as.factor('1-.Estival')
# head(ete)

index <- which(dtf$DETE=='DCOL' & dtf$M==4 |
                 dtf$DETE=='DCOL' & dtf$M==9);length(index)
transit <- dtf[index,]
transit$col.rcl <- as.factor('2-.Transit')
# head(transit)

index <- which(dtf$DETE=='DCOL' & dtf$M==1 |
                 dtf$DETE=='DCOL' & dtf$M==2 |
                 dtf$DETE=='DCOL' & dtf$M==3 |
                 dtf$DETE=='DCOL' & dtf$M==10 |
                 dtf$DETE=='DCOL' & dtf$M==11 |
                 dtf$DETE=='DCOL' & dtf$M==12);length(index)
hivers <- dtf[index,]
hivers$col.rcl <- as.factor('3-.Hivernal')
# head(hivers)

index <- which(dtf$DETE=='DCOL' & is.na(dtf$M));length(index)
autre <- dtf[index,]
autre$col.rcl <- as.factor('4-.Autre gites')
# head(autre)

dtf <- rbind(ete,transit,hivers,autre,naCol)
names(dtf)
dim(dtf)


ind <- which(dtf$Valid==7 | dtf$Kt=='VD')
dtf <- dtf[ -ind, ]
dim(dtf)
summary(dtf)


cxcykm <- as.data.frame(tapply(dtf$cxcykm,dtf$cxcykm,length));dim(cxcykm)
dtf.km.dt <- data.frame('cxcykm'=rownames(cxcykm),cxcykm)
rownames(dtf.km.dt) <- c(1:dim(dtf.km.dt)[1])
colnames(dtf.km.dt) <- c('cxcykm','data')
summary(dtf.km.dt)

ind <- which(dtf$SP=='Pipistrellus sp.' |
               dtf$SP=='Myotis sp.' |
               dtf$SP=='Vespertilionidae sp.' |
               dtf$SP=='Plecotus sp.' |
               dtf$SP=='Nyctalus sp.' |
               dtf$SP=='Chiroptera sp.' |
               dtf$SP=='Eptesicus sp.');length(ind)
dtf.sp <- dtf[ -ind,];names(dtf.sp)
sp.cxcykm <- as.data.frame(tapply(dtf.sp$SP, dtf.sp$cxcykm, FUN=function(x) length(unique(x))))
dtf.km.sp <- data.frame('cxcykm'=rownames(sp.cxcykm),sp.cxcykm)
rownames(dtf.km.sp) <- c(1:dim(dtf.km.sp)[1])
colnames(dtf.km.sp) <- c('cxcykm','sp')
summary(dtf.km.sp)

ind <- which(dtf$Genre=='Vespertilionidae'|
             dtf$Genre=='Chiroptera');length(ind)
dtf.gn <- dtf[ -ind,]
gn.cxcykm <- as.data.frame(tapply(dtf.gn$Genre, dtf.gn$cxcykm, FUN=function(x) length(unique(x))))
dtf.km.gn <- data.frame('cxcykm'=rownames(gn.cxcykm),gn.cxcykm)
rownames(dtf.km.gn) <- c(1:dim(dtf.km.gn)[1])
colnames(dtf.km.gn) <- c('cxcykm','gn')
summary(dtf.km.gn)

dtf.km <- merge(dtf.km.dt,merge(dtf.km.sp,dtf.km.gn,by='cxcykm',all=T),by='cxcykm',all=T)
summary(dtf.km)
write.csv(dtf.km,'cxcykm.dt.sp.gn.csv')

# 
# dtf <- read.csv('FRIbat_db_20151123.csv',header=T,sep=';')
# names(dtf)
# write.csv(dtf,'FRIbat_db_20151123.csv',row.names=F)
# 
# dtf <- read.csv('FRIbat_DB_Morpho_20151110.csv',header=T,sep=';')
# names(dtf)
# write.csv(dtf,'FRIbat_DB_Morpho_20151110.csv',row.names=F)

morphTot <- read.csv('FRIbat_DB_Morpho_20151110.csv',header=T,sep=',')
morphAlive <- read.csv('FRIbat_DB_Morpho_20151120.csv',header=T,sep=',')
names(morphAlive)

index <- which(morphAlive $Age=='A')
morph <- morphAlive[index,];dim(morph)


dtf.ch <- read.csv('ReyE_5x5_chiros_CH_20151125.csv',header=T,sep=',')
names(dtf.ch)
# write.csv(dtf.ch,'ReyE_5x5_chiros_CH_20151125.csv',row.names=F)


fn <- file.path("/Volumes/Data/GIS/swissadmin/Fribourg.shp")
shp.fr <- readShapeSpatial(fn)
proj4string(shp.fr) <- CRS(proj)
shp.ext <-  shp.fr@bbox 
plot(shp.fr)

fn <- file.path('/Volumes/Data/GIS/swissadmin/CH_Boundaries.shp')
ch <- readShapeSpatial(fn)
proj4string(ch) <- CRS(proj)
shp.ext <-  ch@bbox 
plot(ch)


fn <- file.path("Colonies_NEBEVD_1x1km_CSCF_20151115.shp")
bat.outer <- readShapeSpatial(fn)
proj4string(bat.outer) <- CRS(proj)
shp.ext <-  bat.outer@bbox 
plot(bat.outer)

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
plot(dem,col=grey(0:255 / 255),ext=extent,alpha=0.6,xlab='Coord X', ylab='Coord Y',legend=F,main='Fribourg')
plot(dem.fr,add=T,col=BrBG(255),alpha=0.6,legend=F)
plot(river,add=T,col='royalblue4')
plot(water,add=T,col='steelblue',border='royalblue4')
plot(shp.fr,add=T)

#####
#Reprises Myo Myo

begin.coord <- data.frame(cx=c(564250,585080,585080,564800,568624,578000,564250,552725,542825,573750), cy=c(144300,163520,163520,171650,152041,183500,144300,199561,199025,167175))

end.coord <- data.frame(cx=c(573750,572480,573750,573750,573750,572480,572480,556200,572480,570500), cy=c(167175,185700,167175,167175,167175,185700,185700,186200,185700,151900))

dt <- data.frame('label'=c('015F','034J','044J','455J','456J','457J','525M','608I','659I','953I'),
'YCap'=c(1991,1992,1992,1999,2000,2001,2001,1991,1992,1992),
'MCap'=c(8,8,9,9,9,8,8,8,8,7),
'YRecap'=c(2002,2002,2002,2002,2006,2007,2008,1991,2001,1992),
'MRecap'=c(8,6,8,8,6,6,6,9,6,9))

l <- vector("list", nrow(begin.coord))
library(sp)
for (i in seq_along(l)) {
    l[[i]] <- Lines(list(Line(rbind(begin.coord[i, ], end.coord[i,]))), as.character(i))
}

l.shp <- SpatialLines(l)
l.shp.data <- SpatialLinesDataFrame(l.shp,dt,match.ID=F)
proj4string(l.shp.data) <- CRS(proj)
plot(l.shp.data,lwd=2,col='tomato')
#writeSpatialShape(l.shp.data, '/Users/erey/Documents/GIS/PyGIS_SSD/MyoMyo_Recap_FRIbat.shp')


#####
#Reprises Myo Dau
begin.coord <- data.frame(cx=c(529040,564250,568620,570120,570120,540060,579440,529930), cy=c(198160,144300,151980,149350,149350,198160,183750,183160))

end.coord <- data.frame(cx=c(556142,568624,564250,569600,569500,579440,533753,553175), cy=c(184969,152041,144300,148750,148800,183750,195612,187925))

dt <- data.frame('label'=c('F2-845','M124','M124','M184','M186','R340','R340','Z202'),
'YCap'=c(2014,1989,2000,1989,1989,1992,1994,2007),
'MCap'=c(9,8,9,7,7,6,7,4),
'YRecap'=c(2015,1993,2000,1993,1993,1994,1996,2007),
'MRecap'=c(7,9,9,7,7,7,8,5))


l <- vector("list", nrow(begin.coord))
library(sp)
for (i in seq_along(l)) {
    l[[i]] <- Lines(list(Line(rbind(begin.coord[i, ], end.coord[i,]))), as.character(i))
}

l.shp <- SpatialLines(l)
l.shp.data <- SpatialLinesDataFrame(l.shp,dt,match.ID=F)
proj4string(l.shp.data) <- CRS(proj)
plot(l.shp.data,lwd=2,col='tomato')
#writeSpatialShape(l.shp.data, '/Users/erey/Documents/GIS/PyGIS_SSD/MyoDau_Recap_FRIbat.shp')


#####
#Reprises Ple Aur
begin.coord <- data.frame(cx=c(564250,565340,564250,564250,564250,565875,565875,564250,564250,564250), cy=c(144300,143081,144300,144300,144300,144263,144263,144300,144300,144300))

end.coord <- data.frame(cx=c(565340,564250,565875,565875,565875,564250,564250,555200,554700,556600), cy=c(143081,144300,144263,144263,144263,144300,144300,152400,151200,158400))

dt <- data.frame('label'=c('J112','J344','J435','J867','P616','P616','T127','T183','U932','V520'),
'YCap'=c(1987,1989,1986,1997,1991,1998,1998,1998,2000,2001),
'MCap'=c(8,8,9,9,8,9,9,8,9,8),
'YRecap'=c(1989,1991,1987,1998,1998,2009,1998,1998,2002,2005),
'MRecap'=c(8,10,8,9,9,8,9,8,6,8))


l <- vector("list", nrow(begin.coord))
library(sp)
for (i in seq_along(l)) {
    l[[i]] <- Lines(list(Line(rbind(begin.coord[i, ], end.coord[i,]))), as.character(i))
}

l.shp <- SpatialLines(l)
l.shp.data <- SpatialLinesDataFrame(l.shp,dt,match.ID=F)
proj4string(l.shp.data) <- CRS(proj)
plot(l.shp.data,lwd=2,col='tomato')
#writeSpatialShape(l.shp.data, '/Users/erey/Documents/GIS/PyGIS_SSD/PleAur_Recap_FRIbat.shp')





#####
#Reprises FRIbat
begin.coord <- data.frame(cx=c(627835,529040,564250,568620,570120,570120,540060,579440,529930,564250,585080,585080,564800,568624,578000,564250,552725,542825,573750,514750,564250,565340,564250,564250,564250,565875,565875,564250,564250,564250,529930),
cy=c(128950,198160,144300,151980,149350,149350,198160,183750,183160,144300,163520,163520,171650,152041,183500,144300,199561,199025,167175,145500,144300,143081,144300,144300,144300,144263,144263,144300,144300,144300,183160))

end.coord <- data.frame(cx=c(573750,556142,568624,564250,569600,569500,579440,533753,553175,573750,572480,573750,573750,573750,572480,572480,556200,572480,570500,551850,565340,564250,565875,565875,565875,564250,564250,555200,554700,556600,565130),
cy=c(167175,184969,152041,144300,148750,148800,183750,195612,187925,167175,185700,167175,167175,167175,185700,185700,186200,185700,151900,186700,143081,144300,144263,144263,144263,144300,144300,152400,151200,158400,185280))

dt <- data.frame('ESPECE' = c('Myotis blythii','Myotis daubentonii','Myotis daubentonii','Myotis daubentonii','Myotis daubentonii','Myotis daubentonii','Myotis daubentonii','Myotis daubentonii','Myotis daubentonii','Myotis myotis','Myotis myotis','Myotis myotis','Myotis myotis','Myotis myotis','Myotis myotis','Myotis myotis','Myotis myotis','Myotis myotis','Myotis myotis','Nyctalus noctula','Plecotus auritus','Plecotus auritus','Plecotus auritus','Plecotus auritus','Plecotus auritus','Plecotus auritus','Plecotus auritus','Plecotus auritus','Plecotus auritus','Plecotus auritus','Pipistrellus pipistrellus'),
'label'=c('065M','F2-845','M124','M124','M184','M186','R340','R340','Z202','015F','034J','044J','455J','456J','457J','525M','608I','659I','953I','219L','J112','J344','J435','J867','P616','P616','T127','T183','U932','V520','C3-612'),
'YCap'=c(1998,2014,1989,2000,1989,1989,1992,1994,2007,1991,1992,1992,1999,2000,2001,2001,1991,1992,1992,2006,1987,1989,1986,1997,1991,1998,1998,1998,2000,2001,2009),
'MCap'=c(8,9,8,9,7,7,6,7,4,8,8,9,9,9,8,8,8,8,7,2,8,8,9,9,8,9,9,8,9,8,7),
'YRecap'=c(2009,2015,1993,2000,1993,1993,1994,1996,2007,2002,2002,2002,2002,2006,2007,2008,1991,2001,1992,2006,1989,1991,1987,1998,1998,2009,1998,1998,2002,2005,2010),
'MRecap'=c(6,7,9,9,7,7,7,8,5,8,6,8,8,6,6,6,9,6,9,5,8,10,8,9,9,8,9,8,6,8,6))


l <- vector("list", nrow(begin.coord))
library(sp)
for (i in seq_along(l)) {
    l[[i]] <- Lines(list(Line(rbind(begin.coord[i, ], end.coord[i,]))), as.character(i))
}

l.shp <- SpatialLines(l)
l.shp.data <- SpatialLinesDataFrame(l.shp,dt,match.ID=F)
proj4string(l.shp.data) <- CRS(proj)
plot(l.shp.data,lwd=2,col='tomato')
#writeSpatialShape(l.shp.data, '/Users/erey/Documents/GIS/PyGIS_SSD/Recap_FRIbat.shp')




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

lause <- read.csv('/Volumes/Data/PhD/Datas/LandUse.CH/AS_Wlz_ALL.csv',sep=',',header=T)
lause <- data.frame('cxcyHA' = lause$RELI,
'asWlz_85' = lause$asWlz_85,
'as85' = lause$as_85,
'asWlz_97' = lause$asWlz_97,
'as97' = lause$as_97,
'asWlz_09' = lause$asWlz,
'as09' = lause$as)


dtf.lause <- merge(dtf,lause,by='cscyHa',all.x=T)
names(dtf.lause)
# write.csv(dtf,'/Users/erey/Desktop/FRIbat_Alt_ok.csv')
# write.csv(dtf,'FRIbat_db_20151110.csv',row.names=F)


dim(dtf)
names(dtf)



listSp <- c('Chiroptera',
			'Pipistrellus','Nyctalus','Myotis','Plecotus','Eptesicus',
			'Barbastella barbastellus',
            'Eptesicus nilssonii','Eptesicus serotinus',
            'Myotis alcathoe','Myotis bechsteinii','Myotis brandtii',
            'Myotis daubentonii','Myotis myotis','Myotis mystacinus','Myotis nattereri',
            'Nyctalus leisleri','Nyctalus noctula',
            'Pipistrellus kuhlii','Pipistrellus nathusii','Pipistrellus pipistrellus','Pipistrellus pygmaeus',
            'Plecotus auritus','Plecotus austriacus',
            'Miniopterus schreibersii',
            'Rhinolophus hipposideros',
            'Vespertilio murinus',
            'Myotis blythii','Rhinolophus ferrumequinum')


for(sp in listSp){
  # sp <- listSp[1];sp
  # sp <- listSp[21];sp
  # sp <- listSp[8];sp
  # sp <- listSp[14];sp
  print(sp)

###--Select for Chiroptera
  if(any(sp=='Chiroptera')){
  	spAbbr <- sp
  	pdfName <- paste('/Users/erey/Dropbox/Brochure FRIbat/Cartes/Genre',paste(sp,'data.pdf',sep='_'),sep='/')
  	batSp <- dtf
  	bat.outer.sp <- bat.outer
  	batmorph <- morph
  	batCH <- dtf.ch

  ABmin <- NA
  ABmax <- NA
  ABmedian <- NA
  ABmean <- NA
  Pmin <- NA
  Pmax <- NA
  Pmedian <- NA
  Pmean <- NA
  Pce <- NA
  GrffPce <- NA
  	}

###--Select for Gender
  if(!any(sp=='Chiroptera') & any(sp=='Pipistrellus' | sp=='Nyctalus' | sp=='Myotis' | sp=='Plecotus' | sp=='Eptesicus')){
  	spAbbr <- sp
  	pdfName <- paste('/Users/erey/Dropbox/Brochure FRIbat/Cartes/Genre',paste(sp,'data.pdf',sep='_'),sep='/')
  		batSp<-subset(dtf,dtf$Genre==sp);dim(batSp)
	bat.outer.sp <- subset(bat.outer,bat.outer@data$Genre==sp)
	batmorph <- subset(morph,morph$Genre==sp);dim(batmorph)
	batCH <- subset(dtf.ch, dtf.ch$Genre==sp);dim(batCH)

  ABmin <- NA
  ABmax <- NA
  ABmedian <- NA
  ABmean <- NA
  Pmin <- NA
  Pmax <- NA
  Pmedian <- NA
  Pmean <- NA
  Pce <- NA
  GrffPce <- NA
}
  
  if(!any(sp=='Chiroptera' | sp=='Pipistrellus' | sp=='Nyctalus' | sp=='Myotis' | sp=='Plecotus' | sp=='Eptesicus')){
  	  genreName <- strsplit(sp,split=' ')[[1]][1]
  	  spName <- strsplit(sp,split=' ')[[1]][2]
  	  spAbbr <- paste(toupper(strsplit(genreName,split='')[[1]][1]),strsplit(genreName,split='')[[1]][2],strsplit(genreName,split='')[[1]][3],
  	  				  toupper(strsplit(spName,split='')[[1]][1]),strsplit(spName,split='')[[1]][2],strsplit(spName,split='')[[1]][3],sep='')
      pdfName <- paste('/Users/erey/Dropbox/Brochure FRIbat/Cartes/sp',paste(spAbbr,'data.pdf',sep='_'),sep='/')
      
#Select dtf for species
batSp<-subset(dtf,dtf$SP==sp);dim(batSp)
bat.outer.sp <- subset(bat.outer,bat.outer@data$ESPECE==sp) 
if(any(l.shp.data@data$ESPECE ==sp)){l.shp.data.sp <-  subset(l.shp.data,l.shp.data@data$ESPECE==sp)}
batmorph <- subset(morph,morph$SP==sp);dim(batmorph)
batCH <- subset(dtf.ch, dtf.ch$ESPECE==sp);dim(batCH)

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
}

print(spAbbr)
print(pdfName)
  
names(batSp);dim(batSp)
names(batmorph);dim(batmorph)
  
  print(summary(batSp$datas.dem2))
  print(summary(batmorph$AB,na.rm=T))
  print(summary(batmorph$P,na.rm=T))
  print(summary(batSp$TYP_COL))
  print(summary(batSp$obs))
  print(summary(batSp$obs.rcl))
  print(summary(batSp$col.rcl))


  
  dtf.fc<-subset(batSp,batSp$TYP_COL=='FC' & batSp$A>=2010);dim(dtf.fc)
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
  nMorph <- dim(batmorph)[1]
 
   
  dtf.resum.site <- t(data.frame('nbre données' = nData,
                                 'ancienne donnée' = oldData,
                                 'donnée récente' = lastData,
                                 'nbre km2' = nSQkm,
                                 'nbre colonies' = nCol,
                                 'altitude min' = minAlt,
                                 'altitude max' = maxAlt,
                                 'altitude moyenne' = meanAlt,
                                 'altitude mediane' = medianAlt))
  dtf.resum.morph <- t(data.frame('nbre données morpho' = nMorph,
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
  print(dtf.resum.site)
  print(dtf.resum.morph)
  
  
  #Create Point shapefile for the given dtf
  names(dtf)
  xy <- data.frame('cx' = batSp$cx,'cy' = batSp$cy)
  bat.shp <- SpatialPointsDataFrame(coords = xy, data = batSp); bat.shp
  proj4string(bat.shp) <- CRS(proj); bat.shp
  
   #Create Point shapefile for the given dtf
  names(dtf.ch)
  xy.ch <- data.frame('cx' = batCH$XN5,'cy' = batCH$YN5)
  batCH.shp <- SpatialPointsDataFrame(coords = xy.ch, data = batCH); batCH.shp
  proj4string(batCH.shp) <- CRS(proj); batCH.shp
  
  plot(bat.shp,col='tomato')
  plot(bat.outer.sp,add=T,col='steelblue')
  head(batSp);tail(batSp)
  bat.shp


########  ########  ########  ########  ########  
# Plot all data in a pdf
########  ########  ########  ########  ########  

  pdf(pdfName,family='Akkurat',paper='a4r',width=11,height=8.5)
#   pdf('testrplot.pdf',paper='a4r',width=11,height=8.5)

	plot(ch,main=sp)
	plot(batCH.shp,add=T,pch=15,col='grey35',cex=0.8)
  
  plot(dem,col=grey(0:255 / 255),ext=extent,alpha=0.4,xlab='Coord X', ylab='Coord Y',legend=F,main=sp)
  plot(dem.fr,add=T,col=BrBG(255),alpha=0.6,legend=F)
  plot(river,add=T,col='royalblue4')
  plot(water,add=T,col='steelblue',border='royalblue4')
  plot(shp.fr,add=T)
  if(any(ls()=='l.shp.data.sp')){plot(l.shp.data.sp,add=T,lwd=2,col='yellow')
  	rm(l.shp.data.sp)}
  plot(bat.shp,add=T,pch=4, col='tomato',lwd=2)
  plot(bat.outer.sp,add=T,pch=4,col='limegreen',lwd=2)
    
  
  
  #######
  #Type dtf
  #######
  plot(1,pch=0,col='white',axes=F,ann=F)
  text(0.7,1, paste(capture.output(dtf.resum.site), collapse='\n'), pos=4,cex=1,col='grey30')
  text(1,1, paste(capture.output(dtf.resum.morph), collapse='\n'), pos=4,cex=1,col='grey30')
  mtext(sp,side=3,line=3,col='grey30')
  
  
  #######
  #Type Year
  #######
  
  A <- table(batSp$A)
  
  minA <- min(batSp$A,na.rm=T)
  maxA <- max(batSp$A,na.rm=T)
  A.seq <- data.frame('A.rcl' = as.factor(seq(minA,maxA,by=1)))
  A.dat <- as.data.frame(A)
  dtf.seq <- merge(A.dat,A.seq,by.x='Var1',by.y='A.rcl',all=T)
  dtf.seq$A.rcl <- as.numeric(as.character(dtf.seq$Var1))
  dtf.seq <- dtf.seq[ order(dtf.seq[,3]),]
  
  AProp <- floor(A/sum(A)*100)
  AName <- paste(A,paste(AProp,'%',sep=' '),sep='-');AName
  # plot(A,bty='n',axes=F,ann=F,lwd=5,type='h',col='tomato')
  
  # mtext("Nbre de données", side=2, line=3,cex=1.2,col="grey30")
  # box(col="grey")
  
  # if(!any(sp=='Myotis blythii' | sp=='Rhinolophus ferrumequinum')){
  	# axis(1,at=seq(floor(min(batSp$A,na.rm=T)/5)*5,2015,by=5),cex.axis=1,col.axis='grey30',col='grey30',las=2)}
  # # axis for RhiFer and MyoBly: 
  # if(any(sp=='Myotis blythii' | sp=='Rhinolophus ferrumequinum')){
  	# axis(1,at=min(batSp$A,na.rm=T),cex.axis=1,col.axis='grey30',col='grey30',las=2)}


  # axis(2,cex.axis=1,col.axis='grey30',col='grey30')
  # axis(4,cex.axis=1,col.axis='grey30',col='grey30')
# # mtext(sp,side=3,line=3,col='grey10')
  # mtext("Années", side=3, line=3,cex=1.2,col="grey30")
  # mtext(paste('Données la plus ancienne:',oldData,'--',
              # 'Données la plus récente:',lastData,sep=' '),
        # side=3, line=1,col='grey30')
  
  if(!any(sp=='Chiroptera' | sp=='Pipistrellus' | sp=='Nyctalus' | sp=='Myotis' | sp=='Plecotus' | sp=='Eptesicus')){
  h2 <- seq(0,max(dtf.seq$Freq,na.rm=T),by=2)
  abline(h=h2,lty=2,lwd=0.5)
  h10 <- seq(0,max(dtf.seq$Freq,na.rm=T),by=10)
  abline(h=h10,lty=1,lwd=0.7)
  h50 <- seq(0,max(dtf.seq$Freq,na.rm=T),by=50)
  abline(h=h50,lty=1,lwd=1)
  }

  if(any(sp=='Chiroptera' | sp=='Pipistrellus' | sp=='Nyctalus' | sp=='Myotis' | sp=='Plecotus' | sp=='Eptesicus')){
  h2 <- 0
  h10 <- seq(0,max(dtf.seq$Freq,na.rm=T),by=10)
  abline(h=h10,lty=1,lwd=0.7)
  h50 <- seq(0,max(dtf.seq$Freq,na.rm=T),by=50)
  abline(h=h50,lty=1,lwd=1)
  }

  # text(min(batSp$A,na.rm=T),max(A)-(max(A/2)), paste(capture.output(dtf.resum.site), collapse='\n'), pos=4,cex=1)
  # mtext(sp,side=3,line=3,col='grey10')

  csvName <- paste('/Users/erey/Dropbox/Brochure FRIbat/Cartes/csv/',spAbbr,'_Data_Year','.csv',sep='')
  write.csv(dtf.seq[,2:3],csvName,row.names=F)
  
    
  #######
  #Type Year vs. Obs
  #######

  
  Y.obs <- table(batSp$obs.rcl,batSp$A);Y.obs
  
  colnames <- unique(as.factor(as.data.frame(Y.obs)$Var2));print(colnames)
  colCode <- c()
  if(any(colnames=='1-.Acoustique',na.rm=T)){
  col <- 'steelblue'
  colCode <- c(colCode, col)}
  if(any(colnames=='2-.Captures',na.rm=T)){
  col <- 'turquoise'
  colCode <- c(colCode, col)}
if(any(colnames=='3-.R-tracking',na.rm=T)){
  col <- 'lightskyblue'
  colCode <- c(colCode, col)}
if(any(colnames=='4-.Gites',na.rm=T)){
  col <- 'limegreen'
  colCode <- c(colCode, col)}
if(any(colnames=='5-.Museum',na.rm=T)){
  col <- 'gold'
  colCode <- c(colCode, col)}
if(any(colnames=='6-.NA',na.rm=T)){
  col <- 'tomato'
  colCode <- c(colCode, col)}
  print(colCode)

  
  minA <- min(batSp$A,na.rm=T)
  maxA <- max(batSp$A,na.rm=T)
  A.seq <- data.frame('A.rcl' = as.factor(seq(minA,maxA,by=1)))
  A.dat <- as.data.frame(Y.obs)
  dtf.seq <- merge(A.dat,A.seq,by.x='Var2',by.y='A.rcl',all=T)
  dtf.seq$A.rcl <- as.numeric(as.character(dtf.seq$Var2))
  dtf.seq <- dtf.seq[ order(dtf.seq[,4]),]


p <- ggplot(dtf.seq, aes(x=dtf.seq$A.rcl, y=dtf.seq$Freq, fill=dtf.seq$Var1))
pp <- p+geom_bar(stat="identity")+
	theme_bw(base_size = 12, base_family = "Akkurat")+
	scale_y_continuous(breaks=h10) +
	scale_x_continuous(breaks=seq(floor(min(dtf.seq$A.rcl,na.rm=T)/5)*5,2015,by=5))+
    geom_hline(yintercept=h2,colour='grey80',size=.2,lintype='dashed')+
    geom_hline(yintercept=h10,colour='grey80',size=.2,lintype='dashed')+
	theme(axis.text.x=element_text(colour='black',angle=30,hjust=1,vjust=1),
        axis.title.x=element_blank())+
	theme(axis.title.y=element_blank())+
	scale_fill_manual(values=c("steelblue","turquoise","lightskyblue","limegreen","gold","tomato"))+
	guides(fill=guide_legend(title=NULL))+
	theme(legend.position=c(0.1,0.85)) +
	theme(legend.background=element_blank()) + 
	theme(legend.key=element_blank())+
	ggtitle(paste("Années",paste('Données la plus ancienne:',oldData,'--','Données la plus récente:',lastData,sep=' '),sep='\n'))

print(pp)


csvName <- paste('/Users/erey/Dropbox/Brochure FRIbat/Cartes/csv/',spAbbr,'_Data_Year_vs_TYPObs','.csv',sep='')
write.csv(dtf.seq[,2:4],csvName,row.names=F)

  
  #######
  #Type Mois
  #######
  
  M <- table(batSp$M)
  M.seq <- data.frame('M.rcl' = as.factor(c(1:12)))
  M.dat <- as.data.frame(M)
  dtf.seq <- merge(M.dat,M.seq,by.x='Var1',by.y='M.rcl',all=T)
  dtf.seq$M.rcl <- as.numeric(as.character(dtf.seq$Var1))
  dtf.seq <- dtf.seq[ order(dtf.seq[,3]), ];print(dtf.seq)
  dtf.seq <- dtf.seq[ order(dtf.seq[,3]),]

  
  MProp <- floor(dtf.seq $Freq/sum(dtf.seq$Freq,na.rm=T)*100)
  MName <- paste(dtf.seq$Freq,paste(MProp,'%',sep=' '),sep='-');MName
  MName[which(is.na(dtf.seq$Freq))] <- NA;MName
  
  plot(dtf.seq$M.rcl,dtf.seq$Freq,bty='n',axes=F,ann=F,lwd=5,type='h',col='tomato',ylim=c(0,max(dtf.seq$Freq,na.rm=T)+1))
  text(dtf.seq$M.rcl,dtf.seq$Freq+0.5, labels = MName,col='grey20',cex=0.8)
  
  mtext("Nbre de données", side=2, line=3,cex=1.2,col="grey30")
  box(col="grey")
  
  axis(1,at=c(1:12),labels=c('jan','fev','mar','avr','mai','jun','jui','aou','sep','oct','nov','dec')
       ,cex.axis=1,col.axis='grey30',col='grey30')
  axis(2,cex.axis=1,col.axis='grey30',col='grey30')
  axis(4,cex.axis=1,col.axis='grey30',col='grey30')
  # mtext(sp,side=3,line=3,col='grey10')
  mtext("Mois", side=3, line=3,cex=1.2,col="grey30")
  # mtext(paste('Données la plus courante:',floor(median(batSp$M,na.rm=T)),sep=' '),
  #       side=3, line=1,col='grey10')
  
  csvName <- paste('/Users/erey/Dropbox/Brochure FRIbat/Cartes/csv/',spAbbr,'_Data_Month','.csv',sep='')
  write.csv(dtf.seq[,2:3],csvName,row.names=F)

  
  #######
  #Type altitude
  #######
  
  alt <- table(batSp$datas.rcl100)
  minAltrcl <- min(batSp$datas.rcl100,na.rm=T)
  maxAltrcl <- max(batSp$datas.rcl100,na.rm=T)
  alt.seq <- data.frame('alt.rcl' = as.factor(seq(minAltrcl,maxAltrcl,by=100)))
  alt.dat <- as.data.frame(alt)
  dtf.seq <- merge(alt.dat,alt.seq,by.x='Var1',by.y='alt.rcl',all=T)
  dtf.seq$alt.rcl <- as.numeric(as.character(dtf.seq$Var1))
  dtf.seq <- dtf.seq[ order(dtf.seq[,3]),]
  
  altProp <- floor(dtf.seq $Freq/sum(dtf.seq$Freq,na.rm=T)*100)
  altName <- paste(dtf.seq$Freq,paste(altProp,'%',sep=' '),sep='-'); altName
  altName[which(is.na(dtf.seq$Freq))] <- NA; altName
  # plot(alt,bty='n',axes=F,ann=F,lwd=5,type='h',col='tomato')
  plot(dtf.seq$alt.rcl,dtf.seq$Freq,bty='n',axes=F,ann=F,lwd=5,type='h',col='tomato',ylim=c(0,max(dtf.seq$Freq,na.rm=T)+1))
  text(dtf.seq$alt.rcl,dtf.seq$Freq+0.5, labels = altName,col='grey20',cex=0.8)
  
  
  mtext("Nbre de données", side=2, line=3,cex=1.2,col="grey30")
  box(col="grey")
  
  axis(1,at=seq(min(batSp$datas.rcl100,na.rm=T),max(batSp$datas.rcl100,na.rm=T),by=100),cex.axis=1,col.axis='grey30',col='grey30',las=2)
  axis(2,cex.axis=1,col.axis='grey30',col='grey30')
  axis(4,cex.axis=1,col.axis='grey30',col='grey30')
  
  # mtext(sp,side=3, line=3,col='grey10')
  mtext("Altitude", side=3, line=3,cex=1.2,col="grey30")
  mtext(paste('Altitude la plus basse:',minAlt,'--',
              'Altitude moyenne',meanAlt,'--',
              'Altitude médiane',medianAlt,'--',
              'Altitude la plus élevée:',maxAlt,sep=' '),
        side=3, line=1,col='grey30')
        
csvName <- paste('/Users/erey/Dropbox/Brochure FRIbat/Cartes/csv/',spAbbr,'_Data_Altitude','.csv',sep='')
write.csv(dtf.seq[,2:3],csvName,row.names=F)
  
  
  #######
  #Type obs
  #######
  obs <- table(batSp$obs.rcl)
  propOBS <- floor(obs/sum(obs)*100)
  OBSName <- paste(obs,paste(propOBS,'%',sep=' '),sep='-');OBSName
  plot(obs,bty='n',axes=F,ann=F,lwd=5,type='h',col='tomato',ylim=c(0,max(obs,na.rm=T)+1))
  text(obs+0.5, labels = OBSName,col='grey20',cex=0.8)
  rownames <- as.factor(as.data.frame(obs)$Var1)
  
  mtext("Nbre d'observation", side=2, line=3,cex=1,col="grey30")
  box(col="grey")
  
  axis(1,at=c(1:length(rownames)),label=rownames,cex.axis=0.7,las=2,col.axis='grey30',col='grey30')
  axis(2,cex.axis=1,col.axis='grey30',col='grey30')
  axis(4,cex.axis=1,col.axis='grey30',col='grey30')
  # mtext(sp,side=3, line=3,col='grey10')
  mtext("Type d'observation", side=3, line=3,cex=1.2,col="grey30")
  
  # obs <- table(batSp$obs,useNA='no')
  # propobs <- floor(obs/sum(obs)*100)
  # pie(propobs, clockwise=T,col=jet.colors(length(typCOL)),label=names(obs),cex=0.6)
  # mtext("Type d'observations", side=3, line=1,cex=1.2,col="grey30")
  
csvName <- paste('/Users/erey/Dropbox/Brochure FRIbat/Cartes/csv/',spAbbr,'_Data_TYPObs','.csv',sep='')
write.csv(obs,csvName,row.names=F)

  
  #######
  #Type Year vs. obs
  #######
  Y.obs <- table(batSp$A,batSp$obs.rcl);Y.obs
  dim(Y.obs)
  colnames <- unique(as.factor(as.data.frame(Y.obs)$Var2));print(colnames)
  colCode <- c()
  if(any(colnames=='1-.Acoustique',na.rm=T)){
  col <- 'steelblue'
  colCode <- c(colCode, col)}
  if(any(colnames=='2-.Captures',na.rm=T)){
  col <- 'turquoise'
  colCode <- c(colCode, col)}
if(any(colnames=='3-.R-tracking',na.rm=T)){
  col <- 'lightskyblue'
  colCode <- c(colCode, col)}
if(any(colnames=='4-.Gites',na.rm=T)){
  col <- 'limegreen'
  colCode <- c(colCode, col)}
if(any(colnames=='5-.Museum',na.rm=T)){
  col <- 'gold'
  colCode <- c(colCode, col)}
if(any(colnames=='6-.NA',na.rm=T)){
  col <- 'tomato'
  colCode <- c(colCode, col)}
  print(colCode)

  plot(Y.obs,col=colCode,las=2,cex=0.6,main="Types d'observations par année")
  legend('topleft', horiz=F,bg='white',box.col='white',ncol=1, cex=0.7, 
         legend=colnames, pch=16,col=colCode)
  
  # mtext("Fréquences d'observations", side=2, line=3,cex=1,col="grey30")
  # box(col="grey")
  
  # axis(2,cex.axis=1,col.axis='grey30',col='grey30')
  # mtext(sp,side=3, line=3,col='grey10')
  
# csvName <- paste('/Users/erey/Dropbox/Brochure FRIbat/Cartes/csv/',spAbbr,'_Data_TYPObs_vs_Year','.csv',sep='')
# write.csv(Y.obs,csvName,row.names=F)

  
  #######
  #Type colonies
  #######
  
  typCOL <- table(batSp$col.rcl)
  propCOL <- floor(typCOL/sum(typCOL)*100)
  TYPName <- paste(typCOL,paste(propCOL,'%',sep=' '),sep='-');TYPName
  
  plot(typCOL,bty='n',axes=F,ann=F,lwd=5,type='h',col='tomato',ylim=c(0,max(typCOL,na.rm=T)+1))
  text(typCOL+0.5, labels = TYPName,col='grey20',cex=0.8)
  
  rownames <- as.factor(as.data.frame(typCOL)$Var1)
  
  mtext("Nbre de données", side=2, line=3,cex=1.2,col="grey30")
  box(col="grey")
  
  axis(1,at=c(1:length(rownames)),label=rownames,cex.axis=1,col.axis='grey30',col='grey30')
  axis(2,cex.axis=1,col.axis='grey30',col='grey30')
  axis(4,cex.axis=1,col.axis='grey30',col='grey30')
  # mtext(sp,side=3, line=3,col='grey10')
  mtext("Type de gîte", side=3, line=3,cex=1.2,col="grey30")
  
  # typCOL <- table(batSp$TYP_COL,useNA='no')
  # proptypCOL <- floor(typCOL/sum(typCOL)*100)
  # pie(propCOL, clockwise=T,col=jet.colors(length(typCOL)),label=names(typCOL),cex=0.6)
  # mtext("Type de gîte", side=3, line=1,cex=1.2,col="grey30")
  # 
  
csvName <- paste('/Users/erey/Dropbox/Brochure FRIbat/Cartes/csv/',spAbbr,'_Data_typCOL','.csv',sep='')
write.csv(typCOL,csvName,row.names=F)


  #######
  #Type Land Use
  #######
  
  lause <- table(batSp$LaUse,useNA='always')
  proplause <- floor(lause/sum(lause)*100)
  lulcName <- paste(lause,paste(proplause,'%',sep=' '),sep='-');lulcName
  
  plot(lause,bty='n',axes=F,ann=F,lwd=5,type='h',col='tomato',ylim=c(0,max(lause)+1))
  text(lause+0.5, labels = lulcName,col='grey20',cex=0.8)
  
  rownames <- as.factor(as.data.frame(lause)$Var1)
  
  mtext("Nbre de données", side=2, line=3,cex=1.2,col="grey30")
  box(col="grey")
  
  axis(1,at=c(1:length(rownames)),label=rownames,cex.axis=0.8,las=2,col.axis='grey30',col='grey30')
  axis(2,cex.axis=1,col.axis='grey30',col='grey30')
  axis(4,cex.axis=1,col.axis='grey30',col='grey30')
  # mtext(sp,side=3, line=3,col='grey10')
  mtext("Type de de statistique de la superficie 79-85 (=as85), 92-97 (=as97), 04-09 (=as09)", side=3, line=2,cex=1,col="grey30")
  mtext("classification selon: as85: 1973-1991, as97: 1992-2003, as09: 2004-2015", side=3, line=1,cex=1,col="grey30")
  
  
csvName <- paste('/Users/erey/Dropbox/Brochure FRIbat/Cartes/csv/',spAbbr,'_Data_LandUse','.csv',sep='')
write.csv(lause,csvName,row.names=F)
  
  dev.off()
  embed_fonts(pdfName,outfile=pdfName)
}
end <- Sys.time()
end-start
