source('/Volumes/Data/PhD/Script/R/Template.r')

setwd('~/Desktop/CurrentJob/DataBase/')
setwd('C:/Users/reyemman/Dropbox')

dtf <- read.csv('FRIbat_db_20151110.csv',header=T,sep=',')
names(dtf)
dim(dtf)
head(dtf)
summary(dtf)

index <- which(dtf$obs=='DAUD' | dtf$obs=='DBOX' | dtf$obs=='DCHA' | dtf$obs=='DCVU' | 
                 dtf$obs=='DUEXP' | dtf$obs=='DUFQ' | dtf$obs=='DUHET')
ac <- dtf[index,]
ac$obs.rcl <- as.factor('1-.Acoustique')
head(ac)

index <- which(dtf$obs=='DCAP')
cap <- dtf[index,]
cap$obs.rcl <- as.factor('2-.Captures')
head(cap)

index <- which(dtf$obs=='DCOL' | dtf$obs=='RCRO' | dtf$obs=='TNOU')
gite <- dtf[index,]
gite$obs.rcl <- as.factor('3-.Gites')
head(gite)

index <- which(dtf$obs=='RCAD' | dtf$obs=='RCECR' | dtf$obs=='RCRA' | dtf$obs=='ROSS' | 
                 dtf$obs=='RPOI')
musee <- dtf[index,]
musee$obs.rcl <- as.factor('4-.Museum')
head(musee)

index <- which(dtf$obs=='DGENE' | dtf$obs=='DJUM' | dtf$obs=='DPHO' | dtf$obs=='DTEL' | 
                 dtf$obs=='DVID' | dtf$obs=='DVUE' | dtf$obs=='TPRE')
autre <- dtf[index,]
autre$obs.rcl <- as.factor('5-.Autre')
head(autre)


index <- which(is.na(dtf$obs) | dtf$obs==unique(dtf$obs)[7])
naDATA <- dtf[index,]
naDATA$obs.rcl <- as.factor('6-.NA')
head(naDATA)

dtf <- rbind(ac,cap,gite,musee,autre,naDATA)
names(dtf)
dim(dtf)

ind <- which(dtf$Valid==7 | dtf$Kt=='VD')
dtf <- dtf[ -ind, ]
dim(dtf)
summary(dtf)
# 
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

listSp <- c('Pipistrellus','Nyctalus','Myotis','Plecotus','Eptesicus')

# 
listSp <- c('Barbastella barbastellus',
            'Eptesicus nilssonii','Eptesicus serotinus',
            'Myotis alcathoe','Myotis bechsteinii','Myotis brandtii',
            'Myotis daubentonii','Myotis myotis','Myotis mystacinus','Myotis nattereri',
            'Nyctalus leisleri','Nyctalus noctula',
            'Pipistrellus kuhlii','Pipistrellus nathusii','Pipistrellus pipistrellus','Pipistrellus pygmaeus',
            'Plecotus auritus','Plecotus austriacus',
            'Miniopterus schreibersii',
            'Rhinolophus hipposideros',
            'Vespertilio murinus')
# 
listSp <- c('Myotis blythii','Rhinolophus ferrumequinum');listSp


for(sp in listSp){
  # sp <- listSp[1];sp
  # sp <- listSp[21];sp
  # sp <- listSp[10];sp
  print(sp)
  genreName <- strsplit(sp,split=' ')[[1]][1]
  spName <- strsplit(sp,split=' ')[[1]][2]
  pdfName <- paste('/Users/erey/Dropbox/Brochure FRIbat/Cartes/sp',
                   paste(
                     paste(toupper(strsplit(genreName,split='')[[1]][1]),strsplit(genreName,split='')[[1]][2],strsplit(genreName,split='')[[1]][3],
                           toupper(strsplit(spName,split='')[[1]][1]),strsplit(spName,split='')[[1]][2],strsplit(spName,split='')[[1]][3],
                           sep=''),'data.pdf',sep='_'),sep='/')
  
#   pdfName <- paste('/Users/erey/Dropbox/Brochure FRIbat/Cartes/Genre',paste(sp,'data.pdf',sep='_'),sep='/')
  
  print(pdfName)
  
batSp<-subset(dtf,dtf$SP==sp);dim(batSp)
# batSp<-subset(dtf,dtf$Genre==sp);dim(batSp)
# batSp <- dtf

  names(batSp)
  dim(batSp)
  
bat.outer.sp <- subset(bat.outer,bat.outer@data$ESPECE==sp)
# bat.outer.sp <- subset(bat.outer,bat.outer@data$Genre==sp)
# bat.outer.sp <- bat.outer
  
  
batmorph <- subset(morph,morph$SP==sp);dim(batmorph)
# batmorph <- subset(morph,morph$Genre==sp);dim(batmorph)
# batmorph <- morph
  names(batmorph)
  dim(batmorph)
  
  print(summary(batSp$datas.dem2))
  print(summary(batmorph$AB,na.rm=T))
  print(summary(batmorph$P,na.rm=T))
  print(summary(batSp$TYP_COL))
  print(summary(batSp$obs))
  print(summary(batSp$obs.rcl))

  
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
  nMorph <- dim(batmorph)[1]
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

#   ABmin <- NA
#   ABmax <- NA
#   ABmedian <- NA
#   ABmean <- NA
#   Pmin <- NA
#   Pmax <- NA
#   Pmedian <- NA
#   Pmean <- NA
#   Pce <- NA
#   GrffPce <- NA
#   
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
  shp <- SpatialPointsDataFrame(coords = xy, data = batSp);shp
  proj4string(shp) <- CRS(proj);shp
  
  plot(shp,col='tomato')
  plot(bat.outer.sp,add=T,col='steelblue')
  head(batSp);tail(batSp)
  shp
  
  pdf(pdfName,family='Akkurat',paper='a4r',width=11,height=8.5)
#   pdf('testrplot.pdf',paper='a4r',width=11,height=8.5)
  
  plot(dem,col=grey(0:255 / 255),ext=extent,alpha=0.4,xlab='Coord X', ylab='Coord Y',legend=F,main=sp)
  plot(dem.fr,add=T,col=BrBG(255),alpha=0.6,legend=F)
  plot(river,add=T,col='royalblue4')
  plot(water,add=T,col='steelblue',border='royalblue4')
  plot(shp.fr,add=T)
  plot(shp,add=T,pch=4, col='tomato',lwd=2)
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
  dtf.seq[ order(dtf.seq[,3]), ]
  
  AProp <- floor(A/sum(A)*100)
  AName <- paste(A,paste(AProp,'%',sep=' '),sep='-');AName
  plot(A,bty='n',axes=F,ann=F,lwd=5,type='h',col='tomato')
  #   plot(dtf.seq$A.rcl,dtf.seq$Freq,bty='n',axes=F,ann=F,lwd=5,type='h',col='tomato')
  #   text(dtf.seq$A.rcl,dtf.seq$Freq+1, labels = MName,col='grey20',cex=0.8)
  
  mtext("Nbre de données", side=2, line=3,cex=1.2,col="grey30")
  box(col="grey")
  
#   axis(1,at=seq(floor(min(batSp$A,na.rm=T)/5)*5,2015,by=5),cex.axis=1,col.axis='grey30',col='grey30',las=2)
  # axis for RhiFer and MyoBly: 
  axis(1,at=min(batSp$A,na.rm=T),cex.axis=1,col.axis='grey30',col='grey30',las=2)
  axis(2,cex.axis=1,col.axis='grey30',col='grey30')
  axis(4,cex.axis=1,col.axis='grey30',col='grey30')
# mtext(sp,side=3,line=3,col='grey10')
  mtext("Années", side=3, line=3,cex=1.2,col="grey30")
  mtext(paste('Données la plus ancienne:',oldData,'--',
              'Données la plus récente:',lastData,sep=' '),
        side=3, line=1,col='grey30')
  
  #   h5 <- seq(0,max(dtf.seq$Freq,na.rm=T),by=5)
  #   abline(h=h5,lty=6,lwd=0.5)
  h2 <- seq(0,max(dtf.seq$Freq,na.rm=T),by=2)
  abline(h=h2,lty=2,lwd=0.5)
  h10 <- seq(0,max(dtf.seq$Freq,na.rm=T),by=10)
  abline(h=h10,lty=1,lwd=0.7)
  h50 <- seq(0,max(dtf.seq$Freq,na.rm=T),by=50)
  abline(h=h50,lty=1,lwd=1)

  # text(min(batSp$A,na.rm=T),max(A)-(max(A/2)), paste(capture.output(dtf.resum.site), collapse='\n'), pos=4,cex=1)
  # mtext(sp,side=3,line=3,col='grey10')
  
  
  
  
  #######
  #Type Mois
  #######
  
  M <- table(batSp$M)
  M.seq <- data.frame('M.rcl' = as.factor(c(1:12)))
  M.dat <- as.data.frame(M)
  dtf.seq <- merge(M.dat,M.seq,by.x='Var1',by.y='M.rcl',all=T)
  dtf.seq$M.rcl <- as.numeric(as.character(dtf.seq$Var1))
  dtf.seq <- dtf.seq[ order(dtf.seq[,3]), ]
  
  MProp <- floor(M/sum(M)*100)
  MName <- paste(M,paste(MProp,'%',sep=' '),sep='-');MName
  
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
  dtf.seq <- dtf.seq[ order(dtf.seq[,3]), ]
  
  altProp <- floor(alt/sum(alt)*100)
  altName <- paste(alt,paste(altProp,'%',sep=' '),sep='-');altName
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
  col <- 'turquoise3'
  colCode <- c(colCode, col)}
  if(any(colnames=='3-.Gites',na.rm=T)){
  col <- 'limegreen'
  colCode <- c(colCode, col)}
  if(any(colnames=='4-.Museum',na.rm=T)){
  col <- 'gold'
  colCode <- c(colCode, col)}
  if(any(colnames=='5-.Autre',na.rm=T)){
  col <- 'tomato'
  colCode <- c(colCode, col)}
if(any(colnames=='6-.NA',na.rm=T)){
  col <- 'red4'
  colCode <- c(colCode, col)}
  print(colCode)

  plot(Y.obs,col=colCode,las=2,cex=0.6,main="Types d'observations par année")
  legend('topleft', horiz=F,bg='white',box.col='white',ncol=1, cex=0.7, 
         legend=colnames, pch=16,col=colCode)
  
  # mtext("Fréquences d'observations", side=2, line=3,cex=1,col="grey30")
  # box(col="grey")
  
  # axis(2,cex.axis=1,col.axis='grey30',col='grey30')
  # mtext(sp,side=3, line=3,col='grey10')
  
  
  
  #######
  #Type colonies
  #######
  
  typCOL <- table(batSp$TYP_COL,useNA='always')
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
  # pie(proptypCOL, clockwise=T,col=jet.colors(length(typCOL)),label=names(typCOL),cex=0.6)
  # mtext("Type de gîte", side=3, line=1,cex=1.2,col="grey30")
  # 
  
  
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
  
  dev.off()
  embed_fonts(pdfName,outfile=pdfName)
}
