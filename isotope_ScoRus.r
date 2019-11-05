#########################################################
#########################################################
# © eRey.ch | bioGIS; erey@biogis.ch
# created on 2019.11.01

# Geospatial analysis of isotope analysis on a woodcock project
# info fauna CSCF & KARCH - @ Thierry Bohnenstengel

#########################################################
#########################################################


#fix timezone on Bern
Sys.setenv(TZ='Europe/Paris')
# clear all objects saved in the R session
rm(list=ls())

#get proj4 for LV03 or LV95 for any projection map or spatial analysis requireng a projection

#LV03
proj.lv03 <- '+proj=somerc +lat_0=46.95240555555556 +lon_0=7.439583333333333 +k_0=1 +x_0=600000 +y_0=200000 +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs '

#LV95
proj.lv95 <- '+proj=somerc +lat_0=46.95240555555556 +lon_0=7.439583333333333 +k_0=1 +x_0=2600000 +y_0=1200000 +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs'

#wgs84
proj.wgs <- '+proj=longlat +datum=WGS84 +no_defs'

# Library list
packages <- c(
  #stat libraries
  'zoo','data.table','IsoriX',
  
  #Spatial libraries
  'foreign','sp','raster','maps','proj4','rgeos','maptools','rgdal','spatial',
  
  #graphics libraries,
  'TeachingDemos','ggplot2','RColorBrewer','extrafont','jpeg','png',
  
  #R libraries
  'telegram.bot','rawr'#'installr'
)


# Check if library exist, install and/or update and activate it

for(pkg in packages){print(pkg)
  libTest <- try(library(pkg,character.only=T),silent=T)
  if(class(libTest)=='try-error'){
    updteTest <- try(install.packages(pkg))
    if(class(updteTest)=='try-error'){update.packages(pkg)}
    else{install.packages(pkg)}
    print(pkg)
    library(pkg,character.only=T)
  }
}




# Color ramp for plotting
BrBG <- colorRampPalette(c('#543005','#8c510a','#bf812d','#dfc27d','#f6e8c3','#f5f5f5','#c7eae5','#80cdc1','#35978f','#01665e','#003c30'))
elevRamp <- colorRampPalette(c('#aff0e9','#ffffb3','#008040','#fcba03','#780000','#69300d','#ababab','#fffcff'))
ScoRusRamp <- colorRampPalette(c('#2346c7','#ffffb3','#008040','#fcba03','#780000','#69300d', '#fe7c97', '#680459'))


#fix working directory:
setwd(choose.dir(default = "", caption = "Select a working directory"))


# select specific patterns of the h0X.tif file, we want the values for april to september, so look for ho[apr-sep].tif
fn <- paste(getwd(),'raster',sep='/')
fns <- list.files(fn,pattern='h0[4-9].tif$',full.names=T);fns

# import shapefiles for the plotting
f.bdy <- file.path(paste(getwd(),'shp','Europe.shp',sep='/'))
shp.bdy <- readOGR(f.bdy,p4s = proj.wgs,encoding = 'latin1',use_iconv = T)

f.water <- file.path(paste(getwd(),'shp','ne_10m_ocean.shp',sep = '/'))
shp.water <- readOGR(f.water,p4s = proj.wgs,encoding = 'latin1',use_iconv = T)

f.land <- file.path(paste(getwd(),'shp','ne_10m_land.shp',sep= '/'))
shp.land <- readOGR(f.land,p4s = proj.wgs,encoding = 'latin1',use_iconv = T)

plot(shp.water,col='steelblue',border='NA')
plot(shp.land,col='#543005',border='NA',add=T)
plot(shp.bdy,col='NA',border='black',add=T)



#import annual deuterium raster as base for the raster stacking
f.r <- file.path(paste(fn,'hma.tif',sep='/'))
s <- raster(f.r)
extent <- extent(-25,70,30,85)

plot(s,col=BrBG(255),ext=extent,xlim=c(-25,70),ylim=c(30,85))
plot(shp.bdy,col='NA',border='black',add=T)
plot(shp.water,col='grey10',border='NA',add=T)


# open each raster select in fns -> hma04, hma05, hma06, hma07, hma08, hma09
# and stack all in a rasterstack
for(f in fns){
  r <- raster(f)
  s <- stack(s,r)
}


# compute mean value for the hma between april and september
sm <- mean(raster::subset(s,c(2:7)))
s <- stack(s,sm)
names(s) <- c('hma','h04','h05','h06','h07','h08','h09','amjjas')


#plot(s,col=BrBG(255),ext=extent,xlim=c(-25,70),ylim=c(30,85))


# save annual hma layer and amjjas layer
r.ann <- s$hma
r.mn <- crop(s$amjjas,extent)


# open european forest cover
fn.forest <- file.path(paste(getwd(),'raster','forest.img',sep='/'))
r.forest <- raster(fn.forest)

# re-project raster to the same resolution and extent than je hma amjjas raster layer
r.forest <- projectRaster(from=r.forest,to=r.mn,res=res(r.mn),crs=CRS(proj.wgs),method='bilinear',filename=paste(getwd(),'raster','forestWGS.img',sep='/'),overwrite=T)

# select the forest cover greater than 20% for the latter analysis
r.forest[r.forest>100 | r.forest<20] <- NA


# plot for control
plot(r.mn,col=BrBG(255),ext=extent,xlim=c(-25,70),ylim=c(30,85))
plot(r.forest,add=T,col='grey20',legend=F)

# restrict the hma amjjas layer to the pixel with a forest cover > 20%
r.mn.frt <- mask(r.mn,r.forest)
plot(r.mn.frt,col=ScoRusRamp(255),ext=extent,xlim=c(-25,70),ylim=c(30,85))
plot(shp.water,col='black',border='NA',add=T)


jpegName <- paste(getwd(),'Graphs','Deuterium_Europe_AMJJAS.jpg',sep='/')
jpeg(jpegName,1200,1200,units = 'px',quality=100,pointsize=36)
plot(r.mn,col=BrBG(255),ext=extent,xlim=c(-25,70),ylim=c(30,85),legend=T)
#plot(r.mn.frt,col=elevRamp(255),ext=extent,xlim=c(-25,70),ylim=c(30,85),legend=T)
plot(shp.bdy,col='NA',border='black',add=T)
plot(shp.water,col='black',border='NA',add=T)
subplot(plot(shp.bdy,col='transparent',border='black',xlim=c(4.5,14.5),ylim=c(43,50),axes=F,ann=F,add=F),'bottomright',size=c(3,3))
subplot(plot(r.mn,col=BrBG(255),xlim=c(4.5,14.5),ylim=c(43,50),alpha=0.65,legend=F,axes=F,ann=F,box=F),'bottomright',size=c(3,3))
dev.off()


# save the hma amjjas layer to a GTIFF format
rasterName <- paste(getwd(),'raster','h049.tif',sep='/')
writeRaster(r.mn,rasterName,format='GTiff',overwrite=T)


# open isopote data frame, and show control values
dtf <- read.csv('isoscape_2013_2018.csv',sep=';',header=T)
summary(dtf)
str(dtf)
dim(dtf)


# concatenate values for the group selection in the raster analysis section
dtf$source_ID <- paste(dtf$AGE,dtf$t_PRELE,sep='_')
dtf$ORIG_ID <- paste(dtf$AGE,dtf$ORIGINE,sep='_')
dtf$jgd_ID <- paste(dtf$KT,dtf$t_PRELE,sep='_')

colKeep <- c('sp','t_PRELE','lon','lat','cz','ID','AGE','dH_correct','dH_reg','ORIGINE','KT', 'source_ID','ORIG_ID','jgd_ID')
dt <- dtf[,colKeep]
head(dt)


# show first line for control
t(dt[1,])
head(dt)


# compute the bin value as the nearest integer value of dH_reg/10, save in the column bin.reg
# prepare the reclassification matrix
fr <- seq(-16.5, -1.5,1)
to <- seq(-15.5,-0.5,1)
be <- seq(-16,-1,1)
rcl <- data.frame(from=fr, to=to,becomes = abs(be))
dt$bin.reg <- NA

# loop through each line of the reclassification matrix, select the values between the intervals fr - to
# and replace the values in the dt$bin.reg column
for(i in 1:length(be)){
  dt[dt$dH_reg/10>rcl$from[i] & dt$dH_reg/10<rcl$to[i],'bin.reg'] <- rcl$becomes[i]}

dt[1:10,c('dH_reg','bin.reg')]

# comput proportion of the data in each bin, for control
dtf.bin <- as.data.frame(table(dt$bin.reg))
dtf.bin$Prop <- round((dtf.bin$Freq/sum(dtf.bin$Freq))*100,2)
dtf.bin$CumSum <- cumsum(dtf.bin$Prop)
dtf.bin


# create a vector of the groups
slct <- c('ScoRus','a','j','a_jgd','j_jgd','a_CH','j_CH','TI','CH','jgd')


# 2 loops:
for(d in slct){
  #Subset data with a given category (d)
  print(d)
  # find in which column is found the value d
  c <- unique(names(dt)[which(dt == d, arr.ind=T)[, "col"]])
  # find which row has data with the group d in the column c
  i <- which(dt[,c]==d)
  
  # select hunt data not in TI, to do MANUALLY
  # i <- which(dt[,'KT']!='TI' & dt[,'t_PRELE']=='jgd') 
  # d <- 'jgd_NO_TI'
  
  # select data with the group d
  dt.slct <- dt[i,]
  n <- length(dt.slct$dH_reg)
  
  # prepare a proportion data frame of the data in each bins
  dtf.bin <- as.data.frame(table(dt.slct$bin.reg))
  dtf.bin$Var1 <- as.numeric(as.character(dtf.bin$Var1))
  
  # complete the bins sequence not to have any missing bin
  full <- seq(1,max(dtf.bin$Var1),1)
  # fill the dtf.bin dataframe with NA values for the missing bins
  dtf.bin <- data.frame('Var1'=full, 'Freq'=with(dtf.bin, dtf.bin$Freq[match(full, dtf.bin$Var1)]))
  
  # compute proportion of data in each bin, and the cumulative sum
  dtf.bin$Prop <- round((dtf.bin$Freq/sum(dtf.bin$Freq,na.rm=T))*100,2)
  dtf.bin$CumSum <- cumsum(ifelse(is.na(dtf.bin$Prop), 0, dtf.bin$Prop)) + dtf.bin$Prop*0
  
  # plot the proportion and cum sum of data in each bins
  pdfName <- paste(getwd(),'Graphs',paste('isotope','reg',d,'CumSum','bin_Proportion','pdf',sep='.'),sep='/')
  pdf(pdfName,paper='a4r',width=11,height=8.5)
  par(mfrow=c(1,1),mar=c(6,5,4,5))
  plot(dtf.bin$CumSum,type='b',cex=0.5,main=paste(d),ylim=c(0,100),ann=F,axe=F,col='sienna')
  mtext("Proportion cumulée [%]", side=2, line=3,cex=1.2,col="sienna")
  mtext(paste('Proportion of data in the bins, n =',length(i),sep=' '),side = 3, line = 1)
  box(col='grey')
  axis(2,cex.axis=1,col.axis='sienna',col='sienna')
  axis(1,at=c(min(index(dtf.bin$Var1)):max(index(dtf.bin$Var1))),
       labels=c(min(as.numeric(as.character(dtf.bin$Var1))):max(as.numeric(as.character(dtf.bin$Var1)))),
       las=3, cex.axis=1,col.axis='grey30',col='grey30')
  par(new=T)
  plot(dtf.bin$Prop,type='h',col='tomato',ann=F,axe=F)
  mtext("Proportion [%]", side=4, line=3,cex=1.2,col="tomato")
  axis(4,cex.axis=1,col.axis='tomato',col='tomato')
  dev.off()
  
  
  # get the 1st and 3rd quantile of dH_reg of the d group
  qtl.1 <- quantile(dt.slct$dH_reg,.25)
  qtl.3 <- quantile(dt.slct$dH_reg,.75)
  
  # find the rows where dH_reg is between the 1st and 3rd quantile
  i <- which(dt.slct$dH_reg<qtl.1 | dt.slct$dH_reg>qtl.3)
  
  # creat a new column for aggregating the 1-3 quantile values
  dt.slct[i,c('bin.reg','bin.agg')] <- 'NA'
  #dt.slct[-i,'bin.agg'] <- 'binAGG'
  dt.slct[i,'bin.agg'] <- 'binAGG'
  head(dt.slct)
  
  
  # create a vector with the bin and aggregated data
  slct.bin <- c('binAGG',sort(unique(dt.slct$bin.reg)))
  
  # reclassify the raster values with a probability based on the Mean +- 2*Standard deviation
  for(b in slct.bin){
    print(b)
    if(!(b=='NA')){
      # find in which column is found the bin b
      c <- unique(names(dt.slct)[which(dt.slct == b, arr.ind=T)[, "col"]])
      # find which row has data with the bin b in the column c
      i <- which(dt.slct[,c]==b)
      
      # compute mean value, standard deviation e and interval et of the dH_reg values in the bin b
      m <- mean(dt.slct[i,'dH_reg'])
      e <- sd(dt.slct[i,'dH_reg'])
      et <- 2*e
      
      # compute the proportion of datas used for this map (length(dt[i,'dH_reg'])) in the group d (n)
      propValue <- round((length(dt[i,'dH_reg'])/n)*100,2)
      
      
      # check if standard deviation is not NA, else pass to another bin
      if(!is.na(e)){
        # set minimal and maximal value for the reclassification matrix, if m+SD < min(value of the raster), 
        # the classification matrix will have crossed values
        if((m-et)<r.mn@data@min){minValue <- floor(r.mn@data@min)} else {minValue <- floor(m-et)}
        if((m+et)>r.mn@data@max){maxValue <- ceiling(r.mn@data@max)} else {maxValue <- floor(m+et)}
        
        # control interval values to pass to the next step
        if(minValue<(m+et) & maxValue>(m-et)){
          # print the bin, mean and sd value
          print(paste(b, m, e,sep='...-...'))
          
          # get the interval value for the reclassification matrix
          itl<-abs((minValue-maxValue)/253)
          
          # get initial values of the reclassification interval
          r.from <- seq(minValue,maxValue,itl)
          r.from <- c(floor(r.mn@data@min), r.from)
          
          # get final value of the reclassification interval
          r.to <- seq(minValue+itl,maxValue+itl,itl)
          r.to <- c(r.to, ceiling(r.mn@data@max))
          
          # get a probability vector rangin from 0 -- 1 -- 0 on a 255 steps. Many steps will give a finer color ramp on the raster
          r.becomes <- c(seq(from=0,to=0.999,by=1/127),1,rev(seq(from=0,to=0.999,by=1/127)))
          
          # check the length of each vector
          length(r.from);length(r.to);length(r.becomes)
          
          # make the reclassification matrix
          rcl <- data.frame(from=r.from,to=r.to ,becomes= r.becomes)
          rcl
          
          # reclassify the raster layer of hma amjjas masked with the forest using the reclassification matrix, save as GTIFF
          rasterName <- paste(getwd(),'raster',paste('ho49',d,b,'tif',sep='.'),sep='/')
          r <- reclassify(r.mn.frt,as.matrix(rcl),format='GTiff',filename=rasterName,overwrite=T)
          
          # set probability = 0 to NA for mapping
          r[r==0] <- NA
          
          # plot the raster and save as jpeg
          jpgName <- paste(getwd(),'Graphs',paste('isotope','reg','CHsubplot',d,'bin',b,'jpg',sep='.'),sep='/')
          jpeg(jpgName,1200,1200,units = 'px',quality=100,pointsize=36)
          
          # pdfName <- paste(getwd(),'Graphs',paste('isotope','reg',d,'bin',b,'pdf',sep='.'),sep='/')
          #pdf(pdfName,paper='a4r',width=11,height=8.5)
          
          # plot the reclassified - probability of origin of the woodcock, add countries and ocean for clarity and orientation
          plot(r,ext=extent,
               col=ScoRusRamp(255),
               xlim=c(-25,70),ylim=c(30,85)#,main=paste('Bin',b,propValue, '%','of',n,'values plotted',sep=' ')
          )
          plot(shp.bdy,col='NA',border='black',add=T)
          plot(shp.water,col='grey10',border='NA',add=T)
          subplot(plot(shp.bdy,col='transparent',border='black',xlim=c(5.8,10.6),ylim=c(45.7,47.9),axes=F,ann=F,add=F),'bottomright',size=c(3,3))
          subplot(plot(r,col=ScoRusRamp(255),xlim=c(5.8,10.6),ylim=c(45.7,47.9),legend=F,axes=F,ann=F),'bottomright',size=c(3,3))
          dev.off()

          
        }
      }
    }
  }
}


#####################################################################
#####################################################################
# Proportion of origin of woodcock
#####################################################################
#####################################################################

# MANUALLY SELECT THE GROUPS CH AND jgd and run each round once, 
# after the first round CH; save m, e, et and n in the ith.CH vector
itl.CH <- c(m,e,et,n)

# after the first round jgd; save m, e, et and n in the ith.jgd vector
itl.jgd <- c(m,e,et,n)

# convert to a dataframe, and bind both vectors
itl.CH <- as.data.frame(t(data.frame(itl.CH)))
itl.jgd <- as.data.frame(t(data.frame(itl.jgd)))
names(itl.jgd) <- names(itl.CH)
itl <- rbind(itl.CH,itl.jgd)
rownames(itl) <- c('CH','jgd')


# compute a reclassification matrix using the mean sd and interval to categorize the origins
fr <- c(itl['CH','m']-itl['CH','et'],
        floor(min(dt$dH_reg)),
        itl['jgd','m']+itl['jgd','et'],
        itl['CH','m']+itl['CH','et'])
to <- c(itl['CH','m']+itl['CH','et'],
        itl['jgd','m']+itl['jgd','et'],
        itl['CH','m']-itl['CH','et'],
        ceiling(max(dt$dH_reg)))
be <- c('CH','SC','CH_SC','wEU')
rcl <- data.frame(from=fr, to=to,becomes = be);rcl
dt$orig <- NA

# loop through each line of the reclassification matrix, select the values between the intervals fr - to
# and replace the values in the dt$orig column

for(i in 1:length(be)){
  dt[dt$dH_reg>rcl$from[i] & dt$dH_reg<rcl$to[i],'orig'] <- as.character(rcl$becomes[i])
}


# plot each origin for control
i <- which(dt$orig=='CH')
plot(dt[i,'dH_reg'],col='tomato',cex=0.5,ylim=c(-135,0))

i <- which(dt$orig=='SC')
points(dt[i,'dH_reg'],col='steelblue',cex=0.5)

i <- which(dt$orig=='CH_SC')
points(dt[i,'dH_reg'],col='orange',cex=0.5)

i <- which(dt$orig=='wEU')
points(dt[i,'dH_reg'],col='limegreen',cex=0.5)

boxplot(dt$dH_reg~dt$orig)

# get cross table (frequency for each value of dt$orig) and save it to a dataframe
dtf.orig <- as.data.frame(table(dt$orig))
dtf.orig$Prop <- round((dtf.orig$Freq/sum(dtf.orig$Freq))*100,2)
dtf.orig$CumSum <- cumsum(dtf.orig$Prop)
dtf.orig
write.csv(dtf.orig,'prob_orig_ScoRus.csv')
