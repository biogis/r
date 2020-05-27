#########################################################
#########################################################
# © eRey.ch | bioGIS; erey@biogis.ch
# created on 2019.11.01

# Spatial capture-recapture analysis on a woodcock project
# info fauna CSCF & KARCH - @ Thierry Bohnenstengel

#########################################################
#########################################################


#fix timezone on Bern
Sys.setenv(TZ='Europe/Paris')
# clear all objects saved in the R session
rm(list=ls())

#get proj4 for LV03 or LV95 for any projection map or spatial analysis requireng a projection

#LV03
proj <- '+proj=somerc +lat_0=46.95240555555556 +lon_0=7.439583333333333 +k_0=1 +x_0=600000 +y_0=200000 +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs '

#LV95
#proj <- '+proj=somerc +lat_0=46.95240555555556 +lon_0=7.439583333333333 +k_0=1 +x_0=2600000 +y_0=1200000 +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs'


# Library list
packages <- c(
  #CMR libraries
  'openCR','mra','Rcapture','multimark','secr',
  
  #habitat selection libraries
  'ade4','adehabitatLT','adehabitatHR','adehabitatHS','adehabitatMA',
  
  #stat libraries
  'lattice','permute','reshape','zoo','miscTools','data.table',
  
  #Spatial libraries
  'foreign','sp','plotrix','raster','maps','proj4','rgeos',
  'maptools','rgdal','spatial','Rcpp',
  
  #Looping libraries
  'foreach','iterators','parallel','doParallel',
  
  #graphics libraries
  'ggplot2','RColorBrewer','jpeg','png',
  
  #R libraries
  'devtools','telegram.bot','rlist'
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



#fix working directory:
setwd(choose.dir(default = "", caption = "Select a working directory"))

# open and import datafiles and convert to .txt files
edf <- read.csv(paste(getwd(),'spatial_CMR/edf_2016_2018_nid_migr.csv',sep='/'),sep=';', header = T, fileEncoding = 'latin1')
tdf <- read.csv(paste(getwd(),'spatial_CMR/tdf.csv',sep='/'),sep=';', header = T, fileEncoding = 'latin1')
dtel <- read.csv(paste(getwd(),'spatial_CMR/dtel.csv',sep='/'),sep=';', header = T, fileEncoding = 'latin1')
head(edf);head(tdf);head(dtel)
dim(edf);dim(tdf);dim(dtel)



#read and save the data in a secr readable data format 
# 1-. csv with real ',' as separator, 
# 2-.txt file with tab as separator)
write.csv(edf,paste(getwd(),'spatial_CMR/edf.csv',sep='/'),row.names=F)
write.table(edf,paste(getwd(),'spatial_CMR/edf.txt',sep='/'),row.names=F)

write.csv(tdf,paste(getwd(),'spatial_CMR/tdf.csv',sep='/'),row.names=F)
write.table(tdf,paste(getwd(),'spatial_CMR/tdf.txt',sep='/'),row.names=F)

write.csv(dtel,paste(getwd(),'spatial_CMR/dtel.csv',sep='/'),row.names=F)
write.table(dtel,paste(getwd(),'spatial_CMR/dtel.txt',sep='/'),row.names=F)

# create a vector of file names for captfile and trapfile
# path to capture-recapture datafile
captfile <- paste(getwd(),'spatial_CMR/edf.txt',sep='/') # link to capture file

# path to trap datafile, it is 6* the same as we have 3 years sampling with 2 sessions per year
trapfile <- c(paste(getwd(),'spatial_CMR/tdf.txt',sep='/'), # link to trapfile for 2016.a -. april - august (same for all years)
              paste(getwd(),'spatial_CMR/tdf.txt',sep='/'), # link to trapfile for 2016.b -. september - november (same for all years)
              paste(getwd(),'spatial_CMR/tdf.txt',sep='/'), # link to trapfile for 2017.a -. april - august (same for all years)
              paste(getwd(),'spatial_CMR/tdf.txt',sep='/'), # link to trapfile for 2017.b -. september - november  (same for all years)
              paste(getwd(),'spatial_CMR/tdf.txt',sep='/'), # link to trapfile for 2018.a -. april - august (same for all years)
              paste(getwd(),'spatial_CMR/tdf.txt',sep='/')  # link to trapfile for 2018.b -. september - november  (same for all years)
)

# path to dtel datafile
dtelfile <- paste(getwd(),'spatial_CMR/dtel.txt',sep='/')


#open shapefile for the mask, convex hull of dcap-dtel-CH data with a 2 km buffer
fn <- file.path(paste(getwd(),'Boundary/ConvexHull_DCAP_DTEL_TRAP_Buffer2km.shp',sep='/'))
shp <- readOGR(fn,p4s = proj,encoding = 'UTF-8',use_iconv = T);print(shp)

plot(shp,col='limegreen',border='darkgreen')


# import data for Spatial-Capture-Recapture analysis
# Detector Description:
# proximity:  records presence at a point without restricting movement
# count:      proximity detector allowing >1 detection per animal per time
# telemetry:  locations from radiotelemetry

# import capture-recaoture data
captdata <- read.capthist(captfile = captfile, 
                          trapfile = trapfile,
                          covnames = NULL,
                          trapcovnames = NULL,
                          detector = 'count',
                          fmt='trapID',
                          skip=1)


# import trap data
trapdata <- read.traps(trapfile,detector = 'count',skip=1)


# import telemetry data
dteldata <- read.telemetry(file = dtelfile,skip=1,covnames = NULL)

summary(dteldata)

#combine telemetry data, not used for the models
capt.dtel.data <-MS.capthist(captdata, dteldata,renumber = FALSE)
# capt.dtel.data <-addTelemetry(captdata, dteldata,type = 'dependent',collapsetelemetry = T)



# Optional: save data imported
#write.capthist(captdata)


########### Data exploration ###########

# for summary explanation:
# n number of distinct individuals detected on each occasion t
# u number of individuals detected for the first time on each occasion t
# f number of individuals detected on exactly t occasions
# M(t+1) cumulative number of detected individuals on each occasion t

summary(captdata)
summary(dteldata)
summary(capt.dtel.data)



# Plot the datas, grid of CMR data and histogram of distance from capture location and save in a pdf
pdfName <- paste(getwd(),'Graphs',paste('ScoRus','SCR','DatExplor','dtel','pdf',sep='.'),sep='/')
pdf(pdfName,paper='a4r',width=11,height=8.5)

par(mar=c(1,1,3,1),mfrow=c(3,2))
plot(captdata,gridsp=10000,track=T)

m <-unlist(moves(captdata))
par(mar =c(3.2,4,1,1),mgp =c(2.1,0.6,0),mfrow=c(1,1))# reduce margins
m.hist <- hist(m/1000,breaks=seq(0,50,1),plot=F);print(m.hist$density*100)
hist(m/1000,breaks=seq(0,50,1),col='steelblue',border='steelblue4',xlab ="Mouvements en km",main ="",plot=T)
dev.off()




#RPSV (for Root Pooled Spatial Variance) is a measure of the 2-D dispersion 
#of the locations at which individual animals are detected, pooled over individuals 
#(cf Calhoun and Casby 1958, Slade and Swihart 1983).
#get values from capt history file and dtel file
initialsigma.dcap <-RPSV(captdata,CC =TRUE);initialsigma.dcap
initialsigma.dtel <-RPSV(dteldata,CC =TRUE);initialsigma.dtel

dtf.move <- t(cbind(as.data.frame(initialsigma.dcap),as.data.frame(initialsigma.dtel)))
for(inisig in dtf.move){cat("Quick and biased estimate of sigma =", inisig/1000,"km\n")}

# define the buffer around the recapture for analysis
# As a rule of thumb, a buffer of 4 sigma HN is likely to be adequate (result in truncation bias of less than 0.1%). 
# A pilot estimate of sigma HN may be found for a particular dataset (capthist object) with the function
# RPSV with the argument CC set to TRUE:
bfr.4 <- 4*median(dtf.move[,1])

#choose a arger uffer width to detect detection tail
bfr.6 <- 6*median(dtf.move[,1])

#choose a arger uffer width to detect detection tail
bfr.10 <- 10*median(dtf.move[,1])

print(paste(round(bfr.4/1000,3),'km',sep=' '))
print(paste(round(bfr.6/1000,3),'km',sep=' '))
print(paste(round(bfr.10/1000,3),'km',sep=' '))

# Exclusion of non-habitat (Fig. 7b,c) is achieved by providing make.mask with a digital map of the habitat
# in the poly argument. The digital map may be a SpatialPolygons or SpatialPolygonsDataFrame
# clippedMask <-make.mask(traps(capt.dtel.data), type ='polygon',buffer = bfr.10, poly = shp, cell.overlap = "any")
clippedMask <-make.mask(traps(captdata), type ='polygon',buffer = bfr.10, poly = shp, cell.overlap = "any")

pdfName <- paste(getwd(),'Graphs',paste('ScoRus','SCR', 'MASK','DatExplor','.dtel','pdf',sep='.'),sep='/')
pdf(pdfName,paper='a4r',width=11,height=8.5)
plot(clippedMask)
dev.off()


#Model argument and learned response:
#b   permanent global learned response
#bk  permanent detector-specific learned response

# detection function relates the probability of detection g or 
# the expected number of detections ?? for an animal 
# to the distance of a detector from a point usually thought of as its home-range centre

# HN   	halfnormal
# HHN 	hazard halfnormal
# EX 	  exponential
# HEX 	hazard exponential
# HR 	  hazard rate
# HHR 	hazard hazard rate

fit.HNbk <- secr.fit(captdata,
                     mask = clippedMask, 
                     buffer=bfr.10, 
                     detectfn = 'HN', 
                     trace = T, 
                     model = list(D~Session,g0 ~ bk)
)

fit.HHNbk <- secr.fit(captdata,
                      mask = clippedMask, 
                      buffer=bfr.10, 
                      detectfn = 'HHN', 
                      trace = T, 
                      model = list(D~Session,g0 ~ bk)
)

fit.EXbk <- secr.fit(captdata,
                     mask = clippedMask, 
                     buffer=bfr.10, 
                     detectfn = 'EX', 
                     trace = T, 
                     model = list(D~Session,g0 ~ bk)
)

fit.HEXbk <- secr.fit(captdata,
                      mask = clippedMask, 
                      buffer=bfr.10, 
                      detectfn = 'HEX', 
                      trace = T, 
                      model = list(D~Session,g0 ~ bk)
)

fit.HRbk <- secr.fit(captdata,
                     mask = clippedMask, 
                     buffer=bfr.10, 
                     detectfn = 'HR', 
                     trace = T, 
                     model = list(D~Session,g0 ~ bk)
)

fit.HHRbk <- secr.fit(captdata,
                      mask = clippedMask, 
                      buffer=bfr.10, 
                      detectfn = 'HHR', 
                      trace = T, 
                      model = list(D~Session,g0 ~ bk)
)


#create a secr list with all detection functions modelling
fits <- secrlist(HN = fit.HNbk,
                 HHN = fit.HHNbk, 
                 EX = fit.EXbk, 
                 HEX = fit.HEXbk, 
                 HR = fit.HRbk,  
                 HHR = fit.HHRbk)

# get the density estimate (predict) and the Akaike's Criterion
prd.fits <- predict(fits);prd.fits
aic.fits <- AIC(fits);aic.fits

# get the density estimates for each model
D <- collate(fits,realnames='D')[,1:6,1:4,]
write.csv(as.data.frame(D),'DensityEstimate.csv',row.names = T)

#One way to duck the problem of selecting a single model is 
#to average over the models using AIC model weights. There is a function for this:

result.H <- model.average(secrlist(HN = fit.HNbk, 
                                   EX = fit.EXbk, 
                                   HR = fit.HRbk))

result.HH <- model.average(secrlist(HHN = fit.HHNbk,
                                    HEX = fit.HEXbk,
                                    HHR = fit.HHRbk))

list.save(list(result.H,result.HH), 'model.average.fits.json')

# convert results to a dataframe
result.H <- as.data.frame(result.H[,,'D'])
result.H$avType <- 'HNbk - EXbk - HRbk'

# convert results to a dataframe
result.HH <- as.data.frame(result.HH[,,'D'])
result.HH$avType <- 'HHNbk - HEXbk - HHRbk'

# combine both data frames and save it
result <- rbind(result.H,result.HH)

write.csv(result,'model.average.fits.Dvalue.csv')

# save predict values and AIC values as txt file and as json file
write.table(prd.fits,'prd.fits.txt')
write.table(aic.fits,'aic.fits.txt')

list.save(prd.fits, 'prd.fits.modelSessionBK.json')
list.save(aic.fits, 'aic.fits.modelSessionBK.json')


