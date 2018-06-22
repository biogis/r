#########################################################
#########################################################
# © eRey.ch | bioGIS; erey@biogis.ch
# created on 2018.06.18
# modified on 2018.06.21
#source('./Bat_Filter_III.r')
# https://github.com/biogis/r/blob/master/Bat_Filter_III.r

# Set your working directory

# 1-. in.dir, select the directory with wav files
# 2-. out.dir select the directory where the wav files will be copied


###########################################################
###########################################################


start <- Sys.time();start

packages <- c(
  #stat libraries
  'lattice','zoo','data.table','openxlsx','quantmod','matrixStats',
  'foreign','sp','plotrix','raster','maps','proj4','rgeos',
  'grid','rworldmap','maptools','rgdal','spatial','plyr','dplyr',
  'ggmap','geosphere','rasterVis','latticeExtra','ggplot2','grid','gridExtra','ggpubr','directlabels','ggrepel',
  'RColorBrewer','extrafont','jpeg','png','seewave','tuneR','warbleR')


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

#https://github.com/stas-g/findPeaks
find_peaks <- function (x, m = 3){
  shape <- diff(sign(diff(x, na.pad = FALSE)))
  pks <- sapply(which(shape < 0), FUN = function(i){
    z <- i - m + 1
    z <- ifelse(z > 0, z, 1)
    w <- i + m + 1
    w <- ifelse(w < length(x), w, length(x))
    if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
  })
  pks <- unlist(pks)
  pks
}




jet.colors <- colorRampPalette(c('#371450','#00007F','#0000ff','#007FFF','#00ffff','#7FFF7F','#ffff00','#FF7F00','#ff0000','#7F0000'))
BrBG <- colorRampPalette(c('#543005','#8c510a','#bf812d','#dfc27d','#f6e8c3','#f5f5f5','#c7eae5','#80cdc1','#35978f','#01665e','#003c30'))
cbbPalette <- colorRampPalette(c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))
discret <- colorRampPalette(c('tomato','steelblue','gold','limegreen'))
spectral.colors <- colorRampPalette(c('red3','darkblue','gold2','limegreen','green4'))



#choose working directory with all wav files
in.dir <- choose.dir()

#choose copying directory
out.dir <- choose.dir()

setwd(out.dir)


fn.f <- file.path(in.dir)
fns <- list.files(fn.f,pattern='.WAV$',all.files=T,full.names=T,recursive=T,include.dirs=T) # store all wave files in the working directory
print(length(fns))



result <- read.csv(text='fileName,freq,Date,FullName,SNR,RMS')
result.all <- read.csv(text='fileName,freq,Date,FullName,SNR,RMS')
tt <- c()


a <- readWave(fns[1])
# str(a)
wl <- 1024
ovlp <- 0
wl.half <- wl/2
fH <- a@samp.rate
fH.half <- fH/2
dur <- length(a@left)/fH
rm(a)
#f <- fns[1]


for(f in fns){ start.1 <- Sys.time();start.1
  y <- which(f==fns)
  a <- try(readWave(f),silent=T)
  if(class(a)=='try-error' | length(a@left)>0){
    
    tr <- strsplit(basename(f),"_")
    seq.f <- tail(tr[[1]],n=1)
    x.f <- as.integer(as.hexmode(substr(basename(seq.f), 1, 8))) # get the integer version of the HEX code
    asofdate <- as.POSIXct(x.f, origin="1970-01-01") # convert unix EPOC time to UTC time
    x.convert <- strptime(asofdate, format="%Y-%m-%d %H:%M:%S") # paste and format date and time in a single vector
    Date.file <- format(x.convert, "%Y%m%d%H%M");Date.file # format date and time, add to the dtf.header
    
    dt <- stft.ext(f,verbose=F,ovlp=ovlp,wl=wl,dB = T,mean=T)#compute fft of the wav file
    # str(dt)
    
    row <- seq(0,fH.half,by=fH/wl)/1000 # prepare new rownames with frequencies
    col <- seq(0,dur,by=wl/fH)*1000 # prepare new colnames with time
    rownames(dt$amp) <- row[-length(row)] # rename rows with frequencies values
    colnames(dt$amp) <- col[-length(col)] # rename columns with time values
    
    dt$amp[1:10,1:10] # check matrix

    #Signal Finder, for each frequency band, substract the 10% quantile and select the values > than 10% above the mean, else set as NA
    rms.tot.unfilt <- -(sqrt(mean(dt$amp^2,na.rm=T)));rms.tot.unfilt
    
    #select data for every line, aka freq bin, or freq line
    for(i in 1:wl.half){
      dt$amp[i,which(dt$amp[i,]<quantile(dt$amp[i,],.1,na.rm=T))] <- NA # substract 5% quantile to avoid specific freq bin noise floor
      mea <- mean(dt$amp[i,],na.rm=T)
      dt$amp[i,which(dt$amp[i,]<(mea-(mea/10)))] <- NA # denoise freq line
      
            }

mean.mod <- rowMeans(dt$amp,na.rm=T)



    
    for(j in 1:dim(dt$amp)[2]){
      dt$amp[which(dt$amp[,j]<quantile(dt$amp[,j],.5,na.rm=T)),j] <- NA
      mea <- mean(dt$amp[,j],na.rm=T)
      dt$amp[which(dt$amp[,j]<(mea-(mea/10))),j] <- NA
      
    }

specSum <- colSums(dt$amp,na.rm=T)
peakBin <- colMaxs(dt$amp,na.rm=T)

    mean.mod[is.na(mean.mod)] <- min(mean.mod,na.rm=T)
    peakBin[is.infinite(peakBin)] <- NA
    
    dtf.freq <- data.frame('Time'=col[-length(col)],peakBin,specSum);summary(dtf.freq)
    
    
    ## the 'lm'
    tl <- lm(peakBin ~ specSum)
    data.tl <- summary(tl);data.tl
    slp <- format(as.numeric(coef(tl)[2]), digits = 2);slp       ## extracting slope:
    R2 <- paste0(format(data.tl$r.squared, digits = 2), "%");R2
    
    rms.tot.filt <- -(sqrt(mean(dt$amp^2,na.rm=T)));rms.tot.filt

    
    pwr <- data.frame('freq'= row[-length(row)],'amp'=mean.mod) #dataframe for power spectrum
    
    pwr.bat <- pwr[which(pwr$freq>11 & pwr$freq<60 | # Look for any bat in freq range 10 to 60 kHz
                           pwr$freq>75 & pwr$freq<85 | # Look for rFe in freq range 75 to 85 kHz
                           pwr$freq>100 & pwr$freq<110),] # Look fir rHi in freq range 100 to 110 kHz
    pks.i <- find_peaks(pwr.bat$amp,15) # find index with peaks values
    pks <- subset(pwr.bat[pks.i,])#;print(pks) # subset peaks frequencies compared to rms value
    
    pks
    

    
    if(length(pks$freq)>0 & slp<0){
      
      nse <- median(dt$amp,na.rm=T)
      pks$snr <- pks$amp/nse
      pks
      

      
      
      #### ^.^ #### ^.^ #### ^.^ #### ^.^ ####
      # bat
      
      if(any((pks$snr)>1)){
             # & rms.tot.filt>median(dt$amp,na.rm=T)
            
        
        
        output <- paste(out.dir,basename(f),sep='\\')
        
        
        file.copy(f,output)
        
        chiro.dtf <- data.frame('fileName' = output,
                                'freq' = pks$freq,
                                'Date' = asofdate,
                                'FullName' = basename(f),
                                'SNR' = pks$snr,
                                'RMS' = rms.tot.filt,
                                'median' = median(dt$amp,na.rm=T))
        
        result <- rbind(result, chiro.dtf)
        
        print(paste(y,': chiro found at',pks$freq, 'in', basename(f), sep=' '))
        }
    }
  }
  print(paste(y,':',Sys.time()-start.1,sep=' '))
  tt <- c(tt,Sys.time()-start.1)
  
  rm(output,pks,nse,pwr.bat,pwr,rms.tot.filt,r2,slp,data.tl,tl,dtf.freq,peakBin,mean.mod,
   specSum,rms.tot.unfilt,col,row,dt,Date.file,x.convert,asofdate,x.f,seq.f,tr,y,a)
}


print(' ')
print('-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.')
print(' ^Ö^ ^Ö^ ^Ö^ ^Ö^ ^Ö^ ^Ö^ ^Ö^ ^Ö^ ^Ö^ ^Ö^ ^Ö^')
print(' ')
end <- Sys.time()
print(end-start)

print(' ')
print(summary(tt))


write.csv(result,'filteredWAV.csv')



