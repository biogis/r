  # source('/Volumes/Data/PhD/Script/R/Template.r', chdir = TRUE)
  # 
  library('seewave')
  library('tuneR')
  library('warbleR')
  
  
  jet.colors <- colorRampPalette(c('#371450','#00007F','#0000ff','#007FFF','#00ffff','#7FFF7F','#ffff00','#FF7F00','#ff0000','#7F0000'))
  BrBG <- colorRampPalette(c('#543005','#8c510a','#bf812d','#dfc27d','#f6e8c3','#f5f5f5','#c7eae5','#80cdc1','#35978f','#01665e','#003c30'))
  cbbPalette <- colorRampPalette(c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))
  discret <- colorRampPalette(c('tomato','steelblue','gold','limegreen'))
  spectral.colors <- colorRampPalette(c('red3','darkblue','gold2','limegreen','green4'))
  
  
  setwd('g:/')
  pathName <- file.path('d:/Users/erey/Documents/RData')
  
  fn.f <- file.path('g:/')
  fns <- list.files(fn.f,pattern='.WAV$',all.files=T,full.names=T,recursive=T,include.dirs=T) # store all wave files in the working directory
  x <- as.integer(as.hexmode(substr(basename(fns), 1, 8))) # get the integer version of the HEX code
  DateTime <- as.POSIXct(x, origin="1970-01-01") # convert unix EPOC time to UTC time
  x.convert <- strptime(DateTime, format="%Y-%m-%d %H:%M:%S") # paste and format date and time in a single vector
  Date.file <- format(x.convert, "%Y%m%d%H%M");Date.file # format date and time, add to the dtf.header
  
  siteName <- paste('Fonderie8',Date.file[1],Date.file[length(Date.file)],sep='_');print(siteName)
  
  start <- Sys.time();start
  
  
  result <- read.csv(text='fileName,freq,Date,SNR')
  dt.freq <- read.csv(text='freq,amp')

  f <- fns[28]
  
  for(f in fns){
    # startSeq <- Sys.time()
    index <- which(f==fns)
    a <- try(readWave(f),silent=T)
    if(class(a)=='try-error' | length(a@left)>0){

      x.f <- as.integer(as.hexmode(substr(basename(f), 1, 8))) # get the integer version of the HEX code
      asofdate <- as.POSIXct(x.f, origin="1970-01-01") # convert unix EPOC time to UTC time
      
      
      ms <- meanspec(a,plot=F);dim(ms)
      dt.ms <- as.data.frame(ms); names(dt.ms) <- names(dt.freq)
      st.quartile <- as.numeric(summary(dt.ms[,2])[2])
      med <- as.numeric(summary(dt.ms[,2])[3])
      rd.quartile <- as.numeric(summary(dt.ms[,2])[5])
      dt.freq <- rbind(dt.freq,dt.ms)
      
      
      fp <- as.data.frame(fpeaks(ms,amp=c(0.1,0.1),freq=10000,threshold= rd.quartile,plot=F));fp
      fp.chiro <- subset(fp,fp$freq > 15)
      freq.peaks <- fp.chiro$freq
      SNR.chiro <- ifelse(dim(fp.chiro)[1]==0,NA,(rd.quartile/max(fp.chiro[,2],na.rm=T))*100)

      
      # #### ^.^ #### ^.^ #### ^.^ #### ^.^ ####
      # # No bat
      # if(length(freq.peaks)==0){
      # pdfname <- paste(pathName,'strange_pdf',paste(paste(siteName,sub("(.+)[.][^.]+$", "\\1", basename(f)),sep='_'),'pdf',sep='.'),sep='/')
      # pdf(pdfname,paper='a4r',width=11,height=8.5)
      # fpeaks(ms,amp=c(0.1,0.1),freq=10000,threshold= st.quartile)
      # dev.off()
      # }
        
        #### ^.^ #### ^.^ #### ^.^ #### ^.^ ####
        # bat
      if(any(freq.peaks>15)==T & SNR.chiro<26.7){
        pdfname <- paste(pathName,'pdf',paste(paste(siteName,sub("(.+)[.][^.]+$", "\\1", basename(f)),sep='_'),'pdf',sep='.'),sep='/')
        
        fpeaks(ms,amp=c(0.1,0.1),freq=10000,threshold= st.quartile)
        
        pdf(pdfname,paper='a4r',width=11,height=8.5)
        spectro(a,scale=F,flim=c(0,150),palette = jet.colors)
        fpeaks(ms,amp=c(0.1,0.1),freq=10000,threshold= st.quartile)
        dev.off()
        
        output <- paste(pathName,'wav',paste(siteName,basename(f),sep='_'),sep='/')
        file.copy(f,output)
        
        chiro.dtf <- data.frame('fileName' = output,
                              'freq' = freq.peaks,
                              'Date' = asofdate,
                              'FullName' = basename(f),
                              'SNR' = SNR.chiro)
        
        result <- rbind(result, chiro.dtf)
        
        print(paste(index,': chiro found at',freq.peaks, 'in', basename(f), sep=' '))
      }

    
    }
    
  }
  write.table(result,paste(pathName,'csv',paste('Bat',siteName,'csv',sep='.'),sep='/'),row.names=F,sep=';',dec=',')
  write.table(dt.freq,paste(pathName,'csv',paste('FreqLandscape',siteName,'csv',sep='.'),sep='/'),row.names=F,sep=';',dec=',')
  
  names(dt.freq) <- c('freq','amp')
  freq <- floor(dt.freq$freq)
  amp <- dt.freq$amp
  aggr<-aggregate(amp~freq,FUN="mean",na.rm=T)
  # aggr<-aggregate(dt.freq$amp~dt.freq$freq,FUN="sum",na.rm=T)
  mt <- as.matrix(aggr)
  
  fpeaks(mt,amp=c(0.05,0.05),freq=100000)
    
  end <- Sys.time()
  print(end-start)
  
