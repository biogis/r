#########################################################
#########################################################
# © eRey.ch | bioGIS; erey@biogis.ch
# created on 2018.06.18
# modified on 2021.11.08
#source('./batFunc.r')
# https://github.com/biogis/r/blob/master/batFunc.r

###########################################################
###########################################################

require(tuneR)
require(seewave)
require(data.table)
require(matrixStats)
require(ggplot2)
require(ggpubr)
require(raster)


jet.colors <- colorRampPalette(c('#371450','#00007F','#0000ff','#007FFF','#00ffff','#7FFF7F','#ffff00','#FF7F00','#ff0000','#7F0000'))
BrBG <- colorRampPalette(c('#543005','#8c510a','#bf812d','#dfc27d','#f6e8c3','#f5f5f5','#c7eae5','#80cdc1','#35978f','#01665e','#003c30'))
cbbPalette <- colorRampPalette(c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))
spectral.colors <- colorRampPalette(c('red3','darkblue','gold2','limegreen','green4'))
discret <- colorRampPalette(c('tomato','steelblue','gold','limegreen','darkorchid3'))
cb <- colorRampPalette('#E69F00', '#56B4E9', '#009E73', '#F0E442', '#0072B2', '#D55E00', '#CC79A7', '#000000')
elevRamp <- colorRampPalette(c('#aff0e9','#ffffb3','#008040','#fcba03','#780000','#69300d','#ababab','#fffcff'))
ScoRusRamp <- colorRampPalette(c('#2346c7','#ffffb3','#008040','#fcba03','#780000','#69300d', '#fe7c97', '#680459'))



#https://github.com/stas-g/findPeaks
bat_pks <- function (x, m = 3){
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



bat <- function(f, wl=1024, ovl=50, wn='hamming', zp=64, fr=8000, to=150000){
  a <- try(readWave(f),silent=T)
  if(class(a)!='try-error'){
    if(length(a@left)>0){
      
      # Open file and extract fft matrix
      # tr <- strsplit(basename(f),"_")
      # asofdate <- strptime(paste(tr[[1]][5], tr[[1]][6], sep=' '), format="%Y%m%d %H%M%S", tz='') # paste and format date and time in a single vector
      asofdate <- 'x'
      
      a <- readWave(f)
      # str(a)
      wl <- 1024
      ovlp <- 50
      wl.half <- wl/2
      fH <- a@samp.rate
      fH.half <- fH/2
      dur <- length(a@left)/fH
      wn <- 'hamming'
      zp <- 64
      fr <- 8000
      to=150000
      
      a.filt <- fir(a, from = fr, to = to)
      dt <- seewave::spectro(a.filt, f=fH, wn=wn, ovlp=ovlp, wl=wl, zp=zp, plot=F)#compute fft of the wav file
      # str(dt)
      dtf <- as.data.table(dt$amp)
      
      
      row <- dt$freq # prepare new rownames with frequencies
      col <- dt$time # prepare new colnames with time
      length(row); length(col)
      rownames(dt$amp) <- row # rename rows with frequencies values
      colnames(dt$amp) <- col # rename columns with time values
      
      dt$amp[1:10,1:10] # check matrix
      return(dt)
    }
  } else cat('This is not a sound file')
}


# dt <- bat(f)
# str(dt)
      

cleanBat <- function(dt){
      #Signal Finder, for each frequency band, substract the 50% quantile and select the values > than 10% above the mean, else set as NA
  row <- dt$freq # prepare new rownames with frequencies
  col <- dt$time # prepare new colnames with time
  rms.tot.unfilt <- -(sqrt(mean(dt$amp^2,na.rm=T)));rms.tot.unfilt
      
  # qtl <- 0.5 # Define quantile in %
    
  system.time(rowmea  <-  rowMeans(dt$amp, na.rm=T))
  system.time(rowmed  <-  apply(dt$amp, 1, median, na.rm = T))
  # system.time(rowQtl  <-  rowQuantiles(dt$amp, probs = qtl, na.rm = T))
  system.time(colmea  <-  colMeans(dt$amp, na.rm=T))
  system.time(colmed  <-  apply(dt$amp, 2, median, na.rm = T))
  # system.time(colQtl  <-  colQuantiles(dt$amp, probs = qtl, na.rm = T))
  
  
  system.time(
    #select data for every row, aka freq bin, or freq line
    for(frq in 1:dim(dt$amp)[1]){
      dt$amp[frq,which(dt$amp[frq,]<rowmed[frq])] <- NA # substract 5% quantile to avoid specific freq bin noise floor
      mea <- mean(dt$amp[frq,],na.rm=T)
      mea.10 <- mea-(mea/10)
      dt$amp[frq,which(dt$amp[frq,] < mea.10)] <- NA # denoise freq line
    })
  
  
  system.time(
    #select data for every column, aka time bin, or time column
    for(tm in 1:dim(dt$amp)[2]){
      dt$amp[which(dt$amp[,tm]<colmed[tm]),tm] <- NA
      mea <- mean(dt$amp[,tm],na.rm=T)
      mea.10 <- mea-(mea/10)
      dt$amp[which(dt$amp[,tm] < mea.10),tm] <- NA
    })
  return(dt)
}


# dt <- cleanBat(dt)


statBat <- function(dt){
  row <- dt$freq # prepare new rownames with frequencies
  col <- dt$time # prepare new colnames with time
  
  mean.mod <- rowMeans(dt$amp,na.rm=T)
  specSum <- colSums(dt$amp,na.rm=T)
  peakBin <- colMaxs(dt$amp,na.rm=T)
  
  mean.mod[is.na(mean.mod)] <- min(mean.mod,na.rm=T)
  peakBin[is.infinite(peakBin)] <- NA
  
  dtf.freq <- data.frame('Time'=col,peakBin,specSum);summary(dtf.freq)
  
  ## the 'lm'
  tl <- lm(peakBin ~ specSum)
  data.tl <- summary(tl);data.tl
  slp <- format(as.numeric(coef(tl)[2]), digits = 2);slp       ## extracting slope:
  R2 <- paste0(format(data.tl$r.squared, digits = 2), "%");R2
  
  rms.tot.filt <- -(sqrt(mean(dt$amp^2,na.rm=T)));rms.tot.filt
  
  pwr <- data.frame('freq'= row,'amp'=mean.mod) #dataframe for power spectrum
  
  pwr.bat <- pwr[which(pwr$freq>10 & pwr$freq<60 | # Look for any bat in freq range 10 to 60 kHz
                         pwr$freq>75 & pwr$freq<85 | # Look for rFe in freq range 75 to 85 kHz
                         pwr$freq>100 & pwr$freq<110),] # Look fir rHi in freq range 100 to 110 kHz
  pks.i <- bat_pks(pwr.bat$amp,15) # find index with peaks values
  pks <- subset(pwr.bat[pks.i,])#;print(pks) # subset peaks frequencies compared to rms value
  pks
  
  
  if(length(pks$freq)>0 & slp<0){
    i <- which(dt$freq<10 |
                 dt$freq > 60 & dt$freq <75 |
                 dt$freq > 85 & dt$freq < 100 |
                 dt$freq > 120) # low and high pass filter
    pks$rms <- -(sqrt(mean(pks$amp^2,na.rm=T)));pks
    # i <- which(dt$freq>10 & dt$freq<60 | # Look for any bat in freq range 10 to 60 kHz
    #              dt$freq>75 & dt$freq<85 | # Look for rFe in freq range 75 to 85 kHz
    #              dt$freq>100 & dt$freq<110) # Look fir rHi in freq range 100 to 110 kHz
    
    nse <- median(dt$amp[i,],na.rm=T)
    pks$snr_nse <- (pks$amp/nse)^2
    pks[which(is.na(pks$snr_nse)),'snr_nse'] <- 9999
    rms.tot.filt
    pks$snr_rms <- pks$amp/rms.tot.filt
    pks[which(is.na(pks$snr_rms)),'snr_rms'] <- 9999
    pks} else if (length(pks$freq)>0 & slp<=0) {
      pks$snr_nse <- NA
      pks$snr_rms <- NA
    } else if (is.null(pks$freq)){
      pks <- data.frame('freq'=NA,
                        'amp'=NA,
                        'rms'=NA,
                        'snr_nse'=NA,
                        'snr_rms'=NA)
    }
  
  resultList <- list('mean.mod' = mean.mod,
                     'specSum' = specSum, 
                     'peakBin' = peakBin,
                     'dtf.freq' = dtf.freq,
                     'pwr' = pwr,
                     'pks' = pks,
                     'slp' = slp)
  return(resultList)
}


# L <- statBat(dt)
# str(L)
# 
# L$pks$snr_nse
# 
  
#### ^.^ #### ^.^ #### ^.^ #### ^.^ ####
# bat

batFinder <- function(f,L, output){
  if(any((L$pks$snr_nse) > 0.6 | (L$pks$snr_rms) > 0.6)){
    output <- paste(out.dir,basename(f),sep='/')
    file.copy(f,output)
  # chiro.dtf <- data.frame('fileName' = output,
  #                         'freq' = pks$freq,
  #                         'Date' = asofdate,
  #                         'FullName' = basename(f),
  #                         'SNR SNE' = pks$snr_nse,
  #                         'SNR RMS' = pks$snr_rms,
  #                         'RMS' = rms.tot.filt,
  #                         'Slope' = slp,
  #                         'R2' = R2,
  #                         'median' = median(dt$amp,na.rm=T)
  # )
  
  # result <- rbind(result, chiro.dtf)
  # result.all <- rbind(result.all, chiro.dtf)
  
  # print(paste(y,': chiro found at',pks$freq, 'in', basename(f), sep=' '))
  print(paste('chiro found in', basename(f), sep=' '))
  }
}




batlab <- function(f){
  a <- try(readWave(f),silent=T)
  if(class(a)!='try-error'){
    if(length(a@left)>0){
      a <- readWave(f)
      fH <- a@samp.rate
      fH.half <- fH/2
      lab10 <- floor(seq(0,fH.half,by=5000)/1000)
      brks <- seq(0, fH.half, by=2000)
      dur <- length(a@left)/fH
      print(fH)
      ticks <- list('lab10' = lab10, 
                    'brks' = brks, 
                    'dur' = dur)
      return(ticks)
    }
  } else cat('This is not a sound file')
}

# tk <- batlab(f)

      

# plot control graph
################################

control <- function(f, dt, L, pathName){
    freqPeak <- c()
    for(j in 1:dim(dt$amp)[2]){
      mxm <- max(dt$amp[,j],na.rm=T)
      idx <- which(dt$amp[,j]==mxm)
      fpk <- ifelse(is.infinite(mxm)==T,NA,rownames(dt$amp)[idx])
      freqPeak <- c(freqPeak,as.numeric(fpk))
      }
    pathName <- file.path(getwd())
    pdfname <- paste(pathName,paste(paste(sub("(.+)[.][^.]+$", "\\1", basename(f)),sep='_'),'pdf',sep='.'),sep='/')
            
    pdf(pdfname,paper='a4r',width=11,height=8.5)
    
    par(mfrow=c(1,1),mar=c(6,5,4,5))
    

    lab10 <- batlab(f)

    pp <- ggplot(L$dtf.freq, aes(x = specSum, y = peakBin))+
            geom_point(aes(color = freqPeak),
                       alpha = 0.7,
                       size = 1.5,
                       na.rm=T) +
            scale_color_gradient2(name="freqPeak",
                                  breaks = c(0, 50, 100),
                                  labels = c("0 kHz", "50 kHz", "100 kHz"),
                                  low = "tomato",
                                  high = "steelblue",
                                  mid = "gray60",
                                  midpoint = 50)+
            theme_bw(base_size = 10)+
            theme(axis.line = element_line(colour = 'grey80'),
                  panel.border = element_blank(),
                  panel.background = element_blank(),
                  axis.title.x=element_text(colour='grey50'),
                  axis.title.y=element_text(colour='grey50')
            )+
            geom_smooth(mapping = aes(linetype = "r2"),method = "lm",formula = y ~ x,se = T,na.rm=T,color='tomato')+
            scale_linetype(name = "",
                           breaks = "r2",
                           labels = list(bquote(slope==.(L$slp))),
                           guide = guide_legend(override.aes = list(linetype = 1, size = 2, color = "red")))
          #
          print(pp)
          
          dev.off()
          return(pp)
          # #
}

# pp <- control(f, dt, L)
        

# # # PLOT Spectrogram
# # ######################
# #

graph <- function(f, dt, L, pp, pathName){
  tk <- batlab(f)
  lab10 <- tk$lab10
  brks <- tk$brks
  dur <- tk$dur
  
  df <- as.data.frame(raster(dt$amp), xy=TRUE)
  head(df)
  df$layer.med <- df$layer-(median(df$layer,na.rm=T))
  rd.quartile <- as.numeric(summary(df$layer.med)[5])
  df$layer.med <- df$layer.med-rd.quartile
  df$layer.med[df$layer.med<0] <- 0
  df$layer.med[is.na(df$layer.med)] <- 0
  head(df)
  
  
  arg.tps <- list(at=seq(0,1,by=1/(floor(dur/0.1))), labels=seq(0,dur,by=0.1))
  arg.freq <- list(at=seq(-1,0,by=1/(length(lab10)-1)), labels=lab10, minor=seq(-1,0,by=1/(2*(length(lab10)-1))))
  
  
  sp <- ggplot(df) +
        geom_raster(aes(x, -y, fill=layer)) +
        scale_fill_gradientn(name="[dB]",colours=jet.colors(length(brks)),na.value='white')+
        theme_bw(base_size = 10)+
        scale_x_continuous(breaks=arg.tps$at, labels = arg.tps$labels,sec.axis = dup_axis(name=waiver()))+
        xlab('Temps [s]')+
        scale_y_continuous(breaks= arg.freq$at,labels=arg.freq$labels,minor_breaks = arg.freq$minor,sec.axis = dup_axis(name=waiver())) +
        ylab('FrÃ©quence [kHz]')+
        theme(legend.position="none")+
        theme(axis.text.x=element_text(colour='grey50',angle=0,hjust=1,vjust=1),
              axis.title.x=element_text(colour='grey50'))+
        theme(axis.text.y=element_text(colour='grey50'),
              axis.title.y=element_text(colour='grey50'))+
        theme(axis.line = element_line(colour = 'grey80'),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              axis.ticks = element_line(colour='grey80'),
              panel.background = element_blank())
  
  
  xplot <- ggplot(df, aes(x=x, y=layer.med))+
              geom_bar(stat="identity",colour='grey20')+
              geom_bar(data = df,aes(y=-layer.med), stat="identity",colour='grey20')+
              theme_bw(base_size = 8)+
              xlab('Temps [s]')+
              ylab('Ampl [dB]')+
              theme(axis.line = element_line(colour = 'grey80'),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_blank(),
                    panel.background = element_blank(),
                    axis.title.x=element_text(colour='grey50'),
                    axis.text.x=element_blank(),
                    axis.ticks.x=element_blank(),
                    axis.title.y=element_text(colour='grey50'),
                    axis.text.y=element_blank(),
                    axis.ticks.y=element_blank())+
              ggtitle(basename(f))

  
  yplot <- ggplot(L$pwr, aes(x=L$pwr$freq, y=L$pwr$amp))+
              geom_line()+
              geom_point(data=L$pks,aes(x=L$pks$freq,y=L$pks$amp),colour='tomato')+
              #  geom_text(data=pks,aes(label=freq),hjust=0.2, vjust=-0.5,colour='tomato')+
              theme_bw(base_size = 10)+
              xlab('Frequence [kHz]')+
              ylab('Ampl [dB]')+
              theme(axis.line = element_line(colour = 'grey80'),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_blank(),
                    panel.background = element_blank(),
                    axis.title.x=element_text(colour='grey50'),
                    axis.text.x=element_blank(),
                    axis.ticks.x=element_blank(),
                    axis.title.y=element_text(colour='grey50'),
                    axis.text.y=element_blank(),
                    axis.ticks.y=element_blank())+
                ggpubr::rotate()
  
  
  p <- ggarrange(xplot, pp, sp, yplot,
                 ncol = 2, nrow = 2,  align = "hv",
                 widths = c(6, 1), heights = c(1, 6),
                 common.legend = F)
  
  
  jpegName <- paste(pathName,paste(sub("(.+)[.][^.]+$", "\\1", basename(f)),'jpg',sep='.'),sep='/')
  jpeg(jpegName,bg='transparent',width=10500,height=3500,units='px',res=300)
    
  print(p)
        
  dev.off()
  return(p)
}

# graph(f, dt, pp)


