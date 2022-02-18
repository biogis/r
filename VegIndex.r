#########################################################
#########################################################
# © eRey.ch | bioGIS; erey@biogis.ch
# created on 2022.01.31
# modified on 2022.02.17
# https://github.com/biogis/r/blob/master/vegIndex.r
# url <- 'https://raw.githubusercontent.com/biogis/r/master/VegIndex.r'
# source('./vegIndex.r')
# source(url)
###########################################################
###########################################################


#fix timezone on Bern
Sys.setenv(TZ='Europe/Paris')
# clear all objects saved in the R session
rm(list=ls())


packages <- c(
  #Spatial libraries
  'sf','terra','spatial','leaflet',

  #graphics libraries
  'RColorBrewer','jpeg','png',

  #Looping libraries
  'foreach','iterators','parallel','doParallel',

  #R libraries
  'telegram.bot','tcltk','svDialogs'
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

# Choose directory function for all OS platforms; require tcltk package for macOS and linux
choose_dir  <-  function(caption = 'Select data directory') {
  if (exists('utils::choose.dir')) {
    choose.dir(caption = caption)
  } else {
    tk_choose.dir(caption = caption)
  }
}


# Color ramp for plotting
BrBG <- colorRampPalette(c('#543005','#8c510a','#bf812d','#dfc27d','#f6e8c3','#f5f5f5','#c7eae5','#80cdc1','#35978f','#01665e','#003c30'))
elevRamp <- colorRampPalette(c('#aff0e9','#ffffb3','#008040','#fcba03','#780000','#69300d','#ababab','#fffcff'))
ScoRusRamp <- colorRampPalette(c('#2346c7','#ffffb3','#008040','#fcba03','#780000','#69300d', '#fe7c97', '#680459'))



# Define how many cores you want to use
UseCores <- detectCores() - 5
# # UseCores <- 10

# Register CoreCluster
cl       <- makeCluster(UseCores, outfile="")
registerDoParallel(cl)


#############################################################################
############################# -- INPUT - PART -- ############################
#############################################################################

# Initiate the bot session using the token from the enviroment variable.
# bot <- Bot(token = 'YOUR.TOKEN.FROM.TELEGRAM.BOT.FATHER')

# choose working directory with all orthoimages
in.dir <- choose_dir(caption = "Select input tif folder")

# choose output directory
out.dir <- choose_dir(caption = "Select output tif folder")


#get CRS for a reprojection of the raster using the EPSG code. See on epsg.io for more informations
# CH1903_lv03: 21781
# CH1903_LV95: 2056
# wgs84: 4326
# Lambert 93: 2154

#If no reprojection is required, enter NA

epsg <- dlgInput("Enter an epsg number for reprojection\nor NA if no reprojection is required:\n# CH1903_lv03:\t21781\n# CH1903_LV95:\t2056\n# wgs84:\t4326", 2056)$res

#############################################################################
########################## -- LET IT RUN - PART -- ##########################
#############################################################################


cat('you selected as working directories:\n', '\tin.dir:\t\t', in.dir, '\n ', '\tout.dir:\t', out.dir, '\n')

setwd(in.dir)

# list all .tif files in your orthoimage directory
fns <- list.files(in.dir, patter='.tif$')#; print(fns)

cat('found the following images in the in.dir directory:\n',fns,sep='\n')

# Choose your orthoimage, this is the place to include a loop if several orthoimages have to be analysed:
i <- 3
f <- fns[i]


foreach(i=1:length(fns)) %dopar% {
  library(terra)
# for(i in 1:length(fns)){
  f <- fns[i]
  f.prj <- paste(sub("(.+)[.][^.]+$", "\\1", f), 'ch.tif', sep='_')
  g <- paste(sub("(.+)[.][^.]+$", "\\1", f), 'VegIndex.tif', sep='_')
  csvName <- paste(sub("(.+)[.][^.]+$", "\\1", f), 'VegIndex.csv', sep='_')
  cat('Working on:\n', '\tinput file:\t\t', f, '\n ', '\toutput File:\t\t', g, '\n','\treprojected file:\t',f.prj,'\n')
  
  # file path to the raster
  fnr <- file.path(in.dir, f)
  
  cat('open the ortho-image\n')
  r <- rast(fnr)
  
  # Check orthoimage
  plotRGB(r, stretch="lin")
  
  # The orthoimage can have really high values for the empty parts of the raster, causing the plotRGB to show you a black image. Replace the max values by NA
  # If the orthoimage is to huge and your computer cannot handle the max value replacement, do it for each layer (see below)
  # else replace on the raster stack:
  NAValues <- max(values(r), na.rm=T)
  r[r==max(values(r))] <- NA
  
  # if(!is.na(epsg)){
  #   p <- paste0('+init=epsg:', epsg)
  #   cat('re-project to the swiss coordinate system\t',p,'\n')
  #   r <- project(r, p, method='bilinear', filename=file.path(out.dir,f.prj), overwrite=T)
  # }
  
  # check image again, it should be a correct rgb image
  plotRGB(r, stretch="lin")
  
  cat('Separate each layer:\n')
  # red layer
  rd <- r[[3]]
  names(rd) <- 'rd'
  
  # green layer
  gr <- r[[2]]
  names(gr) <- 'gr'
  
  # blue layer
  bl <- r[[1]]
  names(bl) <- 'bl'
  
  # red edge layer
  re <- r[[4]]
  names(re) <- 're'
  
  # near infra-red layer
  nir <- r[[5]]
  names(nir) <- 'nir'
  
  # # If the orthoimage is to huge and your computer cannot handle the max value replacement, do it for each layer
  # NAValue <- max(values(nir))
  # cat(NAValue, '\n')
  # rd[rd == NAValue] <- NA
  # gr[gr == NAValue] <- NA
  # bl[bl == NAValue] <- NA
  # re[re == NAValue] <- NA
  # nir[nir == NAValue] <- NA
  #
  # # Check false color orthoimage
  rfc <- c(nir, rd, gr)
  plotRGB(rfc, stretch="lin")
  
  rgb <- c(rd, gr, bl)
  plotRGB(rgb, stretch="lin")
  
  
  # check each single layer:
  rd
  gr
  bl
  re
  nir

  rm(r)

  cat('Compute different vegettion index:\n')
  
  # NDVI, with the NIR and red layers
  cat('NDVI\n')
  ndvi <- (nir-rd)/(nir+rd)
  names(ndvi) <- 'ndvi'

  # NDRE, with the NIR and the red edge layer, better with a dense canopy, this index has a better penetration coefficient
  cat('NDRE\n')
  ndre <- (nir-re)/(nir+re)
  names(ndre) <- 'ndre'

  # GNDVI, with the NIR and green layer, better for late vegetation stage
  cat('GNDVI\n')
  gndvi <- (nir-gr)/(nir+gr)
  names(gndvi) <- 'gndvi'

  # GNDVI, with the NIR and green layer, better for late vegetation stage
  cat('NDWI\n')
  ndwi <- (gr-nir)/(gr+nir)
  names(ndwi) <- 'ndwi'

  # Modified Soil Adjusted Vegetation Index (MSAVI2)
  msavi2 <- (1/2) * (2*(nir+1)-sqrt((2*(nir+1))^2-8*(nir-rd)))
  msavi2[msavi2==Inf | msavi2==-Inf] <- NA
  plot(msavi2, col=elevRamp(255))
  names(msavi2) <- 'msavi2'
  
  # # Burn Area Index
  # bai <- 1/((0.1 - rd)^2 + (0.06 - nir)^2)
  # plot(bai, col=elevRamp(255))
  
  # Global Environmental Monitoring Index (GEMI)
  eta <- ((2 * (nir^2 - rd^2)) + (1.5 * nir) + (0.5 * rd))/(nir + rd + 0.5)
  gemi  <- eta * (1 - (0.25 * eta)) - ((rd - 0.125)/(1 - rd))
  gemi[gemi==Inf | gemi==-Inf] <- NA
  plot(gemi, col=BrBG(255))
  names(gemi) <- 'gemi'
  

  # # Visible Atmospherically Resistant Index (VARI)
  # vari = (gr - rd) / (gr + rd - bl)
  # vari[vari==Inf | vari==-Inf] <- NA
  # plot(vari, col=elevRamp(255))
  
  # # Red-Edge Simple Ratio (SRre)
  # srre <- nir / gr
  # srre[srre==Inf | srre==-Inf] <- NA
  # plot(srre, col=elevRamp(255))
  
  # combine all layers to expend the -1 -> 0 values to -3 -> 0 value. It allow to refine the scale and facilitate the detection of ponds
  cat('Combination of all VI\n')
  allvi <- exp(ndwi)+exp(msavi2)+exp(gndvi)
  names(allvi) <- 'allvi'
  
  # nbg <- c(ndwi, msavi2, gndvi)
  # plotRGB(nbg, stretch="lin")
  # plot(allvi, col=BrBG(255), legend=F)
  # plot(ndwi, col=elevRamp(255), legend=T)

  
  # tt <- exp(nir/max(values(nir), na.rm=T))-log(bl/max(values(bl), na.rm=T))
  tt <- exp(ndwi/max(values(ndwi), na.rm=T))/(log(gr/max(values(gr), na.rm=T))/log(bl/max(values(bl), na.rm=T)))
  tt[tt==Inf | tt==-Inf] <- NA
  plot(tt, col=elevRamp(255), legend=T)
  names(tt) <- 'tt'

  plotRGB(c(ndwi,tt,gndvi), stretch="lin")
  
  
  ndwi.idx <- ndwi+abs(min(values(ndwi), na.rm=T)); ndwi.idx
  tt.idx <- tt+abs(min(values(tt), na.rm=T)); tt.idx
  gndvi.idx <- gndvi+abs(min(values(gndvi), na.rm=T)); gndvi.idx
  

  ndwi.255 <- (ndwi.idx*255)/max(values(ndwi.idx), na.rm=T);ndwi.255
  tt.255 <- (tt.idx*255)/max(values(tt.idx), na.rm=T);tt.255
  gndvi.255 <- (gndvi.idx*255)/max(values(gndvi.idx), na.rm=T);gndvi.255
  
  # plotRGB(c(ndwi.255,tt.255,gndvi.255), stretch="lin")
  
  dt <- data.frame('ndwi'=as.data.frame(values(ndwi.255)),
                   'tt'=as.data.frame(values(tt.255)),
                   'gndvi'=as.data.frame(values(gndvi.255)))
  # names(dt) <- c('ndwi', 'tt', 'gndvi')
  summary(dt)
  head(dt); dim(dt)
  # plot(dt, cex=0.3, pch=16)
  
  xy <- xyFromCell(ndwi.255, 1:ncell(ndwi.255))
  
  dt <- cbind(xy,dt); head(dt)
  
  # system.time(write.csv(dt,file.path(out.dir,csvName), row.names=F))
  

  # Stack all the layers, and give them a new name
  s <- c(ndwi, tt, gndvi, msavi2, gemi, ndvi, allvi, rd, gr, bl, re, nir)
  names(s) <- c('ndwi','tt','gndvi', 'msavi2','gemi','ndvi','allvi', 'rd','gr','bl','re','nir')
  
  cat('Save a tif file with Vegetation index layers\n')
  rName <- file.path(out.dir, g)
  system.time(writeRaster(s, rName, overwrite=TRUE))
  
  system.time(writeRaster(c(ndwi, tt, gndvi, msavi2, gemi, ndvi, allvi, rd, gr, bl, re, nir), rName, overwrite=TRUE))
  
  rm(s, dt)  
  

  cat('Plot all\n')
  
  jpegName <- file.path(out.dir, paste(sub("(.+)[.][^.]+$", "\\1", f), 'VegIndex.jpg', sep='_'))
  jpeg(jpegName,6000,6000,units = 'px',quality=100,pointsize=72)
  par(mfrow=c(3,3))
  plotRGB(rgb, stretch="lin")
  plotRGB(rfc, stretch="lin")
  plotRGB(c(gndvi, bl, gr), stretch="lin", main='gndvi, bl, gr')
  plot(ndvi, col=BrBG(255), legend=T, main='NDVI')
  plot(gndvi, col=BrBG(255), legend=T, main='GNDVI')
  plotRGB(c(ndwi, tt, gndvi), stretch="lin", main='ndwi, tt, gndvi')
  plot(ndwi, col=elevRamp(255), legend=T, main='NDWI')
  plot(allvi, col=elevRamp(255), legend=T, main='[exp(NDWI)+exp(MSAVI2)+exp(GNDVI)]')
  plot(tt, col=elevRamp(255), legend=T, main='test')
  dev.off()
  
  par(mfrow=c(1,1))
  
  
  # bot$sendDocument(chat_id = 'YOUR_CHAT_ID_FROM_TELEGRAM', document = jpegName)
  bot$sendDocument(chat_id = 783925976, document = jpegName)
  
  rm(rgb, rfc, gndvi, bl, gr, ndvi, ndwi, tt, allvi, ndre)
  
  # Close your bracket if you made a loop through several orthoimages
}
