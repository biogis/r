#########################################################
#########################################################
# © eRey.ch | bioGIS; erey@biogis.ch
# created on 2022.02.17
# modified on 2022.06.29
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
  'sf','terra','spatial','leaflet','raster','caret',

  #graphics libraries
  'RColorBrewer','jpeg','png',

  #Looping libraries
  'foreach','iterators','parallel','doParallel',

  #R libraries
  'telegram.bot','tcltk','svDialogs', 'tidyverse','data.table'
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

# scale raster between 0 and 1
sc <- function(r){r/max(values(r), na.rm=T)}


# Color ramp for plotting
BrBG <- colorRampPalette(c('#543005','#8c510a','#bf812d','#dfc27d','#f6e8c3','#f5f5f5','#c7eae5','#80cdc1','#35978f','#01665e','#003c30'))
elevRamp <- colorRampPalette(c('#aff0e9','#ffffb3','#008040','#fcba03','#780000','#69300d','#ababab','#fffcff'))
ScoRusRamp <- colorRampPalette(c('#2346c7','#ffffb3','#008040','#fcba03','#780000','#69300d', '#fe7c97', '#680459'))
cb <- colorRampPalette(c('#E69F00', '#56B4E9', '#009E73', '#F0E442', '#0072B2', '#D55E00', '#CC79A7', '#000000'))


#Define how many cores you want to use
UseCores <- detectCores() -1
# UseCores <- 10

#Register CoreCluster
cl       <- makeCluster(UseCores)
registerDoParallel(cl)


#############################################################################
############################# -- INPUT - PART -- ############################
#############################################################################

# Initiate the bot session using the token from the enviroment variable.
# bot <- Bot(token = 'YOUR.TOKEN.FROM.TELEGRAM.BOT.FATHER')


# choose working directory with all orthoimages
in.dir <- choose_dir(caption = "Select input tif folder")

# select directory if the in.dir is separated in different projects
# d <- 'Marthalen'

# choose output directory
out.dir <- choose_dir(caption = "Select output tif folder")



#get CRS for a reprojection of the raster using the EPSG code. See on epsg.io for more informations
# CH1903_lv03: 21781
# CH1903_LV95: 2056
# wgs84: 4326
# wgs84 Pseudo-Mercator: 3857
# Lambert 93: 2154

#If no reprojection is required, enter NA

# epsg <- dlgInput("Enter an epsg number for reprojection\nor NA if no reprojection is required:\n# CH1903_lv03:\t21781\n# CH1903_LV95:\t2056\n# wgs84:\t4326", 2056)$res
epsg <- 2056
# epsg <- NA

#############################################################################
########################## -- LET IT RUN - PART -- ##########################
#############################################################################


cat('you selected as working directories:\n', '\tin.dir:\t\t', in.dir, '\n ', '\tout.dir:\t', out.dir, '\n')

setwd(file.path(in.dir,d))

# list all .tif files in your orthoimage directory
fns <- list.files(file.path(in.dir,d), pattern='.tif$')#; print(fns)

cat('found the following images in the in.dir directory:\n',fns,sep='\n')

# Choose your orthoimage, this is the place to include a loop if several orthoimages have to be analysed:
i <- 5
f <- fns[i];f
# fns <- fns[c(8:length(fns))]

#
# foreach(i=1:length(fns)) %dopar% {
#   library(terra)
for(i in 1:length(fns)){
  f <- fns[i]
  f.prj <- paste(sub("(.+)[.][^.]+$", "\\1", f), 'ch.tif', sep='_')
  g <- paste(sub("(.+)[.][^.]+$", "\\1", f), 'VegIndex.tif', sep='_')
  csvName <- paste(sub("(.+)[.][^.]+$", "\\1", f), 'VegIndex.csv', sep='_')
  cat('Working on:\n', '\tinput file:\t\t', f, '\n ', '\toutput File:\t\t', g, '\n','\treprojected file:\t',f.prj,'\n')


  # file path to the raster
  cat('open the ortho-image\n')

  if(file.exists(file.path(out.dir,f.prj))){
    fnr <- file.path(out.dir,f.prj)} else {
    fnr <- file.path(in.dir,d, f)}


  r <- rast(fnr)

  # Check orthoimage
  plotRGB(r, stretch="lin")

  # The orthoimage can have really high values for the empty parts of the raster, causing the plotRGB to show you a black image. Replace the max values by NA
  # If the orthoimage is to huge and your computer cannot handle the max value replacement, do it for each layer (see below)
  # else replace on the raster stack:
  NAValues <- max(values(r), na.rm=T)
  if(NAValues == 2^16-1){
    r[r==max(values(r))] <- NA}

  # Project orthoimage to swiss coordinate system (or any other, check epsg <- XXXXX)
  if(!is.na(epsg) & !file.exists(file.path(out.dir,f.prj))){
    p <- paste0('+init=epsg:', epsg)
    cat('re-project to the swiss coordinate system\t',p,'\n')
    r <- project(r, p, method='bilinear', filename=file.path(out.dir,f.prj), overwrite=T)
  }

  # check image again, it should be a correct rgb image
  plotRGB(r, stretch="lin")

  cat('Separate each layer:\n')

  cat('\tRed\n')
  # red layer
  rd <- r[[3]]
  names(rd) <- 'rd'
  rd <- rd/sd(values(rd), na.rm=T)

  cat('\tGreen\n')
  # green layer
  gr <- r[[2]]
  names(gr) <- 'gr'
  gr <- gr/sd(values(gr), na.rm=T)

  cat('\tBlue\n')
  # blue layer
  bl <- r[[1]]
  names(bl) <- 'bl'
  bl <- bl/sd(values(bl), na.rm=T)

  cat('\tRed Edge\n')
  # red edge layer
  re <- r[[4]]
  names(re) <- 're'
  re <- re/sd(values(re), na.rm=T)

  cat('\tNear Infrared\n')
  # near infra-red layer
  nir <- r[[5]]
  names(nir) <- 'nir'
  nir <- nir/sd(values(nir), na.rm=T)

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
  cat('\tNDVI\n')
  ndvi <- (nir-rd)/(nir+rd)
  names(ndvi) <- 'ndvi'
  ndvi <- ndvi/sd(values(ndvi), na.rm=T)


  # # NDRE, with the NIR and the red edge layer, better with a dense canopy, this index has a better penetration coefficient
  # cat('NDRE\n')
  # ndre <- (nir-re)/(nir+re)
  # names(ndre) <- 'ndre'
  # ndre <- ndre/sd(values(ndre), na.rm=T)


  # GNDVI, with the NIR and green layer, better for late vegetation stage
  cat('\tGNDVI\n')
  gndvi <- (nir-gr)/(nir+gr)
  names(gndvi) <- 'gndvi'
  gndvi <- gndvi/sd(values(gndvi), na.rm=T)
  # gndvi[gndvi<0] <- NA


  # GNDWI, with the NIR and green layer, water version of the NDVI
  cat('\tNDWI\n')
  ndwi <- (gr-nir)/(gr+nir)
  # ndwi <- (bl-nir)/(bl+nir)
  names(ndwi) <- 'ndwi'
  ndwi <- ndwi/sd(values(ndwi), na.rm=T)
  # ndwi[ndwi<0] <- NA

  # # Modified Soil Adjusted Vegetation Index (MSAVI2)
  # # msavi2 <- (1/2) * (2*(nir+1)-sqrt((2*(nir+1))^2-8*(nir-rd)))
  # msavi2 <- (1/2) * (2*(nir+1)-sqrt((2*(nir+1))^2-8*(nir-gr)))
  # msavi2[msavi2==Inf | msavi2==-Inf] <- NA
  # msavi2 <- msavi2/sd(values(msavi2), na.rm=T)
  # # plot(msavi2, col=elevRamp(255))
  # names(msavi2) <- 'msavi2'

  # # Burn Area Index
  # bai <- 1/((0.1 - rd)^2 + (0.06 - nir)^2)
  # plot(bai, col=elevRamp(255))

  # # Global Environmental Monitoring Index (GEMI)
  # eta <- ((2 * (nir^2 - rd^2)) + (1.5 * nir) + (0.5 * rd))/(nir + rd + 0.5)
  # # gemi  <- eta * (1 - (0.25 * eta)) - ((rd - 0.125)/(1 - rd))
  # gemi  <- eta * (1 - (0.25 * eta)) - ((gr - 0.125)/(1 - gr))
  # gemi[gemi==Inf | gemi==-Inf] <- NA
  # gemi <- gemi/sd(values(gemi), na.rm=T)
  # # plot(gemi, col=BrBG(255))
  # names(gemi) <- 'gemi'


  # # Visible Atmospherically Resistant Index (VARI)
  # vari = (gr - rd) / (gr + rd - bl)
  # vari[vari==Inf | vari==-Inf] <- NA
  # plot(vari, col=elevRamp(255))

  # # Red-Edge Simple Ratio (SRre)
  # srre <- nir / gr
  # srre[srre==Inf | srre==-Inf] <- NA
  # plot(srre, col=elevRamp(255))

  # # combine all layers to expend the -1 -> 0 values to -3 -> 0 value. It allow to refine the scale and facilitate the detection of ponds
  # cat('Combination of all VI\n')
  # allvi <- -(exp(ndvi)+exp(msavi2)+exp(gndvi))
  # names(allvi) <- 'allvi'
  # allvi <- allvi/sd(values(allvi), na.rm=T)

  # nbg <- c(ndwi, msavi2, gndvi)
  # plotRGB(nbg, stretch="lin")
  # plot(allvi, col=BrBG(255), legend=F)
  # plot(ndwi, col=elevRamp(255), legend=T)


  # Combine NDWI, green and blue layer to show water surface -> new Water Index -- WI
  cat('\texp(ndwi) * [log(bl)/log(gr)]\n')
  wi <- exp(sc(ndwi))*(log(sc(bl))/log(sc(gr)))
  wi[wi==Inf | wi==-Inf] <- NA
  names(wi) <- 'wi'
  wi <- wi/sd(values(wi), na.rm=T)
  # wi[wi==max(values(wi), na.rm=T)] <- NA
  plot(wi, col=elevRamp(255), legend=T) # Check layer


  # nr <- ndwi^log(wi)
  cat('\texp(ndwi) * log(wi+2) * (re/nir)]\n')
  nr <- exp(ndwi)*log(wi+2)*(re/nir)
  nr[nr==Inf | nr==-Inf] <- NA
  names(nr) <- 'nr'
  nr <- nr/sd(values(nr), na.rm=T)
  plot(nr, col=elevRamp(255))
  plotRGB(c(ndwi,wi,nr), stretch="lin")


  # plotRGB(c(ndwi,wi,gndvi), stretch="lin")
  # plotRGB(c(ndwi,wi,bl), stretch="lin")
  # plotRGB(c(ndwi,nr,gndvi), stretch="lin")
  # plotRGB(c(ndwi,wi,nr), stretch="lin")


  cat('index ndwi layer 0 -> 255\n')
  ndwi.idx <- ndwi+abs(min(values(ndwi), na.rm=T)); ndwi.idx
  ndwi.255 <- (ndwi.idx*255)/max(values(ndwi.idx), na.rm=T);ndwi.255

  cat('index wi layer 0 -> 255\n')
  wi.idx <- wi+abs(min(values(wi), na.rm=T)); wi.idx
  wi.255 <- (wi.idx*255)/max(values(wi.idx), na.rm=T);wi.255

  cat('index nr layer 0 -> 255\n')
  nr.idx <- nr+abs(min(values(nr), na.rm=T)); nr.idx
  nr.255 <- (nr.idx*255)/max(values(nr.idx), na.rm=T);nr.255

  cat('index bl layer 0 -> 255\n')
  bl.idx <- bl+abs(min(values(bl), na.rm=T)); bl.idx
  bl.255 <- (bl.idx*255)/max(values(bl.idx), na.rm=T);bl.255

  cat('index nir layer 0 -> 255\n')
  nir.idx <- nir+abs(min(values(nir), na.rm=T)); nir.idx
  nir.255 <- (nir.idx*255)/max(values(nir.idx), na.rm=T);nir.255

  cat('create data frame\n')
  dt <- data.frame('ndwi.255'=as.data.frame(values(ndwi.255)),
                   'wi.255'=as.data.frame(values(wi.255)),
                   'nr.255'=as.data.frame(values(nr.255)),
                   'bl.255'=as.data.frame(values(bl.255)),
                   'nir.255'=as.data.frame(values(nir.255)))
  names(dt) <- c('ndwi', 'wi', 'nr', 'bl', 'nir')
  summary(dt)
  head(dt); dim(dt)
  # plot(dt, cex=0.3, pch=16)

  cat('compute hex color code of the layers ndwi==r, wi==g, nr==b\n')
  y <- which(!is.na(dt$ndwi) & is.na(dt$nr)); length(y)
  dt[y,] <- NA
  y <- which(!is.na(dt$ndwi)); length(y)
  dt[y,'hex_wtr'] <- rgb(dt[y,'ndwi'],dt[y,'wi'],dt[y,'nr'], max=255)
  # dt[y,'hex_bl'] <- rgb(dt[y,'ndwi'],dt[y,'nr'],dt[y,'bl'], max=255)

  cat('add xy coordinate\n')
  xy <- xyFromCell(ndwi.255, 1:ncell(ndwi.255))
  dt <- cbind(xy,dt); head(dt)

  cat('add black hex code instead of NA values\n')
  j <- which(is.na(dt$hex_wtr)); length(j)
  dt[j,'hex_wtr'] <- rgb(0,0,0, max=255)
  # dt[j,'hex_bl'] <- rgb(0,0,0, max=255)

  cat('convert hex code back to values\n')
  dt$hexCde_wtr <- strtoi(str_sub(dt$hex_wtr,2,7),base=16L)
  # dt$hexCde_bl <- strtoi(str_sub(dt$hex_bl,2,7),base=16L)
  dt <- data.table(dt); dt

  cat('Convert combined ndwi+wi+nr hex values into a raster\n')
  water_nir <- nr
  # water_bl <- nr
  values(water_nir) <- dt$hexCde_wtr
  # values(water_bl) <- dt$hexCde_bl
  plot(water_nir, col=elevRamp(255))
  # plot(water_bl, col=elevRamp(255))

  system.time(fwrite(dt,file.path(out.dir,csvName), row.names=F))

  # lNum <- list('ndwi'=na.omit(values(ndwi.255)),
  #              'wi' = na.omit(values(wi.255)),
  #              'nr' = na.omit(values(nr.255)),
  #              'bl' = na.omit(values(bl.255)),
  #              'nir' = na.omit(values(nir.255)))
  #
  #
  # histName <- file.path(out.dir, paste(sub("(.+)[.][^.]+$", "\\1", f), 'histo.jpg', sep='_'))
  # jpeg(histName,12000,6000,units = 'px',quality=100,pointsize=72)
  # par(mfrow=c(2,3))
  # iwalk(lNum, ~hist(.x, main=.y, breaks=255, col='steelblue', border='steelblue'))
  # dev.off()


  # remove data frame and other raster layers
  rm(dt, xy, ndwi.255, wi.255, nr.255, bl.255, nir.255, ndwi.idx, wi.idx, nr.idx, bl.idx, nir.idx)


  cat('stack all layers and give them a new name\n')
  # Stack all the layers, and give them a new name
  # s <- c(ndwi, wi, nr, gndvi, msavi2, gemi, ndvi, allvi, rd, gr, bl, re, nir)
  # names(s) <- c('ndwi','wi','nr','gndvi','msavi2','gemi','ndvi','allvi','rd','gr','bl','re','nir')

  s <- c(ndwi, wi, nr, rd, gr, bl, re, nir, gndvi, water_nir)
  names(s) <- c('ndwi','wi','nr','rd','gr','bl','re','nir','gndvi','water_nir')


  cat('Save a tif file with Vegetation index layers\n')
  rName <- file.path(out.dir, g)
  system.time(writeRaster(s, rName, overwrite=TRUE))


  # remove raster stack
  rm(s)

  cat('Plot all\n')

  jpegName <- file.path(out.dir, paste(sub("(.+)[.][^.]+$", "\\1", f), 'VegIndex.jpg', sep='_'))
  jpeg(jpegName,6000,6000,units = 'px',quality=100,pointsize=72)
  par(mfrow=c(3,3))
  plotRGB(c(rd, gr, bl), stretch="lin")
  plotRGB(c(nir, rd, gr), stretch="lin")
  plotRGB(c(gndvi, bl, gr), stretch="lin", main='gndvi, bl, gr')
  # plot(ndvi, col=BrBG(255), legend=T, main='NDVI')
  plot(gndvi, col=BrBG(255), legend=F, main='GNDVI')
  plotRGB(c(ndwi, wi, bl), stretch="lin", main='ndwi, wi, bl')
  plotRGB(c(ndwi, nr, bl), stretch="lin", main='ndwi, nr, bl')
  plot(ndwi, col=elevRamp(255), legend=F, main='NDWI')
  plot(wi, col=elevRamp(255), legend=F, main='[exp(NDVI)*(log(bl)/log(gr))')
  plotRGB(c(ndwi, wi, nr), stretch="lin", main='ndwi, nr, nr')
  dev.off()


  jpegName <- file.path(out.dir, 'VegIndex.jpg')
  jpeg(jpegName,6000,6000,units = 'px',quality=100,pointsize=72)
  par(mfrow=c(1,1))
  plot(water_nir, col=elevRamp(255))
  dev.off()

  par(mfrow=c(1,1))

  # bot$sendDocument(chat_id = 'YOUR_CHAT_ID_FROM_TELEGRAM', document = jpegName)
  bot$sendDocument(chat_id = 783925976, document = jpegName)
  # bot$sendDocument(chat_id = 783925976, document = histName)

  # remove raster layers
  # rm(rd, gr, bl, re, nir, rfc, rgb, ndvi, ndre, gndvi, ndwi, msavi2, eta, gemi, allvi, wi, nr, water_nir)
  rm(rd, gr, bl, re, nir, rfc, rgb, gndvi, ndwi, wi, nr)

  # remove file path and file names
  rm(csvName, rName, f, f.prj, fnr, g)
  # Close your bracket if you made a loop through several orthoimages

}
#
map <- leaflet() %>%
 addTiles() %>%
 addRasterImage(raster::raster(water_nir), colors = elevRamp(255), opacity = 0.8) %>%
 setView(7.36265, 46.88558, zoom = 12) %>%
 # setView(8.22667, 46.80111, zoom = 8) %>%
 addWMSTiles(baseUrl = 'https://wmts20.geo.admin.ch/1.0.0/ch.swisstopo.pixelkarte-farbe/default/current/3857/{z}/{x}/{y}.jpeg',
             layers = 'Cartes nationales (couleur)',
             # layers = 'Colonies de bouquetins',
             options = WMSTileOptions(format = "image/png", transparent = TRUE, opacity=0.45),
             attribution = "")


map
