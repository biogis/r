#########################################################
#########################################################
# Â© eRey.ch | bioGIS; erey@biogis.ch
# created on 2022.02.17
# modified on 2022.04.22
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
  'sf','terra','spatial','leaflet','raster',

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
bot <- Bot(token = '716214310:AAFIVH9QOmYOWb6FvDQYjTHGuP4GnYCtDg8')


# choose working directory with all orthoimages
# in.dir <- choose_dir(caption = "Select input tif folder")
in.dir <- '/home/erey/Documents/infoFauna/pyData/Drone_Process/raw/transfer_43935_files_fb7c6ebb'
in.dir <- 'C:/tmp/Drone_Process/VegIndex/raw'

# choose output directory
# out.dir <- choose_dir(caption = "Select output tif folder")
out.dir <- '/home/erey/Documents/infoFauna/pyData/Drone_Process/VegIndex/'
out.dir <- 'C:/tmp/Drone_Process/VegIndex/result'



#get CRS for a reprojection of the raster using the EPSG code. See on epsg.io for more informations
# CH1903_lv03: 21781
# CH1903_LV95: 2056
# wgs84: 4326
# wgs84 Pseudo-Mercator: 3857
# Lambert 93: 2154

#If no reprojection is required, enter NA

# epsg <- dlgInput("Enter an epsg number for reprojection\nor NA if no reprojection is required:\n# CH1903_lv03:\t21781\n# CH1903_LV95:\t2056\n# wgs84:\t4326", 2056)$res
epsg <- 2056
epsg <- NA

#############################################################################
########################## -- LET IT RUN - PART -- ##########################
#############################################################################


cat('you selected as working directories:\n', '\tin.dir:\t\t', in.dir, '\n ', '\tout.dir:\t', out.dir, '\n')

setwd(in.dir)

# list all .tif files in your orthoimage directory
fns <- list.files(in.dir, patter='.tif$')#; print(fns)

cat('found the following images in the in.dir directory:\n',fns,sep='\n')

# Choose your orthoimage, this is the place to include a loop if several orthoimages have to be analysed:
i <- 2
f <- fns[i]

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
  ndvi <- ndvi/sd(values(ndvi), na.rm=T)


  # NDRE, with the NIR and the red edge layer, better with a dense canopy, this index has a better penetration coefficient
  cat('NDRE\n')
  ndre <- (nir-re)/(nir+re)
  names(ndre) <- 'ndre'
  ndre <- ndre/sd(values(ndre), na.rm=T)


  # GNDVI, with the NIR and green layer, better for late vegetation stage
  cat('GNDVI\n')
  gndvi <- (nir-gr)/(nir+gr)
  names(gndvi) <- 'gndvi'
  gndvi <- gndvi/sd(values(gndvi), na.rm=T)
  # gndvi[gndvi<0] <- NA


  # GNDWI, with the NIR and green layer, water version of the NDVI
  cat('NDWI\n')
  ndwi <- (gr-nir)/(gr+nir)
  names(ndwi) <- 'ndwi'
  ndwi <- ndwi/sd(values(ndwi), na.rm=T)
  # ndwi[ndwi<0] <- NA

  # Modified Soil Adjusted Vegetation Index (MSAVI2)
  msavi2 <- (1/2) * (2*(nir+1)-sqrt((2*(nir+1))^2-8*(nir-rd)))
  msavi2 <- (1/2) * (2*(nir+1)-sqrt((2*(nir+1))^2-8*(nir-gr)))
  msavi2[msavi2==Inf | msavi2==-Inf] <- NA
  msavi2 <- msavi2/sd(values(msavi2), na.rm=T)
  plot(msavi2, col=elevRamp(255))
  names(msavi2) <- 'msavi2'

  # # Burn Area Index
  # bai <- 1/((0.1 - rd)^2 + (0.06 - nir)^2)
  # plot(bai, col=elevRamp(255))

  # Global Environmental Monitoring Index (GEMI)
  eta <- ((2 * (nir^2 - rd^2)) + (1.5 * nir) + (0.5 * rd))/(nir + rd + 0.5)
  gemi  <- eta * (1 - (0.25 * eta)) - ((rd - 0.125)/(1 - rd))
  gemi  <- eta * (1 - (0.25 * eta)) - ((gr - 0.125)/(1 - gr))
  gemi[gemi==Inf | gemi==-Inf] <- NA
  gemi <- gemi/sd(values(gemi), na.rm=T)
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
  allvi <- -(exp(ndvi)+exp(msavi2)+exp(gndvi))
  names(allvi) <- 'allvi'
  allvi <- allvi/sd(values(allvi), na.rm=T)

  # nbg <- c(ndwi, msavi2, gndvi)
  # plotRGB(nbg, stretch="lin")
  # plot(allvi, col=BrBG(255), legend=F)
  # plot(ndwi, col=elevRamp(255), legend=T)


  # Combine NDWI, green and blue layer to show water surface -> new Water Index -- WI
  wi <- exp(ndwi/max(values(ndwi), na.rm=T))/(log(gr/max(values(gr), na.rm=T))/log(bl/max(values(bl), na.rm=T)))
  wi[wi==Inf | wi==-Inf] <- NA
  plot(wi, col=elevRamp(255), legend=T) # Check layer
  names(wi) <- 'wi'
  wi <- wi/sd(values(wi), na.rm=T)
  wi[wi==max(values(wi), na.rm=T)] <- NA


  nr <- ndwi^log(wi)
  nr[nr==Inf | nr==-Inf] <- NA
  nr <- nr/sd(values(nr), na.rm=T)
  plot(nr, col=elevRamp(255))


  plotRGB(c(ndwi,wi,gndvi), stretch="lin")
  plotRGB(c(ndwi,nr,gndvi), stretch="lin")


  ndwi.idx <- ndwi+abs(min(values(ndwi), na.rm=T)); ndwi.idx
  wi.idx <- wi+abs(min(values(wi), na.rm=T)); wi.idx
  gndvi.idx <- gndvi+abs(min(values(gndvi), na.rm=T)); gndvi.idx


  ndwi <- (ndwi.idx*255)/max(values(ndwi.idx), na.rm=T);ndwi
  wi <- (wi.idx*255)/max(values(wi.idx), na.rm=T);wi
  gndvi <- (gndvi.idx*255)/max(values(gndvi.idx), na.rm=T);gndvi


  ndwi.255 <- (ndwi.idx*255)/max(values(ndwi.idx), na.rm=T);ndwi.255
  wi.255 <- (wi.idx*255)/max(values(wi.idx), na.rm=T);wi.255
  gndvi.255 <- (gndvi.idx*255)/max(values(gndvi.idx), na.rm=T);gndvi.255

  # rcl <- data.frame(from=seq(5,245,by=10),
  #                   to=seq(15,255,by=10),
  #                   becomes=seq(10,250,by=10))
  # rcl <- rbind(data.frame(from=0, to=5, becomes=0), rcl)
  # ndwi.255 <- classify(ndwi.255,as.matrix(rcl))
  # wi.255 <- classify(wi.255,as.matrix(rcl))
  # gndvi.255 <- classify(gndvi.255,as.matrix(rcl))

  plotRGB(c(ndwi.255,wi.255,gndvi.255), stretch="lin")



  # plotRGB(c(ndwi.255,tt.255,gndvi.255), stretch="lin")

  dt <- data.frame('ndwi.255'=as.data.frame(values(ndwi.255)),
                   'wi.255'=as.data.frame(values(wi.255)),
                   'gndvi.255'=as.data.frame(values(gndvi.255)))
  # names(dt) <- c('ndwi', 'wi', 'gndvi')
  summary(dt)
  head(dt); dim(dt)
  # plot(dt, cex=0.3, pch=16)

  y <- which(!is.na(dt$ndwi) & is.na(dt$wi)); length(y)
  dt[y,] <- NA
  y <- which(!is.na(dt$ndwi)); length(y)
  dt[y,'hex'] <- rgb(dt[y,1],dt[y,2],dt[y,3], max=255)


  xy <- xyFromCell(ndwi.255, 1:ncell(ndwi.255))

  dt <- cbind(xy,dt); head(dt)

  j <- which(!is.na(dt$ndwi)); length(j)
  dt <- data.table(dt[j,]); dt
  
  system.time(fwrite(dt,file.path(out.dir,csvName), row.names=F))
  # system.time(write.csv(dt,file.path(out.dir,csvName), row.names=F))
  
  # plot(dt$x,dt$y, col=dt$hex, cex=0.1)
  # 
  # 
  # lNum <- list('ndwi'=na.omit(values(ndwi)),
  #              'msavi2'=na.omit(values(msavi2)),
  #              'allvi'=na.omit(values(allvi)),
  #              'gndvi' = na.omit(values(gndvi)),
  #              'wi' = na.omit(values(wi)),
  #              # 'gemi' = na.omit(values(gemi)),
  #              'nr' = na.omit(values(nr)))
  # 
  # 
  # histName <- file.path(out.dir, paste(sub("(.+)[.][^.]+$", "\\1", f), 'histo.jpg', sep='_'))
  # jpeg(histName,12000,6000,units = 'px',quality=100,pointsize=72)
  # par(mfrow=c(2,3))
  # iwalk(lNum, ~hist(.x, main=.y, breaks=255, col='steelblue', border='steelblue'))
  # dev.off()


  # plotRGB(c(ndwi,wi,allvi), stretch="lin")
  # plotRGB(c(ndwi,nr,allvi), stretch="lin")
  # 
  # par(mfrow=c(2,2))
  # 
  # plotRGB(c(ndwi,msavi2,gndvi), stretch="lin")
  # plotRGB(c(ndwi,wi,gemi), stretch="lin")
  # plotRGB(c(ndwi,wi,gndvi), stretch="lin")
  # plotRGB(c(ndwi,nr, gndvi), stretch="lin")



  # # It is important to set the seed generator because `kmeans` initiates the centers in random locations
  # set.seed(99)
  # # We want to create 10 clusters, allow 500 iterations, start with 5 random sets using "Lloyd" method
  # kmncluster <- kmeans(na.omit(nr), centers=10,iter.max = 500, nstart = 5, algorithm="Lloyd")
  # # kmeans returns an object of class "kmeans"
  # str(kmncluster)
  #
  # knr <- ndwi
  # values(knr) <- kmncluster$cluster
  # knr
  # plot(knr, col=elevRamp(10))
  #

  # remove data frame and other raster layers
  rm(dt, xy, ndwi.255, wi.255, gndvi.255, ndwi.idx, wi.idx, gndvi.idx)


  # Stack all the layers, and give them a new name
  s <- c(ndwi, wi, gndvi, msavi2, gemi, nr, ndvi, allvi, rd, gr, bl, re, nir)
  names(s) <- c('ndwi','wi','gndvi', 'msavi2','gemi', 'nr','ndvi','allvi', 'rd','gr','bl','re','nir')

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
  plotRGB(c(ndwi, wi, gndvi), stretch="lin", main='ndwi, tt, gndvi')
  plotRGB(c(ndwi, nr, allvi), stretch="lin", main='ndwi, tt, gndvi')
  plot(ndwi, col=elevRamp(255), legend=F, main='NDWI')
  plot(allvi, col=elevRamp(255), legend=F, main='[exp(NDVI)+exp(MSAVI2)+exp(GNDVI)]')
  plot(wi, col=elevRamp(255), legend=F, main='wi')
  dev.off()

  par(mfrow=c(1,1))


  # bot$sendDocument(chat_id = 'YOUR_CHAT_ID_FROM_TELEGRAM', document = jpegName)
  bot$sendDocument(chat_id = 783925976, document = jpegName)
  # bot$sendDocument(chat_id = 783925976, document = histName)

  # remove raster layers
  rm(rd, gr, bl, re, nir, rfc, rgb, ndvi, ndre, gndvi, ndwi, msavi2, eta, gemi, allvi, wi)

  # remove file path and file names
  rm(csvName, rName, f, f.prj, fnr, g)
  # Close your bracket if you made a loop through several orthoimages

}
# 
# map <- leaflet() %>%
#   addTiles() %>%
#   addRasterImage(raster::raster(wi), colors = elevRamp(255), opacity = 0.8) %>%
#   setView(7.36265, 46.88558, zoom = 12) %>%
#   # setView(8.22667, 46.80111, zoom = 8) %>%
#   addWMSTiles(baseUrl = 'https://wmts20.geo.admin.ch/1.0.0/ch.swisstopo.pixelkarte-farbe/default/current/3857/{z}/{x}/{y}.jpeg',
#               layers = 'Cartes nationales (couleur)',
#               # layers = 'Colonies de bouquetins',
#               options = WMSTileOptions(format = "image/png", transparent = TRUE, opacity=0.45),
#               attribution = "")
# 
# 
# map
