#########################################################
#########################################################
# © eRey.ch | bioGIS; erey@biogis.ch
# created on 2022.01.31
# modified on 2022.02.08
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


#############################################################################
############################# -- INPUT - PART -- ############################
#############################################################################

# Initiate the bot session using the token from the enviroment variable.
# bot <- Bot(token = 'YOUR.TOKEN.FROM.TELEGRAM.BOT.FATHER')
bot <- Bot(token = '716214310:AAFIVH9QOmYOWb6FvDQYjTHGuP4GnYCtDg8')


# choose working directory with all orthoimages
in.dir <- choose_dir(caption = "Select input tif folder")

# choose output directory
out.dir <- choose_dir(caption = "Select output wav folder")


#get CRS for a reprojection of the raster using the EPSG code. See on epsg.io for more informations
# CH1903_lv03: 21781
# CH1903_LV95: 2056
# wgs84: 4326
# Lambert 93: 2154

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


for(i in 1:length(fns)){
  f <- fns[i]
  f.prj <- paste(sub("(.+)[.][^.]+$", "\\1", f), 'ch.tif', sep='_')
  g <- paste(sub("(.+)[.][^.]+$", "\\1", f), 'VegIndex.tif', sep='_')
  cat('Working on:\n', '\tinput file:\t\t', f, '\n ', '\toutput File:\t\t', g, '\n','\treprojected file:\t',f.prj)
  
  # file path to the raster
  fnr <- file.path(in.dir, f)
  
  cat('open the ortho-image\n')
  r <- rast(fnr)
  
  # Check orthoimage
  plotRGB(r, stretch="lin")
  
  # The orthoimage can have really high values for the empty parts of the raster, causing the plotRGB to show you a black image. Replace the max values by NA
  # If the orthoimage is to huge and your computer cannot handle the max value replacement, do it for each layer (see below)
  # else replace on the raster stack:
  r[r==max(values(r))] <- NA
  
  if(!is.na(epsg)){
    proj <- paste0('+init=epsg:', epsg)
    cat('re-project to the swiss coordinate system\t',proj,'\t')
    r <- project(r, proj, method='bilinear', filename=file.path(in.dir,f.prj), overwrite=T)
  }

  
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
  
  
  # check each single layer:
  rd
  gr
  bl
  re
  nir
  
  cat('Compute different vegettion index:\n')
  
  # NDVI, with the NIR and red layers
  cat('NDVI\n')
  ndvi <- (nir-rd)/(nir+rd)
  
  # NDRE, with the NIR and the red edge layer, better with a dense canopy, this index has a better penetration coefficient
  cat('NDRE\n')
  ndre <- (nir-re)/(nir+re)
  
  # GNDVI, with the NIR and green layer, better for late vegetation stage
  cat('GNDVI\n')
  gndvi <- (nir-gr)/(nir+gr)
  
  # combine all layers to expend the -1 -> 0 values to -3 -> 0 value. It allow to refine the scale and facilitate the detection of ponds
  cat('Combination of all VI\n')
  allvi <- exp(ndvi)+exp(ndre)+exp(gndvi)
  
  # remove the values higher than the 5% quantile, this schould remove most of the non water pixels (shadow, roads, ...)
  dt <- values(allvi)
  allvi[allvi>quantile(dt, 0.05, na.rm=T)] <- NA
  
  
  # Stack all the layers, and give them a new name
  s <- c(rd,gr,bl,re,nir,ndvi, ndre, gndvi, allvi)
  names(s) <- c('rd','gr','bl','re','nir','ndvi', 'ndre', 'gndvi', 'allvi')
  
  cat('Plot all\n')
  
  jpegName <- file.path(out.dir, paste(sub("(.+)[.][^.]+$", "\\1", f), 'VegIndex.jpg', sep='_'))
  jpeg(jpegName,3000,3000,units = 'px',quality=100,pointsize=36)
  par(mfrow=c(3,2))
  plotRGB(r, stretch="lin")
  plotRGB(rfc, stretch="lin")
  plot(ndvi, col=BrBG(255), legend=F, main='NDVI')
  plot(gndvi, col=BrBG(255), legend=T, main='GNDVI')
  plot(ndre, col=BrBG(255), legend=F, main='NDRE')
  plot(allvi, col='steelblue', legend=F, main='Quantile 5% of [exp(NDVI)+exp(GNDVI)+exp(NDRE)]')
  dev.off()
  
  # Send the image through Telegram bot
  # bot$sendDocument(chat_id = 'YOUR_CHAT_ID_FROM_TELEGRAM', document = jpegName)
  
  cat('Save a tif file with Vegetation index layers\n')
  rName <- file.path(out.dir, g)
  system.time(writeRaster(s, rName, overwrite=TRUE))
  
  # Close your bracket if you made a loop through several orthoimages
}

