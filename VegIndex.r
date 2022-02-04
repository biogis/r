#########################################################
#########################################################
# © eRey.ch | bioGIS; erey@biogis.ch
# created on 2022.01.31
# modified on 2022.02.02
#source('./vegIndex.r')
# https://github.com/biogis/r/blob/master/vegIndex.r
# source('https://raw.githubusercontent.com/biogis/r/master/vegIndex.r')


###########################################################
###########################################################


#fix timezone on Bern
Sys.setenv(TZ='Europe/Paris')
# clear all objects saved in the R session
rm(list=ls())

#get proj4 for LV03 or LV95 for any projection map or spatial analysis requireng a projection
# CH1903_lv03
lv03 = "+init=epsg:21781"
# lv03 <- CRS(SRS_string='EPSG:21781')

#CH1903_LV95
lv95 = "+init=epsg:2056"
# lv95 <- CRS(SRS_string='EPSG:2056')

#wgs84:
wgs84 = "+init=epsg:4326"
# wgs84 <- CRS(SRS_string='EPSG:4326')




packages <- c(
  #Spatial libraries
  'sf','terra','spatial','leaflet',

  #graphics libraries
  'RColorBrewer','jpeg','png',

  #R libraries
  'telegram.bot'
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



# Initiate the bot session using the token from the enviroment variable.
bot <- Bot(token = 'YOUR.TOKEN.FROM.TELEGRAM.BOT.FATHER')



# Color ramp for plotting
BrBG <- colorRampPalette(c('#543005','#8c510a','#bf812d','#dfc27d','#f6e8c3','#f5f5f5','#c7eae5','#80cdc1','#35978f','#01665e','#003c30'))
elevRamp <- colorRampPalette(c('#aff0e9','#ffffb3','#008040','#fcba03','#780000','#69300d','#ababab','#fffcff'))
ScoRusRamp <- colorRampPalette(c('#2346c7','#ffffb3','#008040','#fcba03','#780000','#69300d', '#fe7c97', '#680459'))


# choose working directory with all orthoimages
in.dir <- choose.dir(caption = "Select input tif folder")

# choose output directory
out.dir <- choose.dir(caption = "Select output wav folder")

setwd(in.dir)

# list all .tif files in your orthoimage directory
fns <- list.files(in.dir, patter='.tif$'); print(fns)

# Choose your orthoimage, this is the place to include a loop if several orthoimages have to be analysed:

# for(i in length(fns)){
# f <- fns[i]
i <- 1
f <- fns[i]

g <- paste(sub("(.+)[.][^.]+$", "\\1", f), 'VegIndex.tif', sep='_')


# file path to the raster
fnr <- file.path(in.dir, f)

# open the orthoimage
r <- rast(fnr)


# Check orthoimage
plotRGB(r, stretch="lin")

# The orthoimage can have really high values for the empty parts of the raster, causing the plotRGB to show you a black image. Replace the max values by NA
# If the orthoimage is to huge and your computer cannot handle the max value replacement, do it for each layer (see below)
# else replace on the raster stack:
r[r==max(values(r))] <- NA

# check image again, it should be a correct rgb image
plotRGB(r, stretch="lin")

# Separate each layer:

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

# If the orthoimage is to huge and your computer cannot handle the max value replacement, do it for each layer
NAValue <- max(values(nir))
cat(NAValue, '\n')
rd[rd == NAValue] <- NA
gr[gr == NAValue] <- NA
bl[bl == NAValue] <- NA
re[re == NAValue] <- NA
nir[nir == NAValue] <- NA

# Check false color orthoimage
rfc <- c(nir, rd, gr)
plotRGB(rfc, stretch="lin")


# check each single layer:
rd
gr
bl
re
nir

# Compute different vegettion index:

# NDVI, with the NIR and red layers
ndvi <- (nir-rd)/(nir+rd)

# NDRE, with the NIR and the red edge layer, better with a dense canopy, this index has a better penetration coefficient
ndre <- (nir-re)/(nir+re)

# GNDVI, with the NIR and green layer, better for late vegetation stage
gndvi <- (nir-gr)/(nir+gr)

# combine all layers to expend the -1 -> 0 values to -3 -> 0 value. It allow to refine the scale and facilitate the detection of ponds
allvi <- exp(ndvi)+exp(ndre)+exp(gndvi)

# remove the values higher than the 5% quantile, this schould remove most of the non water pixels (shadow, roads, ...)
dt <- values(allvi)
allvi[allvi>quantile(dt, 0.05, na.rm=T)] <- NA


# Stack all the layers, and give them a new name
s <- c(rd,gr,bl,re,nir,ndvi, ndre, gndvi, allvi)
names(s) <- c('rd','gr','bl','re','nir','ndvi', 'ndre', 'gndvi', 'allvi')

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

bot$sendDocument(chat_id = 'YOUR_CHAT_ID_FROM_TELEGRAM', document = jpegName)

rName <- file.path(out.dir, g)
system.time(writeRaster(s, rName, overwrite=TRUE))

# Close your bracket if you made a loop through several orthoimages
# }
