#########################################################
#########################################################
# @ eRey.ch | bioGIS; erey@biogis.ch
#source_https('https://raw.githubusercontent.com/biogis/r/master/shp2kml')
# 
# #https://tonybreyal.wordpress.com/2011/11/24/source_https-sourcing-an-r-script-from-github/
# source_https <- function(url, ...) {
#   # load package
#   require(RCurl)
#   
#   # parse and evaluate each .R script
#   sapply(c(url, ...), function(u) {
#     eval(parse(text = getURL(u, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))), envir = .GlobalEnv)
#   })
# }


#import libraries
require(maptools)
require(rgdal)

#set CRS, CH1903 and WGS84
lv03 <- '+proj=somerc +lat_0=46.95240555555556 +lon_0=7.439583333333333 +k_0=1 +x_0=600000 +y_0=200000 +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs '
wgs84 <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

#set working directory
setwd(choose.dir())

#list all shp
fn <- file.path(getwd())
fns <- list.files(fn,pattern='.shp',full.names=T);fns

for(i in c(1:length(fns))){print(i)
#import shapefile, set projection, plot
shp <- readShapeSpatial(fns[i]);str(shp)
proj4string(shp) <- CRS(lv03)
plot(shp)

#project to wgs84
shp_wgs84<- spTransform(shp, CRS(wgs84))

nameKML <- paste(sub("(.+)[.][^.]+$", "\\1", basename(fns[i])),'kml',sep='.');nameKML
nameLAYER <-  paste(sub("(.+)[.][^.]+$", "\\1", basename(fns[i])),'layer',sep='_');nameLAYER

#write kml file
writeOGR(shp_wgs84, dsn=nameKML, layer= nameLAYER, driver="KML", dataset_options=c("NameField=name"),overwrite_layer = T)
}
