Sys.setenv(TZ='UTC')
rm(list=ls())


#get proj4 for LV03 or LV95 for any projection map or spatial analysis requireng a projection if spatial data analysis is required

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
  #stat libraries
  'data.table','stats',

  #Spatial libraries
  'sp','raster','sf','proj4','rgeos',
  'maptools','rgdal'
  )


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



### Check the desire elevation model under
# https://www.swisstopo.admin.ch/fr/geodata/height.html

### Download the csv file from the swisstopo website with the url to the zip files (1 zip file per km2)


#choose working directory with csv file -- metadata with .zip url
in.dir <- choose.dir(caption = "Select input folder")

#choose copying directory
out.dir <- choose.dir(caption = "Select output folder")

setwd(out.dir)

### open metadata file with download url

csvName <- file.choose()
dt <- read.csv(csvName, header=F, sep=',')
head(dt)


### open previous data frame if the download was interrupted

# i <- which(is.na(dt$fileName))
# dt <- dt[i,]

### create an empty dataframe
# dem <- read.csv(text='X, Y, Z')


for(i in 1:nrow(dt)){
  url <- dt[i,1]
  
  ### get url and path to folder
  zipName <- basename(url)
  zipDst <- file.path(out.dir, zipName)

  ### Download file
  download.file(url, zipDst)
  dt[i,'fileName'] <- zipName
  dt[i,'filePath'] <- file.path(out.dir,zipName)
  

  ### unzip file
  unzip(zipDst, exdir=out.dir)
  
  ### Save imported filename in a metadata dataframe
  fileName <-  unzip(zipDst, list=T)[,1]
  dt[i,'fileName'] <- fileName
  

  
  ### paste data to dataframe

  ### ### ###  /!\ only if the dataset is not too large, the final dataset can reach up to 700 Go  ### ### ###  
  
  # dtf <- fread(file.path(out.dir,fileName), sep=' ', header=T)
  # head(dtf); tail(dtf)
  # names(dtf) <- c('X', 'Y', 'Z')
  # dem <- rbind(dem, dtf)
  
  ### remove zip and data file to save HD space
  # file.remove(zipName)
  # file.remove(fileName)
}


  # write.csv(dem, file.path(out.dir, 'data.csv'), row.names=F)
  write.csv(dt, file.path(out.dir, 'metadata.csv'), row.names=F)
