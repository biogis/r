#########################################################
#########################################################
# © eRey.ch | bioGIS; erey@biogis.ch
# created on 2019.04.16
# modified on 2022.06.24
# source('./Rhi_Finder.r')
# https://github.com/biogis/r/blob/master/Rhi_Finder.r
# source('https://raw.githubusercontent.com/biogis/r/master/Rhi_Finder.r')


###########################################################
###########################################################

# Library list
packages <- c(
  #Acoustic libraries
  'tuneR','seewave',

  #Data library
  'data.table','matrixStats',

  #Looping libraries
  'foreach','iterators','parallel','doParallel',

  #graphics libraries
  'ggplot2','ggpubr','raster','patchwork', 'svDialogs')


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

# Import bat functions
source('https://raw.githubusercontent.com/biogis/r/master/batFunc.r')


"%notin%" <- Negate("%in%")


#Define how many cores you want to use
UseCores <- detectCores() -5
# UseCores <- 10

#Register CoreCluster
cl <- makeCluster(UseCores)
registerDoParallel(cl)


# # # # ## choose working directory with all wav files
in.dir <- choose.dir(caption = "Select input wav folder")
# in.dir <- dlgInput("Enter your path to your wav files directory", Sys.info()["in.dir"])$res
# # # #

setwd(in.dir)


# # # # ## choose copying directory
out.dir <- choose.dir(caption = "Select output wav folder")
# out.dir <- dlgInput("Enter your path to your output wav files directory", Sys.info()["out.dir"])$res


fn.f <- file.path(in.dir)
fns <- list.files(fn.f,pattern='.wav$',all.files=T,full.names=T,recursive=F,include.dirs=F) # store all wave files in the working directory
print(length(fns))

f <- fns[12]

begin <- Sys.time()
cat(begin, '\n')

# Loop to pass each wav file to the bat detector, using several cores. The wav file vector is divided in x-cores dataset
foreach(i=1:length(fns)) %dopar% {
  # call the functions, must be done within the loop each time
  source('https://raw.githubusercontent.com/biogis/r/master/batFunc.r')
  # cat(i, '\t')
  # Select wav file
  f <- fns[i]
  
  # Try to open and check if the file is readable
  a <- try(readWave(f),silent=T)
  if(class(a)!='try-error'){
    if(length(a@left)>0){
      # cat(basename(f), '\n')
      
      # Open the file, convert the values to a data frame, with frequency as a row and time as columns
      dt <- bat(f)
      
      # Clean the dataframe from the background noise
      dt <- cleanBat(dt)
      
      # Compute different statistics on the wav file, including a peak analysis and the frequency at those eaks
      L <- statBat(f,dt)
      
      # Loop to select peaks round 80 and 105 kHz to copy potential Rhinolophus sequences
      if (any((L$pks$freq > 75 & L$pks$freq < 85) | (L$pks$freq > 100 & L$pks$freq < 110))) {
        
        # Copy wav file in a selected location
        output <- paste(out.dir,basename(f),sep='/')
        file.copy(f,output)
        print(paste('bat, looks like to be a Rhino probably found in', basename(f), sep=' '))
        
        # Makes control graph and spectrogram of the wav file to visually check if a Rhinolophus species is there
        tk <- batlab(f)
        pp <- control(f, dt, L, out.dir)
        p <- graph(f, dt, L, pp, out.dir)


        # Frequency peaks savec as a csv file, one csv file per wav file
        # batFreq <- data.table('filename'=L$dt.wav$filename, 'PeakFreq'=L$pks$freq, 'Amp' = L$pks$amp)
        # csvName <- file.path(out.dir, paste(sub("(.+)[.][^.]+$", "\\1", L$dt.wav$filename),'Freq', 'csv', sep='.'))
        # write.csv(batFreq, csvName)
        
        # Remove everythin to prevent memory overload
        rm(tk, pp, output, p)
      }
      rm(dt, L)
    }
  }
}



end <- Sys.time()
cat(end, '\n')
cat(end-begin, '\n')
