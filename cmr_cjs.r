#########################################################
#########################################################
# © eRey.ch | bioGIS; erey@biogis.ch
#2015.09.25
#source('./CMR/cmr_cjs.r')

# Set your working directory as a directory with all your cmr csv files with, minimal and in the following order:
# all capture at x occasion, named as X2000a, X2000b, ...
# Species
# Sex
# Summary of the captures per year coded in 0-1 and named X2000, X2001, X2003, ...

# the analysis go through 3 loops:
# 1-. through all csv files
# 2-. through all species
# 3-. for eaxh sex, Male and Female

# It compute 3 models:
# a-. constant effect
# b-. sex effect
# c-. effort & sex effect
# ...-. Possible to add other (like effort^2 == behavior or effect of capture, ...)

# Let the scipt work then.

#########################################################
#########################################################
# Have Fun

rm(list=ls())

packages <- c('mra','Rcapture','stringr')


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

################################################
#set your working directory
#setwd('g:/CMR')
#setwd('/Volumes/Bat/CMR')
setwd('/Volumes/Data/PhD/R/CMR')


#list all csv files
fn <- file.path(getwd())
fns <- list.files(fn,pattern='.csv');print(fns)

#prepare empty data frame to receive all results
result <- read.csv(text='name,sp,sex,NumbBat, bat.cjs.cst, bat.cjs.cst.StdErr, chi.cst, df.cst, pvalue.cst, bat.cjs.Sx, bat.cjs.Sx.StdErr, chi.Sx, df.Sx, pvalue.Sx, bat.cjs.SxEff, bat.cjs.SxEff.StdErr, chi.SxEff, df.SxEff, pvalue.SxEff')
dim(result)

#loop through all csv files listed in fns, open each csv
print('1-. Loop 1, opening each csv file')
for(csv in fns){
  #Import csv
  dtf <- read.csv(csv,sep=';',header=T);print(paste(csv,'imported',sep=' '))
  name <- strsplit(csv,split='[.]')[[1]][1];print(name)
  print(dim(dtf))
  print(names(dtf))
  
  spLim <- which(names(dtf)=='species')-1
  listYrs <- names(dtf)[2:spLim]
  cntYrs <- as.data.frame(table(unlist(str_split(listYrs,'[Xabcdef]'))));cntYrs
  
  #set effort vector
  effort <- log(cntYrs$Freq[-1]);plot(effort)
  
  #set intervall between capture (==years)
  yrs <- as.numeric(as.vector(cntYrs$Var1[-1]));yrs
  int <- c()
  for(i in 1:length(yrs)){
    diff <- yrs[i+1]-yrs[i]
    int <- c(int,diff)
  }
  int <- int[-(which(is.na(int)))]

  #list all species captured
  listSp <- as.character(unique(dtf$sp));print(listSp)

  #subset the data of the given species
  print('2-. Loop 2, Select each species')
  for(sp in listSp){
    print(sp)
    genreName <- strsplit(sp,split=' ')[[1]][1]
    spName <- strsplit(sp,split=' ')[[1]][2]
    
    batSp<-subset(dtf,species==sp)
    batSp.M<-subset(dtf,species==sp & sex=='M')
    batSp.F<-subset(dtf,species==sp & sex=='F')
    
    if(dim(batSp)[1]>1){
    
    print(unique(batSp$sp));print(dim(batSp))

    #limit the dataset to the capture-recapture events
    if(csv=='baume_barree.csv'){
      # capture in 2014, Baume barree
      print(paste('building matrix for cmr analysis for',csv,sp,sep=' '))
      spLim <- which(names(dtf)=='species')-1
      bat<-as.matrix(batSp[,2:spLim])
      # Gouffre Baume Barree; fns[1], yrs as doys
      yrs <- c(230,240,249,259);print(length(yrs))
      effort <- log(c(1,2,3,4));print(length(effort))
      int <- c(1,1,1)
      }
    
    if(csv=='Cernil_Ladame.csv'){
      #Annual data Cernil Ladame
      print(paste('building matrix for cmr analysis for',csv,sp,sep=' '))
      bat<-as.matrix(batSp[,44:51]);bat
      # Gouffre Cernil Ladame; fns[2]:
      # For Cernile Ladame: Column 7 == 2009
      # dtf[,7] <- list(NULL)
    }

    if(csv=='Grand_murin.csv'){
      #Annual data Grand murins
      print(paste('building matrix for cmr analysis for',csv,sp,sep=' '))
      bat<-as.matrix(batSp[,30:34]);bat
      # Gouffre Grand Murin; fns[3]:
      # For Grand-Murin: Column 13 == 2014
      # dtf[,13] <- list(NULL)
    }

    if(csv=='La_Baume.csv'){
      #Annual data La Baume
      print(paste('building matrix for cmr analysis for',csv,sp,sep=' '))
      batSp[,30] <- list(NULL)
      bat<-as.matrix(batSp[,29:33]);bat
      # Gouffre La Baume; fns[4]:
      # For La Baume: Column 2,12 == 2009 & 2014
      # dtf[,c(2,12)] <- list(NULL)
    }

    if(csv=='Mines_craie.csv'){
      #Annual data Mines Craie
      print(paste('building matrix for cmr analysis for',csv,sp,sep=' '))
      bat<-as.matrix(batSp[,24:27]);bat
      # Gouffre Mines craie; fns[5]:
      # For Mines Craie: Column 2,10 == 2011 & 2014; 
      # and if analysis on 2013 dataset: Column 2,3,4,10 == 2011, 2012 & 2014
      # dtf[,c(2:4,10)] <- list(NULL)
    }

    if(csv=='Pertuis.csv'){
      #Annual data Pertuis
      print(paste('building matrix for cmr analysis for',csv,sp,sep=' '))
      bat<-as.matrix(batSp[,71:78]);bat
      # Gouffre Pertuis; fns[6]:
    }

 
    #Compute sex, effort and time matrix
    nan <- dim(bat)[1];print(nan)
    ns <- dim(bat)[2];print(ns)
    
    print('effort matrix')
    attr(effort,'nan') <- nan
    attr(effort,'drop.levels') <- ns

    effortvar <- tvar(effort)
    
    #time as tvar
    print('time matrix')
    timevarcap <-  tvar(as.factor(1:ncol(bat)),nrow(bat),c(1,2))
    timevarsurv <-  tvar(as.factor(1:ncol(bat)),nrow(bat),c(1,ns))
    

    # sex effect as ivar
    print('sex matrix')
    batSx <- batSp$sex
    print(dim(bat))

    attr(batSx,'ns') <- ns
    #with drop=1, Male are the references, drop=2, Female are the reference
      
    print('3-. Loop 3, Select all, males and females for cjs cmr models')
    drp <- c('Male','Female')
    Sx <- c('M','F')

    for(i in 1:length(drp)){
      mf <- drp[i]
      print(mf)
      nan <- dim(subset(dtf,species==sp & sex==Sx[i]))[1]

      modelling <- paste('compute Cormak-Jolly-Seber Model for',csv,sp,mf,sep=' ')
      print(modelling)
      #Cormak-Jolly-Seber Model for open populations
      ## Method 2 : same thing using 2-d matrices
      xy <- F.cjs.covars( nrow(bat), ncol(bat));dim(bat)

      # The following extracts 2-D matrices of 0s and 1s
      for(j in 1:ncol(bat)){ assign(paste("x",j,sep=""), xy$x[,,j]) }
      
      print('##############################################################')
      print('##############################################################')
      print('model with constant effect')
      testModel <- try(F.cjs.estim( capture=~1, survival=~timevarsurv, histories = bat, intervals = int),silent=T)
      if(!class(testModel)=='try_error'){
      	
          if(any(ls()=='timevarsurv')){
          bat.cjs.cst.model <- F.cjs.estim( capture=~1, survival=~timevarsurv, histories = bat, intervals = int);print(bat.cjs.cst.model)
          
          if(!median(bat.cjs.cst.model$n.hat,na.rm=T)==0){
            cjs.test.cst.model <- F.cjs.gof(bat.cjs.cst.model);print(cjs.test.cst.model)
            bat.cjs.cst<- median(bat.cjs.cst.model$n.hat,na.rm=T)
            bat.cjs.cst.StdErr<- median(bat.cjs.cst.model$se.n.hat,na.rm=T)
            chi.cst <- cjs.test.cst.model$gof.chi
            df.cst <- cjs.test.cst.model$gof.df
            pvalue.cst<- cjs.test.cst.model$gof.pvalue
          }
          else{
            bat.cjs.cst<- median(bat.cjs.cst.model$n.hat,na.rm=T)
            bat.cjs.cst.StdErr<- median(bat.cjs.cst.model$se.n.hat,na.rm=T)
            chi.cst <- NA
            df.cst <- NA
            pvalue.cst<- NA
          }
          }
      }

        if(!any(ls()=='bat.cjs.cst.model')){
        bat.cjs.cst<- NA
        bat.cjs.cst.StdErr<- NA
        chi.cst <- NA
        df.cst <- NA
        pvalue.cst<- NA
        }
      
      print('##############################################################')
      print('##############################################################')
      print('model with sex effect')
        if(length(unique(batSx))>1){
          sexvar <- ivar(batSx,ns,drop=i)
        
        testModel <- try(F.cjs.estim( capture=~-1+sexvar, survival=~timevarsurv, histories = bat, intervals = int),silent=T)
        if(!class(testModel)=='try_error'){
          
          if(any(ls()=='timevarsurv') & any(ls()=='sexvar')){
            bat.cjs.Sx.model <- F.cjs.estim( capture=~-1+sexvar, survival=~timevarsurv, histories = bat, intervals = int);print(bat.cjs.Sx.model)

                        if(!median(bat.cjs.Sx.model$n.hat,na.rm=T)==0){
                          cjs.test.Sx.model <- F.cjs.gof(bat.cjs.Sx.model);print(cjs.test.Sx.model)
                          bat.cjs.Sx<- median(bat.cjs.Sx.model$n.hat,na.rm=T)
                          bat.cjs.Sx.StdErr<- median(bat.cjs.Sx.model$se.n.hat,na.rm=T)
                          chi.Sx <- cjs.test.Sx.model$gof.chi
                          df.Sx <- cjs.test.Sx.model$gof.df
                          pvalue.Sx<- cjs.test.Sx.model$gof.pvalue
                          }
            else{
              bat.cjs.Sx<- median(bat.cjs.Sx.model$n.hat,na.rm=T)
              bat.cjs.Sx.StdErr<- median(bat.cjs.Sx.model$se.n.hat,na.rm=T)
              chi.Sxt <- NA
              df.Sx <- NA
              pvalue.Sx<- NA
            }
          }
        }
      
        if(!any(ls()=='bat.cjs.Sx.model')){
          bat.cjs.Sx<- NA
          bat.cjs.Sx.StdErr<- NA
          chi.Sx <- NA
          df.Sx <- NA
          pvalue.Sx<- NA
          }
      
      
        print('##############################################################')
        print('##############################################################')
        print('model with capture effort and sex effect')
        testModel <- try(F.cjs.estim( capture=~effortvar-sexvar, survival=~timevarsurv, histories = bat, intervals = int),silent=T)
        if(!class(testModel)=='try_error'){
          if(any(ls()=='timevarsurv') & any(ls()=='sexvar') & any(ls()=='effortvar')){
            bat.cjs.SxEff.model <- F.cjs.estim( capture=~effortvar-sexvar, survival=~timevarsurv, histories = bat, intervals = int);print(bat.cjs.SxEff.model)
            
            if(!median(bat.cjs.SxEff.model$n.hat,na.rm=T)==0){
              cjs.test.SxEff.model <- F.cjs.gof(bat.cjs.SxEff.model);print(cjs.test.SxEff.model)
              bat.cjs.SxEff<- median(bat.cjs.SxEff.model$n.hat,na.rm=T)
              bat.cjs.SxEff.StdErr<- median(bat.cjs.SxEff.model$se.n.hat,na.rm=T)
              chi.SxEff <- cjs.test.SxEff.model$gof.chi
              df.SxEff <- cjs.test.SxEff.model$gof.df
              pvalue.SxEff<- cjs.test.SxEff.model$gof.pvalue
        }
        else{
          bat.cjs.SxEff<- median(bat.cjs.SxEff.model$n.hat,na.rm=T)
          bat.cjs.SxEff.StdErr<- median(bat.cjs.SxEff.model$se.n.hat,na.rm=T)
          chi.SxEff <- NA
          df.SxEff <- NA
          pvalue.SxEff<- NA
        }
        }
        }
        
        if(!any(ls()=='bat.cjs.SxEff.model')){
          bat.cjs.SxEff<- NA
          bat.cjs.SxEff.StdErr<- NA
          chi.SxEff <- NA
          df.SxEff <- NA
          pvalue.SxEff<- NA
        }
      
      rm(sexvar)

      
      cjs <- data.frame('name' = name,
                        'sp' = sp,
                        'Sex' = mf,
                        'NumbBat' = nan,
                        bat.cjs.cst,
                        bat.cjs.cst.StdErr,
                        chi.cst,
                        df.cst,
                        pvalue.cst,
                        bat.cjs.Sx,
                        bat.cjs.Sx.StdErr,
                        chi.Sx,
                        df.Sx,
                        pvalue.Sx,
                        bat.cjs.SxEff,
                        bat.cjs.SxEff.StdErr,
                        chi.SxEff,
                        df.SxEff,
                        pvalue.SxEff)

      print('')
      print('add model results to result data frame')
      result <- rbind(result,cjs)
      # print(summary(result))
      print(paste('nrow result: ',dim(result)[1],sep=' '))

      

      plotTest <- try(plot(bat.cjs.cst.model,main=paste(name,sp, mf, 'constant effect',sep='; '),cex.axis=0.8),silent=T)
      if(!class(plotTest)=='try-error'){
        pdfName <- paste(paste(name,genreName,spName,mf,'cste',sep='_'),'pdf',sep='.')
        pdf(pdfName,paper='a4r',width=11,height=8.5)
        plot(bat.cjs.cst.model,main=paste(name,sp, mf, 'constant effect',sep='; '),cex.axis=0.8)
        dev.off()
      }
      plotTest <- try(plot(bat.cjs.Sx.model,main=paste(name,sp, mf,'sex effect',sep='; '),cex.axis=0.8),silent=T)
      if(!class(plotTest)=='try-error'){
        pdfName <- paste(paste(name,genreName,spName,mf,'Sex',sep='_'),'pdf',sep='.')
        pdf(pdfName,paper='a4r',width=11,height=8.5)
        plot(bat.cjs.Sx.model,main=paste(name,sp, mf, 'Sex effect',sep='; '),cex.axis=0.8)
        dev.off()
      }
      
      plotTest <- try(plot(bat.cjs.SxEff.model,main=paste(name,sp, mf, 'sex and effort effect',sep='; '),cex.axis=0.8),silent=T)
      if(!class(plotTest)=='try-error'){
        pdfName <- paste(paste(name,genreName,spName,mf,'SexEff',sep='_'),'pdf',sep='.')
        pdf(pdfName,paper='a4r',width=11,height=8.5)
        plot(bat.cjs.SxEff.model,main=paste(name,sp, mf, 'Sex & Effort effect',sep='; '),cex.axis=0.8)
        dev.off()
        }
      }
    }
  }
  }
}



write.csv(result,'result.cjs.NE.csv',row.names=F)


print('done, go drink a coffee')


