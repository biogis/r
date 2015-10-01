#########################################################
#########################################################
# © eRey.ch | bioGIS; erey@biogis.ch
# created on 2015.09.25
# modified on 2015.10.01
# source('./CMR/cmr_cjs.r')
# source('/Volumes/Data/PhD/R/CMR/cmr_cjs.r')
# https://github.com/biogis/r/blob/master/cmr_cjs.r

# Set your working directory as a directory with all your cmr csv files with, minimal and in the following order:
# all capture at x occasion, named as 2000a, 2000b, ...
# Species
# Sex
# Summary of the captures per year coded in 0-1 and named 2000, 2001, 2003, ...

# the analysis go through 3 loops:
# 1-. through all csv files
# 2-. through all species
# 3-. for each sex, Male and Female

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
#setwd('d:/CMR')
#setwd('/Volumes/Bat/CMR')
setwd('/Volumes/Data/PhD/R/CMR')


#list all csv files
fn <- file.path(getwd())
fns <- list.files(fn,pattern='.csv');print(fns)

fns <- c('baume_barree.csv','Cernil_Ladame.csv','Grand_murin.csv','La_Baume.csv','Mines_craie.csv','Pertuis.csv')

#prepare empty data frame to receive all results
result <- read.csv(text='name,sp,sex,NumbBat,avg.n,avg.se,bat.cjs.cst,bat.cjs.cst.StdErr,qaicc.cst,roc.cst,chi.cst,df.cst,pvalue.cst,bat.cjs.Sx,bat.cjs.Sx.StdErr,qaicc.Sx,roc.Sx,chi.Sx,df.Sx,pvalue.Sx,bat.cjs.SxEff,bat.cjs.SxEff.StdErr,qaicc.SxEff,roc.SxEff,chi.SxEff,df.SxEff,pvalue.SxEff')
dim(result)

resum <- read.csv(text='ID,model.num,model.name,converged,n.est.parameters,n.coefficients,loglike,aicc,delta.aicc,aicc.wgt,qaicc,delta.qaicc,qaicc.wgt,plausible')


avg.n <- NA
avg.se <- NA
bat.cjs.cst <- NA
bat.cjs.cst.StdErr <- NA
qaicc.Cst <- NA
roc.Cst <- NA
chi.cst <- NA
df.cst <- NA
pvalue.cst <- NA
bat.cjs.Sx <- NA
bat.cjs.Sx.StdErr <- NA
qaicc.Sx <- NA
roc.Sx <- NA
chi.Sx <- NA
df.Sx <- NA
pvalue.Sx <- NA
bat.cjs.SxEff <- NA
bat.cjs.SxEff.StdErr <- NA
qaicc.SxEff <- NA
roc.SxEff <- NA
chi.SxEff <- NA
df.SxEff <- NA
pvalue.SxEff <- NA




####################################################################
#loop through all csv files listed in fns, open each csv
print('1-. Loop 1, opening each csv file')
for(csv in fns){
  # csv <- fns[6];csv
  #Import csv
  dtf <- read.csv(csv,sep=',',header=T);print(paste(csv,'imported',sep=' '))
  name <- strsplit(csv,split='[.]')[[1]][1];print(name)
  print(dim(dtf))
  print(names(dtf))
  
  spLim <- which(names(dtf)=='species')-1
  listYrs <- names(dtf)[2:spLim]
  cntYrs <- as.data.frame(table(unlist(str_split(listYrs,'[Xabcdef]'))));cntYrs
  
  #set effort vector
  effort <- log(cntYrs$Freq[-1]);plot(effort,main=paste('capture effort at:', csv,sep=' '))
  
  #set intervall between capture (==years)
  yrs <- as.numeric(as.vector(cntYrs$Var1[-1]));yrs
  yrsLim <- paste('X',yrs,sep='')
  yrBegin <- which(names(dtf)==yrsLim[1])
  yrEnd <- which(names(dtf)==yrsLim[length(yrsLim)])
  
  int <- c()
  for(i in 1:length(yrs)){
    diff <- yrs[i+1]-yrs[i]
    int <- c(int,diff)
  }
  int <- int[-(which(is.na(int)))]
  print(int)
  
  
  ####################################################################
  #list all species captured
  listSp <- as.character(unique(dtf$sp));print(listSp)
  
  
  ##################################
  #subset the data of the given species
  print('2-. Loop 2, Select each species')
  for(sp in listSp){
    # sp <- listSp[1]
    print(sp)
    genreName <- strsplit(sp,split=' ')[[1]][1]
    spName <- strsplit(sp,split=' ')[[1]][2]
    
    batSp<-subset(dtf,species==sp)
    batSp.M<-subset(dtf,species==sp & sex=='M')
    batSp.F<-subset(dtf,species==sp & sex=='F')
    
    if(dim(batSp)[1]>1){
      
      print(unique(batSp$sp));print(dim(batSp))
      
      print(paste('building matrix for cmr analysis for',csv,sp,sep=' '))
      
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
      
      if(!csv=='baume_barree.csv'){
        bat<-as.matrix(batSp[,yrBegin:yrEnd]);bat
      }
      
      
      ##################################
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
      
      ####################################################################  
      #with drop=1, Male are the references, drop=2, Female are the reference
      print('3-. Loop 3, Select all, males and females for cjs cmr models')
      drp <- c('Male','Female')
      Sx <- c('M','F')
      
      for(i in 1:length(drp)){
        # i <- 1
        mf <- drp[i]
        print(mf)
        nan <- dim(subset(dtf,species==sp & sex==Sx[i]))[1]
        
        modelling <- paste('compute Cormak-Jolly-Seber Model for',csv,sp,mf,sep=' ')
        print(modelling)
        
        
        ##################################
        #Cormak-Jolly-Seber Model for open populations
        ## Method 2 : same thing using 2-d matrices
        xy <- F.cjs.covars( nrow(bat), ncol(bat));dim(bat)
        
        # The following extracts 2-D matrices of 0s and 1s
        for(j in 1:ncol(bat)){ assign(paste("x",j,sep=""), xy$x[,,j]) }
        
        
        ##################################
        #Constant Model
        print('##############################################################')
        print('##############################################################')
        print('model with constant effect')
        
        #test model
        testModel <- try(F.cjs.estim( capture=~1, survival=~timevarsurv, histories = bat, intervals = int, group = batSp$sex),silent=T)
        if(!class(testModel)[1]=='try_error'){
          
          #check validity and presence of effect and survival matrix, then compute modelling
          if(any(ls()=='timevarsurv')){
            bat.cjs.cst.model <- F.cjs.estim( capture=~1, survival=~timevarsurv, histories = bat, intervals = int, group = batSp$sex);print(bat.cjs.cst.model)
            print(bat.cjs.cst.model$message)
            
            #save median values of model and goodness of fit
            if(!median(bat.cjs.cst.model$n.hat,na.rm=T)==0){
              cjs.test.cst.model <- F.cjs.gof(bat.cjs.cst.model);print(cjs.test.cst.model)
              bat.cjs.cst<- median(bat.cjs.cst.model$n.hat,na.rm=T)
              bat.cjs.cst.StdErr<- median(bat.cjs.cst.model$se.n.hat,na.rm=T)
              qaicc.cst <- bat.cjs.cst.model$qaicc
              roc.cst <- cjs.test.cst.model$roc
              chi.cst <- cjs.test.cst.model$gof.chi
              df.cst <- cjs.test.cst.model$gof.df
              pvalue.cst<- cjs.test.cst.model$gof.pvalue
              # close condition save values to dtf
            }
            #close condition modelling cjs
          }
          #close condition try tes modelling
        }
        
        
        
        ##################################
        #Sex effect Model
        print('##############################################################')
        print('##############################################################')
        print('model with sex effect')
        if(length(unique(batSx))>1){
          sexvar <- ivar(batSx,ns,drop=i)
          
          #test model
          testModel <- try(F.cjs.estim( capture=~-1+sexvar, survival=~timevarsurv, histories = bat, intervals = int),silent=T)
          if(!class(testModel)[1]=='try_error'){
            
            #check validity and presence of effect and survival matrix, then compute modelling
            
            if(any(ls()=='timevarsurv') & any(ls()=='sexvar')){
              bat.cjs.Sx.model <- F.cjs.estim( capture=~-1+sexvar, survival=~timevarsurv, histories = bat, intervals = int);print(bat.cjs.Sx.model)
              print(bat.cjs.Sx.model$message)
              
              if(!median(bat.cjs.Sx.model$n.hat,na.rm=T)==0){
                cjs.test.Sx.model <- F.cjs.gof(bat.cjs.Sx.model);print(cjs.test.Sx.model)
                bat.cjs.Sx<- median(bat.cjs.Sx.model$n.hat,na.rm=T)
                bat.cjs.Sx.StdErr<- median(bat.cjs.Sx.model$se.n.hat,na.rm=T)
                qaicc.Sx <- bat.cjs.Sx.model$qaicc
                roc.Sx <- cjs.test.Sx.model$roc
                chi.Sx <- cjs.test.Sx.model$gof.chi
                df.Sx <- cjs.test.Sx.model$gof.df
                pvalue.Sx<- cjs.test.Sx.model$gof.pvalue
                # close condition save values to dtf
              }
              #close condition modelling cjs
            }
            #close condition try test modelling
          }
          
          
          ##################################
          #Sex and Effort Model
          print('##############################################################')
          print('##############################################################')
          print('model with capture effort and sex effect')
          
          #test model
          testModel <- try(F.cjs.estim( capture=~effortvar-sexvar, survival=~timevarsurv, histories = bat, intervals = int, group = batSp$sex),silent=T)
          if(!class(testModel)[1]=='try_error'){
            
            #check validity and presence of effect and survival matrix, then compute modelling
            if(any(ls()=='timevarsurv') & any(ls()=='sexvar') & any(ls()=='effortvar')){
              bat.cjs.SxEff.model <- F.cjs.estim( capture=~effortvar-sexvar, survival=~timevarsurv, histories = bat, intervals = int, group = batSp$sex);print(bat.cjs.SxEff.model)
              print(bat.cjs.SxEff.model$message)
              
              #save median values of model and goodness of fit
              if(!median(bat.cjs.SxEff.model$n.hat,na.rm=T)==0){
                cjs.test.SxEff.model <- F.cjs.gof(bat.cjs.SxEff.model);print(cjs.test.SxEff.model)
                bat.cjs.SxEff<- median(bat.cjs.SxEff.model$n.hat,na.rm=T)
                bat.cjs.SxEff.StdErr<- median(bat.cjs.SxEff.model$se.n.hat,na.rm=T)
                qaicc.SxEff <- bat.cjs.SxEff.model$qaicc
                roc.SxEff <- cjs.test.SxEff.model$roc
                chi.SxEff <- cjs.test.SxEff.model$gof.chi
                df.SxEff <- cjs.test.SxEff.model$gof.df
                pvalue.SxEff<- cjs.test.SxEff.model$gof.pvalue
                # close condition save values to dtf
              }
              #close condition modelling cjs
            }
            #close condition try tes modelling
          }

        
        ##################################
        #weighted average model
          
          modelList <- c('bat.cjs.cst.model',
                         'bat.cjs.Sx.model',
                         'bat.cjs.SxEff.model')
          
          modelSummary <- F.fit.table(fits=modelList, rank.by= "qaicc", plausible.p=0.01)
          print(modelSummary)
#          summaryName <- paste(paste(name,genreName,spName,mf,'Summary',sep='_'),'csv',sep='.')
#          write.csv(modelSummary,summaryName)
          ID <- paste(name,genreName,spName,mf,sep='_');ID
          modelSummary$ID <- ID
          resum <- rbind(resum,modelSummary)
          
          if(!any(modelSummary$converged==FALSE)){
          	plausibleIndex <- which(modelSummary$plausible==T);plausibleIndex
          	modelList <- modelSummary$model.name[plausibleIndex];modelList
          	mod.avg.n <- F.cr.model.avg(fits=modelList,what="n.hat", fit.stat="qaicc" )
          	print(mod.avg.n)
          	avg.n <- median(mod.avg.n$n.hat,na.rm=T)
          	avg.se <- median(mod.avg.n$se.n.hat,na.rm=T)
          	avgList <- c(avg.n,avg.se);print(avgList)
          	if(!any(is.na(avgList))){
          		## Plot
          		maxYLim <- max(mod.avg.n$n.hat,na.rm=T)+max(mod.avg.n$se.n.hat,na.rm=T)
          		minYLim <- 0
          		pdfName <- paste(paste(name,genreName,spName,mf,'avg',sep='_'),'pdf',sep='.')
          		pdf(pdfName,paper='a4r',width=11,height=8.5)
          		plot(mod.avg.n,ylim=c(minYLim,maxYLim),main = paste('model average on qaicc for:',name,sp,mf,sep=' '))
          		dev.off()
          		#close condition plot average
          		}
          		#close average model condition
          		}

                  
        
        ##################################
        #Result data frame
        
        cjs <- data.frame('name' = name,
                          'sp' = sp,
                          'Sex' = mf,
                          'NumbBat' = nan,
                          avg.n,
                          avg.se,
                          bat.cjs.cst,
                          bat.cjs.cst.StdErr,
                          qaicc.cst,
                          roc.cst,
                          chi.cst,
                          df.cst,
                          pvalue.cst,
                          bat.cjs.Sx,
                          bat.cjs.Sx.StdErr,
                          qaicc.Sx,
                          roc.Sx,
                          chi.Sx,
                          df.Sx,
                          pvalue.Sx,
                          bat.cjs.SxEff,
                          bat.cjs.SxEff.StdErr,
                          qaicc.SxEff,
                          roc.SxEff,
                          chi.SxEff,
                          df.SxEff,
                          pvalue.SxEff)
        
        print('')
        print('add model results to result data frame')
        result <- rbind(result,cjs)
        # print(summary(result))
        print(paste('nrow result: ',dim(result)[1],sep=' '))
        
        
        ##################################
        #Plot models
        
        if(!class(try(plot(bat.cjs.cst.model),silent=T))=='try-error'){
          pdfName <- paste(paste(name,genreName,spName,mf,'cste',sep='_'),'pdf',sep='.')
          pdf(pdfName,paper='a4r',width=11,height=8.5)
          plot(bat.cjs.cst.model,main=paste(name,sp, mf, 'constant effect',sep='; '),cex.axis=0.8)
          dev.off()
          #close condition plot cst model
        }
        if(!class(try(plot(bat.cjs.Sx.model),silent=T))=='try-error'){
          pdfName <- paste(paste(name,genreName,spName,mf,'Sex',sep='_'),'pdf',sep='.')
          pdf(pdfName,paper='a4r',width=11,height=8.5)
          plot(bat.cjs.Sx.model,main=paste(name,sp, mf, 'Sex effect',sep='; '),cex.axis=0.8)
          dev.off()
          #close condition plot Sex model
        }
        
        if(!class(try(plot(bat.cjs.SxEff.model),silent=T))=='try-error'){
          pdfName <- paste(paste(name,genreName,spName,mf,'SexEff',sep='_'),'pdf',sep='.')
          pdf(pdfName,paper='a4r',width=11,height=8.5)
          plot(bat.cjs.SxEff.model,main=paste(name,sp, mf, 'Sex & Effort effect',sep='; '),cex.axis=0.8)
          dev.off()
          #close condition plot SexEff model
        }
        #close sexvar loop
        }
          rm(sexvar,
             bat.cjs.cst.model,bat.cjs.Sx.model,bat.cjs.SxEff.model,
             cjs.test.cst.model,cjs.test.Sx.model,cjs.test.SxEff.model,
             mod.avg.n)
        # close Loop 3: sex
      }
      #close condition number of captured bat > 1
    }
    #close Loop 2; sp
  }
  #close Loop 1: site
}


write.csv(result,'result.cjs.csv',row.names=F)
write.csv(resum,'model.summary.cjs.csv',row.names=F)


print('done, go drink a coffee')
