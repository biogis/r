#########################################################
#########################################################
# © eRey.ch | bioGIS; erey@biogis.ch
# created on 2015.09.29
# source('./batseq.r')
# https://github.com/biogis/r/blob/master/batseq.r

# Set your working directory as a directory with all the batscope files you need to retriev the data

# 3 steps:
# 1-. copy all plist files in an external directory
# 2-. in Terminal, convert all copied plist in a true xml files
# 3-. Back in R, export all data from the newly converted xml files in a dataframe


#########################################################
#########################################################
# Have Fun

library('XML')
rm(list=ls())


#####################################################################################################################
#####################################################################################################################
# Step 1-. List all plist files in your batscope project directories, copy them to a scratch working directory

# Enter your Batscope directory where you need to retrieve informations from the .batseq sequences
#setwd('/Users/erey//Documents/BatscopeImport')
setwd('/Users/jerome/Music/acoustique/chiroptera/batlogger')
dir()

fn <- file.path(getwd())
fns <- list.files(fn,pattern='.batseq$',all.files=T,full.names=T,recursive=T,include.dirs=T);length(fns)


#Add the plist file to the fns path &
#Prepare the input list for copying plist files
plistFiles <- 'Contents/Resources/batsndinfo.plist'
input <- paste(fns,plistFiles,sep='/');print(input)

#Prepare output filenames to an xml directory on the desktop
name <- strsplit(basename(fns),split='[.]');print(name)
project <- strsplit(fns,split='/');unique(project)
print(length(name))
print(length(project))

output <- c()
for(i in 1:length(name)){
	filename <- paste('/Users/jerome/Desktop/xml',paste(project[[i]][length(project[[i]])-1],name[[i]][1], 'batsndinfo.plist',sep='_'),sep='/')
	output <- c(output, filename)
	print(filename)
	}

#check which plist file is missing
index <- which(file.exists(input)==FALSE);print(index)
for(i in index){print(input[i])}

if(!dir.exists('/Users/jerome/Desktop/xml')){dir.create('/Users/jerome/Desktop/xml')}

if(file.exists(input)==TRUE){file.copy(input,output)}




#####################################################################################################################
#####################################################################################################################
# Step 2-. Open terminal, follow instructions
# convert all plist files in xml in the mac terminal using the 2 following command lines:
# cd /Users/jerome/Desktop/xml
# for f in ./*.plist; do plutil -convert xml1 $f; echo $f; done


#####################################################################################################################
#####################################################################################################################
# Step 3-. Back in R: 

require('XML')
setwd('/Users/jerome/Desktop/xml')

fn <- file.path(getwd())
fns <- list.files(fn, pattern='^.*Neirivue*.*.plist$');fns
fns <- list.files(fn, pattern='.plist$');fns
length(fns)

dtf <- data.frame('keys' = c("BatRecDate","BatRecProject","BatSeqRawFile",
                             "FileName","BatSeqNotes","BatRecUser",
                             "BatSeqClassifiedVerified",
                             "BatSeqClassifiedFinalSpecies1",
                             "BatSeqClassifiedFinalSpecies2",
                             "BatSeqClassifiedFinalSpecies3",
                             "BatSeqClassifiedManualSpecies1",
                             "BatSeqClassifiedManualSpecies2",
                             "BatSeqClassifiedManualSpecies3"
                             ))


for(f in fns){
	doc <- xmlParse(f);print(doc)
	print('')
	print('')
	r <- xmlRoot(doc);print(r)
	print('')
	print('')
	print(r[[1]])
	print(xmlName(r[[1]]))
	print(xmlSize(r[[1]]))
	
	xmlStrct <- sapply(xmlChildren(r[[1]]),xmlName)
	keyIndex <- which(xmlStrct=='key');print(keyIndex)
	
	keys <- c()
	values <- c()
	for(i in keyIndex){
		k <- xmlValue(r[[1]][[i]]);print(k)
		keys <- c(keys,k)
		v <- xmlValue(r[[1]][[i+1]]);print(v)
		values <- c(values,v)
		}
# 	dtf.kv <- data.frame(t(values))
# 	names(dtf.kv) <- t(keys)
# 	names(dtf.kv)
# 	dtfName <- paste(strsplit(as.character(dtf.kv$FileName),split='[.]')[[1]][1],'csv',sep='.');print(dtfName)
#   write.csv(dtf.kv,dtfName,row.names=F)
  
  dtf.xml <- data.frame(keys,values)
  dtf <- merge(dtf,dtf.xml,by='keys',all=T)
print(dim(dtf)[2])
	}

names(dtf) <- c(1:length(dtf))
dtf.t <- t(dtf)
nameList <- dtf.t[1,]
dtf.t <- as.data.frame(dtf.t);class(dtf.t)
names(dtf.t) <- nameList
write.csv(dtf.t,'/Users/jerome/Desktop/BatscopeData.csv')
