#########################################################
#########################################################
# © eRey.ch | bioGIS; erey@biogis.ch
# created on 2019.04.24
# modified on 2019.04.24
#source('./Extract_xy_Moorland.r')
# https://github.com/biogis/r/blob/master/Extract_xy_Moorland.r

# Set your working directory

# 1-. in.dir, select the directory with wav files
# 2-. out.dir select the directory where the pdf files will be copied


###########################################################
###########################################################

# Launch via:

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
# source_https("https://raw.githubusercontent.com/biogis/r/master/Extract_xy_Moorland.r")




packages <- c('httr','pdftools','RCurl','data.table','openxlsx')


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




#choose working directory with all wav files
in.dir <- choose.dir(caption = "Select working directory")

setwd(in.dir)


ktList <- c('AG','AI','AR','BE','BL','BS','FR','GE','GL','GR','JU','LU','NE','NW','OW','SG','SH','SO','SZ','TI','TG','UR','VD','VS','ZG','ZH')

urlList <- c('https://data.geo.admin.ch/ch.bafu.bundesinventare-flachmoore/objectsheets/2017revision/cantons',
         'https://data.geo.admin.ch/ch.bafu.bundesinventare-hochmoore/objectsheets/2017revision/cantons')

result.all <- read.csv(text='Name,Obj,cx,cy,Alt,Surf')
temp <- tempfile()
fn.f <- file.path(getwd(), 'MaraisFiles')

if (!dir.exists(fn.f)){
  dir.create(fn.f)
} else {
  print("Dir already exists!")
}


for(u in urlList){
  for(k in ktList){
    urlKt <- as.character(paste(u,paste(tolower(k),'zip',sep='.'),sep='/'))
    print(urlKt)
    if(http_error(urlKt)!=TRUE){
      download.file(urlKt,temp)
      unzip (temp, exdir = fn.f)
    }
  }
}
      
fns <- list.files(fn.f,pattern='.pdf$',all.files=T,full.names=T,recursive=T,include.dirs=T)
print(length(fns))

for(f in fns){
  pdf.f <- pdf_text(f)
  txt <- strsplit(pdf.f,split = '\n') #convert all text from pdf file, \n calling a new line

  Name.i <- grep('Localité',txt[[1]])
  Name <- strsplit(txt[[1]][Name.i+1],split='\r')[[1]]
  Name <- trimws(Name) #remove white space
  Name <- as.character(Name) # convert to integer value

  Obj.i <- grep('Objet',txt[[1]])
  Obj <- strsplit(txt[[1]][Obj.i+1],split='\r')[[1]]
  Obj <- strsplit(Obj,split=' ')[[1]]
  Obj <- as.integer(Obj[length(Obj)])
        
  cxcy.i <- grep('Coordonnées',txt[[1]])
  cxcy <- strsplit(txt[[1]][cxcy.i+1],split='\r')[[1]] #get cxcy from file and separate cx and cy
  cxcy <- strsplit(cxcy,split='/') #separate cx and cy
  cx <- cxcy[[1]][1] #get cx
  cx <- trimws(cx) #remove white space
  cx <- gsub("’","",trimws(cx)) #remove 1000 separator
  cx <- as.integer(cx) # convert to integer value
  
  cy <- cxcy[[1]][2] #get cy
  cy <- gsub("’","",cy) #remove 1000 separator
  cy <- as.integer(cy) # convert to integer value
  
  Alt.i <- grep('Altitude',txt[[1]])
  Alt <- strsplit(txt[[1]][Alt.i+1],split='\r')[[1]] #get cxcy from file and separate cx and cy
  Alt <- trimws(Alt) #remove white space
  Alt <- gsub(" m","",trimws(Alt)) #remove altitude unit
  Alt <- as.numeric(Alt) # convert to integer value
  
  Surf.i <- grep('Surface',txt[[1]])
  Surf <- strsplit(txt[[1]][Surf.i+1],split='\r')[[1]] #get cxcy from file and separate cx and cy
  Surf <- trimws(Surf) #remove white space
  Surf <- gsub(" ha","",trimws(Surf)) #remove altitude unit
  Surf <- as.numeric(Surf) # convert to integer value
  
  dtf.temp <- data.frame('Name'=Name,
                         'Obj'=Obj,
                         'cx'=cx,
                         'cy'=cy,
                         'Alt'=Alt,
                         'Surf'=Surf)

  result.all <- rbind(result.all,dtf.temp)
  print(dtf.temp)
}





write.csv(result.all,'ListeSite_Marais_cxcy_fromSource.csv',row.names = F,fileEncoding = 'latin1')

