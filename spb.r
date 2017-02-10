#########################################################
#########################################################
# © eRey.ch | bioGIS; erey@biogis.ch
# created on 2016.11.07
# source('./spb.r')
# https://github.com/biogis/r/blob/master/spb.r

# Set your desktop as your working directory
# 1-. spb dataframe created with the data available on http://www.agrarbericht.ch/fr/services/documentation/download-center
# 2-. plot is made using colorbrewer2.org: http://colorbrewer2.org/#type=diverging&scheme=Spectral&n=10
# 3-. plot is made using a y-log scale


###########################################################
###########################################################



#https://tonybreyal.wordpress.com/2011/11/24/source_https-sourcing-an-r-script-from-github/
source_https <- function(url, ...) {
  # load package
  require(RCurl)
  
  # parse and evaluate each .R script
  sapply(c(url, ...), function(u) {
    eval(parse(text = getURL(u, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))), envir = .GlobalEnv)
  })
}


source_https("https://raw.githubusercontent.com/biogis/CC/master/template.r")


wch <- data.frame('Years'=seq(2000,2015,by=1),
				'PrairExt'=c(13710,17092,17836,18412,19093,19638,20288,20545,21338,21948,22980,24420,25907,27712,30463,31315),
				'PrairMinInt'= c(18592,18140,17290,16587,15803,15155,14489,13896,12995,12287,11466,10848,10297,9799,9579,9627),
				'HauteTige'= c(7568,5272,7558,7548,7495,7428,7349,7250,7121,7026,6985,7061,7088,7197,7512,7177),
				'Haies'=c(1295,1276,1255,1239,1255,1276,1288,1281,1270,1322,1397,1409,1475,1570,1710,1833),
				'JacheresFlor'=c(673,1094,1257,1348,1377,1314,1291,1201,1148,989,999,1053,1111,1128,1194,1369),
				'Litiere'=c(675,696,753,759,764,764,774,765,771,794,800,806,816,839,954,1073),
				'JacheresTourn'=c(481,634,707,683,534,425,366,387,360,275,221,278,293,285,267,311),
				'BandesCulturExt'=c(26,6,20,18,22,35,28,23,25,32,25,39,54,81,118,114),
				'Type16'=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,116,103),
				'Ourlets'=c(NA,NA,NA,NA,NA,NA,NA,NA,9,14,21,26,36,44,59,69))

names(wch)
summary(wch)



#############################
#wCH: http://colorbrewer2.org/#type=diverging&scheme=Spectral&n=10, with rectangles



plot(wch$Year,wch$PrairExt,col='white',type='b',lwd=3,ann=F,axes=F,ylim=c(1,85000),log='y')

rect(min(wch$Years), min(wch$HauteTige,na.rm=T), max(wch$Years), max(wch$PrairExt,na.rm=T), col='grey85' ,border = "grey")

rect(min(wch$Years), min(wch$JacheresTourn,na.rm=T), max(wch$Years), max(wch$Haies,na.rm=T), col='grey85' ,border = "grey")

rect(min(wch$Years), min(wch$BandesCulturExt,na.rm=T), max(wch$Years), max(wch$BandesCulturExt,na.rm=T), col='grey85' ,border = "grey")

points(wch$Year,wch$PrairExt,col='#9e0142',type='b',lwd=3)
points(wch$Year,wch$PrairMinInt,col='#d53e4f',type='b',lwd=3)
points(wch$Year,wch$HauteTige,col='#f46d43',type='b',lwd=3)


points(wch$Year,wch$Haies,col='#fdae61',type='b',lwd=3)
points(wch$Year,wch$JacheresFlor,col='#fee08b',type='b',lwd=3)
points(wch$Year,wch$Litiere,col='#e6f598',type='b',lwd=3)
points(wch$Year,wch$JacheresTourn,col='#abdda4',type='b',lwd=3)

points(wch$Year,wch$BandesCulturExt,col='#66c2a5',type='b',lwd=3)
points(wch$Year,wch$Type16,col='#3288bd',type='b',lwd=3)
points(wch$Year,wch$Ourlets,col='#5e4fa2',type='b',lwd=3)

#abline(h=c(6,118,221,1833,5272,31315), col='grey')

mtext('Surface [Ha], échelle log', side=2, line=3,cex=1)
axis(2,cex.axis=0.7,las=2,at=c(1,6,10,100,118,221,1000,1833,5272,10000,31315,50000),labels=c("1","6","10","100","118","221","1'000","1'833","5'272","10'000","31'315","50'000"))
#axis(2,cex.axis=0.8,las=2)
axis(1,at=seq(2000,2015,1),tick=T,labels=seq(2000,2015,1),las=1,cex.axis=1)

legend('bottomleft', horiz=F, ncol=3,bty='n', cex=0.8, c('Prairies extensives','Prairies peu intensives', 'Arbres fruitiers  haute-tige', 'Haies','Jachères florales','Surfaces à litière','Jachères tournantes', 'Bandes culturales extensives','spécifique à la région','Ourlets sur terres assolées'), lty=1,lwd=2, text.col=c('#9e0142','#d53e4f','#f46d43','#fdae61','#fee08b','#e6f598','#abdda4','#66c2a5','#3288bd','#5e4fa2'),col=c('#9e0142','#d53e4f','#f46d43','#fdae61','#fee08b','#e6f598','#abdda4','#66c2a5','#3288bd','#5e4fa2'))
