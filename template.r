#source('http://www.erey.ch/zipimport/Template.r')
#source('/Volumes/Data/PhD/Script/R/Template.r')
#source('/exchange/manu/Template.r')
#source('D:/erey/CloudStation/Script/R/Template.r')
#source('e:/Script/R/Template.r')


Sys.setenv(TZ='UTC')
rm(list=ls())
proj <- '+proj=somerc +lat_0=46.95240555555556 +lon_0=7.439583333333333 +k_0=1 +x_0=600000 +y_0=200000 +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs '
#proj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
#pdf('rplot.pdf',paper='a4r',width=11,height=8.5)
#dev.off()

## Set library path
try(.libPaths("//eaw-homedirs/reyemman$/My Documents/R/win-library/3.2"),silent=T)
try(.libPaths("C:/Program Files/R/R-3.2.2/library"),silent=T)
try(.libPaths('//giubserv01/hydro/userdata/erey/Documents/R/win-library/3.2'),silent=T)


packages <- c(
  #stat libraries
  'lattice','ade4','permute','reshape','zoo','miscTools','mra','Rcapture',
  #'readxl','vegan',
  
  #Spatial libraries
  'foreign','sp','plotrix','raster','maps','proj4','rgeos','grid',
  'maptools','rgdal','deldir','splancs','spatial','spatialkernel',
  'spatstat','KernSmooth','scales','plyr','Rcpp',
  
  #NCDF, Rain and temperature libraries
  'fts','ncdf',#'gpclib',
  
  #BIOMOD libraries
  'splines','randomForest','survival','gbm','gam','mgcv','MASS','abind',
  'rpart','biomod2',
  
  #Looping libraries
  'foreach','iterators','parallel','doParallel',
  
  #graphics libraries
  'TeachingDemos','hexbin','corrgram','diagram',
  'rasterVis','latticeExtra','beanplot','vioplot','ggplot2','chron',
  'grid','GGally','RColorBrewer','classInt','extrafont','scatterplot3d',
 # 'shiny','rgl','soiltexture',
  
  #R libraries
  'XML','RCurl','devtools','base64enc'#,'installr'
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

gpclibPermit()
print('#Register the number of cores for parallel computation')
registerDoParallel(cores=7)
# print('#Load fonts')
loadfonts(device="pdf")

print('Use font from the OS, list all fonts: fonttable()')
print('#pdf("rplot.pdf",paper="a4r",family="DINbeck",width=11,height=8.5)')
print('p+theme_bw(base_size = 18, base_family = "DINBek")+...')

try(setwd('/Users/erey/Documents/GIS/PyGIS_SSD'),silent=T)
try(setwd('e:/PyGIS'),silent=T)


colPalette <- c('Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges', 'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds', 'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd',
                'BrBG', 'PiYG', 'PRGn', 'PuOr', 'RdBu', 'RdGy', 'RdYlBu', 'RdYlGn', 'Spectral',
                'Accent','Dark2','Paired','Pastel1','Pastel2','Set1','Set2','Set3')
print(colPalette)


###################
#show a notification when a long calculation is done, 
#For Mac Users only: first install via the terminal with:
#sudo gem install terminal-notifier
#and then use:
print('notify function: use as notify()')

notify <- function(message="Operation complete") {
  system(sprintf("/usr/bin/terminal-notifier -title 'RStudio' -message '%s' -sender org.rstudio.RStudio -activate org.rstudio.RStudio",
                 message),
         ignore.stdout=TRUE, ignore.stderr=TRUE, wait=FALSE)
}


###################
#Memory Use function, show the memory with 
print('Memory function: use as showMemoryUse()')

showMemoryUse <- function(sort="size", decreasing=TRUE, limit) {
  
  objectList <- ls(parent.frame())
  
  oneKB <- 1024
  oneMB <- 1048576
  oneGB <- 1073741824
  
  memoryUse <- sapply(objectList, function(x) as.numeric(object.size(eval(parse(text=x)))))
  
  memListing <- sapply(memoryUse, function(size) {
    if (size >= oneGB) return(paste(round(size/oneGB,2), "GB"))
    else if (size >= oneMB) return(paste(round(size/oneMB,2), "MB"))
    else if (size >= oneKB) return(paste(round(size/oneKB,2), "kB"))
    else return(paste(size, "bytes"))
  })
  
  memListing <- data.frame(objectName=names(memListing),memorySize=memListing,row.names=NULL)
  
  if (sort=="alphabetical") memListing <- memListing[order(memListing$objectName,decreasing=decreasing),] 
  else memListing <- memListing[order(memoryUse,decreasing=decreasing),] #will run if sort not specified or "size"
  
  if(!missing(limit)) memListing <- memListing[1:limit,]
  
  print(memListing, row.names=FALSE)
  return(invisible(memListing))
}


#jet.colors <- colorRampPalette(c('black',"#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000",'black'))
#jet.colors <- colorRampPalette(c('black',"#7F0000","red","#FF7F00","yellow","#7FFF7F","cyan","#007FFF","blue","#00007F",'black'))
jet.colors <- colorRampPalette(c("#7F0000","red","#FF7F00","yellow","#7FFF7F","cyan","#007FFF","blue","#00007F"))
jet.colors <- colorRampPalette(c("#7F0000","#ff0000","#FF7F00","#ffff00","#7FFF7F","#00ffff","#007FFF","#0000ff","#00007F","#371450"))

BrBG <- colorRampPalette(c('#543005','#8c510a','#bf812d','#dfc27d','#f6e8c3','#f5f5f5','#c7eae5','#80cdc1','#35978f','#01665e','#003c30'))
cbbPalette <- colorRampPalette (c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))
#plot(dem1,col=jet.colors(255),horizontal=T,legend.mar=3.1)
print('color set defined: use as col=jet.colors(255)')
print('color set defined: use as col=BrBG(255)')
print('color blind set defined: use as col=cbbPalette(255)')




###################
#Calendar Heatmap


##############################################################################
# Calendar Heatmap #
# by #
# Paul Bleicher #
# an R version of a graphic from: #
# http://stat-computing.org/dataexpo/2009/posters/wicklin-allison.pdf #
# requires lattice, chron, grid packages #
##############################################################################
require(lattice); require(chron); require(grid)

## calendarHeat: An R function to display time-series data as a calendar heatmap
## Copyright 2009 Humedica. All rights reserved.

## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.

## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
## GNU General Public License for more details.

## You can find a copy of the GNU General Public License, Version 2 at:
## http://www.gnu.org/licenses/gpl-2.0.html

#' An R function to display time-series data as a calendar heatmap
#'
#' This graphic originally appeared \href{http://stat-computing.org/dataexpo/2009/posters/wicklin-allison.pdf}{here}.
#' This function is included with the \code{makeR} package to support the
#' R-Bloggers demo. See \code{demo('makeR')} for more information.
#'
#' @param dates vector of Dates
#' @param values vector of values
#' @param ncolors number of colors to use
#' @param color the color scheme to use. Currently supports r2b, (red to blue),
#' r2g (red to green), and w2b (white to blue).
#' @param varname varaible names
#' @param date.form the format of the Date column
#' @param ... other non-specified parameters
#' @author Paul Bleicher
#' @export
calendarHeat <- function(dates,
                         values,
                         ncolors=99,
                         color="r2g",
                         varname="Values",
                         date.form = "%Y-%m-%d", ...) {
  require(lattice)
  require(grid)
  require(chron)
  if (class(dates) == "character" | class(dates) == "factor" ) {
    dates <- strptime(dates, date.form)
  }
  caldat <- data.frame(value = values, dates = dates)
  min.date <- as.Date(paste(format(min(dates), "%Y"),
                            "-1-1",sep = ""))
  max.date <- as.Date(paste(format(max(dates), "%Y"),
                            "-12-31", sep = ""))
  dates.f <- data.frame(date.seq = seq(min.date, max.date, by="days"))
  
  # Merge moves data by one day, avoid
  caldat <- data.frame(date.seq = seq(min.date, max.date, by="days"), value = NA)
  dates <- as.Date(dates)
  caldat$value[match(dates, caldat$date.seq)] <- values
  
  caldat$dotw <- as.numeric(format(caldat$date.seq, "%w"))
  caldat$woty <- as.numeric(format(caldat$date.seq, "%U")) + 1
  caldat$yr <- as.factor(format(caldat$date.seq, "%Y"))
  caldat$month <- as.numeric(format(caldat$date.seq, "%m"))
  yrs <- as.character(unique(caldat$yr))
  d.loc <- as.numeric()
  for (m in min(yrs):max(yrs)) {
    d.subset <- which(caldat$yr == m)
    sub.seq <- seq(1,length(d.subset))
    d.loc <- c(d.loc, sub.seq)
  }
  caldat <- cbind(caldat, seq=d.loc)
  
  #color styles
  r2b <- c("#0571B0", "#92C5DE", "#F7F7F7", "#F4A582", "#CA0020") #red to blue
  b2r <- c("#CA0020", "#F4A582", "#F7F7F7", "#92C5DE", "#0571B0") #red to blue
  r2g <- c("#D61818", "#FFAE63", "#FFFFBD", "#B5E384") #red to green
  w2b <- c("#045A8D", "#2B8CBE", "#74A9CF", "#BDC9E1", "#F1EEF6") #white to blue
  b2w <- c("#F1EEF6", "#BDC9E1", "#74A9CF", "#2B8CBE", "#045A8D") #blue to white
  w2b <- c("#045A8D", "#2B8CBE", "#74A9CF", "#BDC9E1", "#F1EEF6") #white to blue
  gry <- c('grey60')
  discret <- c('tomato','steelblue','gold','limegreen')
  #jet.colors <- c('black',"#7F0000","red","#FF7F00","yellow","#7FFF7F","cyan","#007FFF","blue","#00007F",'black')
  jet.colors <- c("#7F0000","red","#FF7F00","yellow","#7FFF7F","cyan","#007FFF","blue","#00007F")
  
  assign("col.sty", get(color))
  calendar.pal <- colorRampPalette((col.sty), space = "Lab")
  def.theme <- lattice.getOption("default.theme")
  cal.theme <-
    function() {
      theme <-
        list(
          strip.background = list(col = "transparent"),
          strip.border = list(col = "transparent"),
          axis.line = list(col="transparent"),
          par.strip.text=list(cex=0.8))
    }
  lattice.options(default.theme = cal.theme)
  yrs <- (unique(caldat$yr))
  nyr <- length(yrs)
  print(cal.plot <- levelplot(value~woty*dotw | yr, data=caldat,
                              as.table=TRUE,
                              aspect=.12,
                              layout = c(1, nyr%%7),
                              between = list(x=0, y=c(1,1)),
                              strip=TRUE,
                              #main = paste("Calendar Heat Map of ", varname, sep = ""),
                              main = paste(" ", varname, sep = ""),
                              scales = list(
                                x = list(
                                  at= c(seq(2.9, 52, by=4.42)),
                                  labels = month.abb,
                                  alternating = c(1, rep(0, (nyr-1))),
                                  tck=0,
                                  cex = 0.7),
                                y=list(
                                  at = c(0, 1, 2, 3, 4, 5, 6),
                                  labels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday",
                                             "Friday", "Saturday"),
                                  alternating = 1,
                                  cex = 0.6,
                                  tck=0)),
                              xlim =c(0.4, 54.6),
                              ylim=c(6.6,-0.6),
                              cuts= ncolors - 1,
                              col.regions = (calendar.pal(ncolors)),
                              xlab="" ,
                              ylab="",
                              colorkey= list(col = calendar.pal(ncolors), width = 0.6, height = 0.5),
                              subscripts=TRUE
  ) )
  panel.locs <- trellis.currentLayout()
  for (row in 1:nrow(panel.locs)) {
    for (column in 1:ncol(panel.locs)) {
      if (panel.locs[row, column] > 0)
      {
        trellis.focus("panel", row = row, column = column,
                      highlight = FALSE)
        xyetc <- trellis.panelArgs()
        subs <- caldat[xyetc$subscripts,]
        dates.fsubs <- caldat[caldat$yr == unique(subs$yr),]
        y.start <- dates.fsubs$dotw[1]
        y.end <- dates.fsubs$dotw[nrow(dates.fsubs)]
        dates.len <- nrow(dates.fsubs)
        adj.start <- dates.fsubs$woty[1]
        
        for (k in 0:6) {
          if (k < y.start) {
            x.start <- adj.start + 0.5
          } else {
            x.start <- adj.start - 0.5
          }
          if (k > y.end) {
            x.finis <- dates.fsubs$woty[nrow(dates.fsubs)] - 0.5
          } else {
            x.finis <- dates.fsubs$woty[nrow(dates.fsubs)] + 0.5
          }
          grid.lines(x = c(x.start, x.finis), y = c(k -0.5, k - 0.5),
                     default.units = "native", gp=gpar(col = "grey", lwd = 1))
        }
        if (adj.start < 2) {
          grid.lines(x = c( 0.5, 0.5), y = c(6.5, y.start-0.5),
                     default.units = "native", gp=gpar(col = "grey", lwd = 1))
          grid.lines(x = c(1.5, 1.5), y = c(6.5, -0.5), default.units = "native",
                     gp=gpar(col = "grey", lwd = 1))
          grid.lines(x = c(x.finis, x.finis),
                     y = c(dates.fsubs$dotw[dates.len] -0.5, -0.5), default.units = "native",
                     gp=gpar(col = "grey", lwd = 1))
          if (dates.fsubs$dotw[dates.len] != 6) {
            grid.lines(x = c(x.finis + 1, x.finis + 1),
                       y = c(dates.fsubs$dotw[dates.len] -0.5, -0.5), default.units = "native",
                       gp=gpar(col = "grey", lwd = 1))
          }
          grid.lines(x = c(x.finis, x.finis),
                     y = c(dates.fsubs$dotw[dates.len] -0.5, -0.5), default.units = "native",
                     gp=gpar(col = "grey", lwd = 1))
        }
        for (n in 1:51) {
          grid.lines(x = c(n + 1.5, n + 1.5),
                     y = c(-0.5, 6.5), default.units = "native", gp=gpar(col = "grey", lwd = 1))
        }
        x.start <- adj.start - 0.5
        
        if (y.start > 0) {
          grid.lines(x = c(x.start, x.start + 1),
                     y = c(y.start - 0.5, y.start - 0.5), default.units = "native",
                     gp=gpar(col = "black", lwd = 1.75))
          grid.lines(x = c(x.start + 1, x.start + 1),
                     y = c(y.start - 0.5 , -0.5), default.units = "native",
                     gp=gpar(col = "black", lwd = 1.75))
          grid.lines(x = c(x.start, x.start),
                     y = c(y.start - 0.5, 6.5), default.units = "native",
                     gp=gpar(col = "black", lwd = 1.75))
          if (y.end < 6 ) {
            grid.lines(x = c(x.start + 1, x.finis + 1),
                       y = c(-0.5, -0.5), default.units = "native",
                       gp=gpar(col = "black", lwd = 1.75))
            grid.lines(x = c(x.start, x.finis),
                       y = c(6.5, 6.5), default.units = "native",
                       gp=gpar(col = "black", lwd = 1.75))
          } else {
            grid.lines(x = c(x.start + 1, x.finis),
                       y = c(-0.5, -0.5), default.units = "native",
                       gp=gpar(col = "black", lwd = 1.75))
            grid.lines(x = c(x.start, x.finis),
                       y = c(6.5, 6.5), default.units = "native",
                       gp=gpar(col = "black", lwd = 1.75))
          }
        } else {
          grid.lines(x = c(x.start, x.start),
                     y = c( - 0.5, 6.5), default.units = "native",
                     gp=gpar(col = "black", lwd = 1.75))
        }
        
        if (y.start == 0 ) {
          if (y.end < 6 ) {
            grid.lines(x = c(x.start, x.finis + 1),
                       y = c(-0.5, -0.5), default.units = "native",
                       gp=gpar(col = "black", lwd = 1.75))
            grid.lines(x = c(x.start, x.finis),
                       y = c(6.5, 6.5), default.units = "native",
                       gp=gpar(col = "black", lwd = 1.75))
          } else {
            grid.lines(x = c(x.start + 1, x.finis),
                       y = c(-0.5, -0.5), default.units = "native",
                       gp=gpar(col = "black", lwd = 1.75))
            grid.lines(x = c(x.start, x.finis),
                       y = c(6.5, 6.5), default.units = "native",
                       gp=gpar(col = "black", lwd = 1.75))
          }
        }
        for (j in 1:12) {
          last.month <- max(dates.fsubs$seq[dates.fsubs$month == j])
          x.last.m <- dates.fsubs$woty[last.month] + 0.5
          y.last.m <- dates.fsubs$dotw[last.month] + 0.5
          grid.lines(x = c(x.last.m, x.last.m), y = c(-0.5, y.last.m),
                     default.units = "native", gp=gpar(col = "black", lwd = 1.75))
          if ((y.last.m) < 6) {
            grid.lines(x = c(x.last.m, x.last.m - 1), y = c(y.last.m, y.last.m),
                       default.units = "native", gp=gpar(col = "black", lwd = 1.75))
            grid.lines(x = c(x.last.m - 1, x.last.m - 1), y = c(y.last.m, 6.5),
                       default.units = "native", gp=gpar(col = "black", lwd = 1.75))
          } else {
            grid.lines(x = c(x.last.m, x.last.m), y = c(- 0.5, 6.5),
                       default.units = "native", gp=gpar(col = "black", lwd = 1.75))
          }
        }
      }
    }
    trellis.unfocus()
  }
  lattice.options(default.theme = def.theme)
}

## Example of use: Plot financial data
## This code is not run.
if(FALSE) {
  
  #create faux data; skip this to use data from a file or stock data
  #ndays <- 1500 #set number of days
  #dates <- as.POSIXlt(seq(Sys.Date()- ndays, Sys.Date() - 1, by="days"))
  #vals <- runif(ndays, -100, 100)
  
  #stock data:
#   stock <- "MSFT"
#   start.date <- "2006-01-12"
#   end.date <- Sys.Date()
#   quote <- paste("http://ichart.finance.yahoo.com/table.csv?s=",
#                  stock,
#                  "&a=", substr(start.date,6,7),
#                  "&b=", substr(start.date, 9, 10),
#                  "&c=", substr(start.date, 1,4),
#                  "&d=", substr(end.date,6,7),
#                  "&e=", substr(end.date, 9, 10),
#                  "&f=", substr(end.date, 1,4),
#                  "&g=d&ignore=.csv", sep="")
#   stock.data <- read.csv(quote, as.is=TRUE)
#   
#   # Plot as calendar heatmap
#   calendarHeat(stock.data$Date, stock.data$Adj.Close, varname="MSFT Adjusted Close")
}

print("calendarHeat map function used as calendarHeat(date,data,varname='Title',color='b2r')")
print("color styles: r2b #red to blue; b2r #blue to red; r2g #red to green; w2b #white to blue; b2w #blue to white; gry # grey60 jet.colors")



###################
#Alluvial graph



#' Alluvial diagram
#'
#' Drawing alluvial diagrams also known as parallel set plots.
#'
#' Still under development!
#'
#' @param ... vectors or data frames, all for the same number of observations
#' @param freq numeric, vector of frequencies of the same length as the number of observations
#' @param col vector of colors of the stripes
#' @param border vector of border colors for the stripes
#' @param layer numeric, order of drawing of the stripes
#' @param hide logical, should particular stripe be plotted
#' @param alpha numeric, vector of transparency of the stripes
#' @param gap.width numeric, relative width of inter-category gaps
#' @param xw numeric, the distance from the set axis to the control points of the xspline
#' @param cw numeric, width of the category axis
#'
#' @return Nothing
#'
#' @export
#'
#' @example examples/alluvial.R

alluvial <- function( ..., freq, col="gray", border=0, layer, hide=FALSE, alpha=0.5,
                     gap.width=0.05, xw=0.1, cw=0.1 )
{
  # Data and graphical parameters
  p <- data.frame( ..., freq=freq, col, alpha, border, hide, stringsAsFactors=FALSE)
  n <- nrow(p)
  # Layers determine plotting order
  if(missing(layer))
  {
    layer <- 1:n
  }
  p$layer <- layer
  np <- ncol(p) - 6 # Number of dimensions
  d <- p[ , 1:np, drop=FALSE] # Dimensions dframe
  p <- p[ , -c(1:np), drop=FALSE] # Parameteres dframe
  p$freq <- with(p, freq/sum(freq)) # Frequencies (weights)
  # Converting colors to hexcodes
  col <- col2rgb(p$col, alpha=TRUE)
  if(!identical(alpha, FALSE)) {
    col["alpha", ] <- p$alpha*256
  }
  p$col <- apply(col, 2, function(x) do.call(rgb, c(as.list(x), maxColorValue = 256)))
  # Compute endpoints of flows (polygons)
  # i = dimension id
  # d = data frame of dimensions
  # f = weights
  # w = gap between categories
  getp <- function(i, d, f, w=gap.width) {
    # Ordering dimension ids for lexicographic sorting
    a <- c(i, (1:ncol(d))[-i])
    # Order of rows of d starting from i-th dimension
    o <- do.call(order, d[a])
    # Breakpoints on a dimension
    x <- c(0, cumsum(f[o])) * (1-w)
    # Stripe coordinates on a dimension
    x <- cbind(x[-length(x)], x[-1])
    # By how much stripes need to be shifted upwards (gap/max(gap))
    gap <- cumsum( c(0L, diff(as.numeric(d[o,i])) != 0) )
    # shifts
    gap <- gap / max(gap) * w
    # add gap-related shifts to stripe coordinates on dimension i
    (x + gap)[order(o),]
  }
  # Calculate stripe locations on dimensions: list of data frames. A component
  # for a dimension. Data frame contains 'y' locations of stripes.
  dd <- lapply(seq_along(d), getp, d=d, f=p$freq)
  # Plotting
  op <- par(mar=c(2, 1, 1, 1))
  plot(NULL, type="n", xlim=c(1-cw, np+cw), ylim=c(0, 1), xaxt="n", yaxt="n",
       xaxs="i", yaxs="i", xlab='', ylab='', frame=FALSE)
  # For every stripe
  ind <- rev(order(p$layer)[!hide])
  for(i in ind )
  {
    # For every inter-dimensional segment
    for(j in 1:(np-1) )
    {
      # Draw stripe
      xspline( c(j, j, j+xw, j+1-xw, j+1, j+1, j+1-xw, j+xw, j) + rep(c(cw, -cw, cw), c(3, 4, 2)),
               c( dd[[j]][i, c(1, 2, 2)], rev(dd[[j+1]][i, c(1, 1, 2, 2)]), dd[[j]][i,c(1, 1)]),
               shape = c(0,0,1,1,0,0,1,1,0, 0),
               open=FALSE,
               col=p$col[i], border=p$border[i])
    }
  }
  # Category blocks with labels
  for(j in seq_along(dd))
  {
    ax <- lapply(split(dd[[j]], d[,j]), range)
    for(k in seq_along(ax))
    {
      rect( j-cw, ax[[k]][1], j+cw, ax[[k]][2] )
      text( j, mean(ax[[k]]), labels=names(ax)[k])
    }
  }
  # X axis
  axis(1, at= rep(c(-cw, cw), ncol(d)) + rep(seq_along(d), each=2),
       line=0.5, col="white", col.ticks="black", labels=FALSE)
  axis(1, at=seq_along(d), tick=FALSE, labels=names(d))
  par(op)
}

print("alluvial graph, use frequency of occurences, use as alluvial(data,freq=freq,border=NA,col=ifelse(tit$survived=='No','red','grey''))")



# Carson's Voronoi polygons function
voronoipolygons <- function(x) {
  require(deldir)
  if (.hasSlot(x, 'coords')) {
    crds <- x@coords  
  } else crds <- x
  z <- deldir(crds[,1], crds[,2])
  w <- tile.list(z)
  polys <- vector(mode='list', length=length(w))
  require(sp)
  for (i in seq(along=polys)) {
    pcrds <- cbind(w[[i]]$x, w[[i]]$y)
    pcrds <- rbind(pcrds, pcrds[1,])
    polys[[i]] <- Polygons(list(Polygon(pcrds)), ID=as.character(i))
  }
  SP <- SpatialPolygons(polys)
  voronoi <- SpatialPolygonsDataFrame(SP, data=data.frame(x=crds[,1],
                                                          y=crds[,2], row.names=sapply(slot(SP, 'polygons'), 
                                                                                       function(x) slot(x, 'ID'))))
}


print('Voronoi Polygon on SpatiolPoint or SpatialPointsDataFrame, using voronoipolygons(spShp)')
print('Link data to the resulted spatial polygons using SpatialPolygonsDataFrame(v,data=ptDataframe)')



## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

#https://tonybreyal.wordpress.com/2011/11/24/source_https-sourcing-an-r-script-from-github/
source_https <- function(url, ...) {
  # load package
  require(RCurl)
  
  # parse and evaluate each .R script
  sapply(c(url, ...), function(u) {
    eval(parse(text = getURL(u, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))), envir = .GlobalEnv)
  })
}

print('data frame summary, use as: cdata <- summarySE(dtf, measurevar="Value", groupvars=c("Dataset", "Model","EvaluationMethod"), na.rm=TRUE)')
print('moving average using zoo library, use as: rollmean(x,k) or rollmedian(x,k), x as vecor value, k as moving window')
print('median by row, use as rowMedians(x), colMedians, rowMeans(x), colMeans()')
print('scatterplot with correlation values: e.g. ggpairs(iris[,1:4])')
print('use as source_https("https://raw.githubusercontent.com/biogis/CC/master/template.r")')

# ########################################################################
# 
# print("pdf('rplot.pdf',paper='a4r',width=11,height=8.5)")
# print('notify function: use as notify()')
# print('Memory function: use as showMemoryUse()')
# print('color set defined: use as col=jet.colors(255)')
# print("calendarHeat map function used as calendarHeat(date,data,varname='Title',color='b2r')")
# print("color styles: r2b #red to blue; b2r #blue to red; r2g #red to green; w2b #white to blue; b2w #blue to white; gry # grey60 jet.colors")
# print("alluvial graph, use frequency of occurences, use as alluvial(data,freq=freq,border=NA,col=ifelse(tit$survived=='No','red','grey''))")
# print('moving average using zoo library, use as: rollmean(x,k) or rollmedian(x,k), x as vecor value, k as moving window')
# 
