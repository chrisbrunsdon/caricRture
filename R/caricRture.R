##
## Lots of functions are 'private' tools used by the 'top level' items
## Exported functions have Roxygen protocols before them...
##  
## Conventions - 
## Code examples run from left to right using dplyr
## Public functions all lower case,  with underline not dot as a word separator.
## Be good and document the package!
##


## Docs for non-functions (eg the package,  data sets)


#' A tool set for creating 'stylised' maps and other graphics.   
#' 
#' The key tools here are functions to create 'sketchy' polygons, lines and points,  and
#' 'curvey' polygons and lines.  Used in combination they can create grap]hics having a 'hand drawn'
#' appearance in a number of styles - such as pencil-drawn or marker pen. The 'sketchy' 
#' renderings are based on the algorithms of \href{https://web.archive.org/web/20140913142528/http://openaccess.city.ac.uk/1274/}{Wood et. al.}.
#' These are useful in conveying ther fact that some data-driven graphics are based 
#' on vague or fuzzy information.
#' 
#' @docType package
#' 
#' @name caricRture
#'
NULL

#' Baltic Soil Survey 
#' 
#' A pair of spatial data objects relating to the 2000 Baltic Soil Survey.
#' @format The data contains two objects:
#' 
#' \describe{
#'  \item{baltic_chop}{A \link[sp]{SpatialPolygons} object of states surrounding the Baltic. Russia is truncated.}
#'  \item{topsoil}{A \link[sp]{SpatialPointsDataFrame} containing survey results for topsoil.}
#' }
#' 
#' The \code{topsoil} data frame contains the following variables;
#' 
#' \describe{
#'  \item{SIO2_T}{Silica (Silicon dioxide) concentration.}
#'  \item{TiO2_T}{Titanium dioxide concentration.}
#'  \item{Al2O3_T}{Aluminium oxide concentration.}
#'  \item{Fe2O3_T}{Iron (III) oxide concentration.}
#'  \item{MnO_T}{Manganese (II) oxide concentration.}
#'  \item{MgO_T}{Magnesium oxide concentration.}
#'  \item{CaO_T}{Calcium oxide concentration.}
#'  \item{Na2O_T}{Sodium oxide concentration.}
#'  \item{K2O_T}{Potassium oxide concentration.}
#'  \item{P2O5_T}{Phosphorus pentoxide concentration.}
#' }
#' 
#' @details Concentrations are percentage by weight, standardised to \eqn{z}-scores. All map projections are UTM zone 35N.
#' @source BSS Project in Northern Europe
#' @references Reimann C, Siewers U, Tarvainen T, Bityukova L, Eriksson J, Gilucis A, Gregorauskiene V, Lukashev VK, Matinian NN, Pasieczna A. Agricultural Soils in Northern Europe: A Geochemical Atlas. Geologisches Jahrbuch, Sonderhefte, Reihe D, Heft SD 5, Schweizerbartsche Verlagsbuchhandlung, Stuttgart, 2003.
#' @name bss
NULL

#' Generalised NUTS3 regions for Ireland
#' 
#' A \link[sp]{SpatialPolygonsDataFrame} of NUTS3 regions in Ireland,  with some associated variables
#' 
#' Data frame contains the following variables;
#' \describe{
#'  \item{NUTS1}{NUTS1 Region code}
#'  \item{NUTS1NAME}{NUTS1 Region name}
#'  \item{NUTS2}{NUTS2 Region code}
#'  \item{NUTS2NAME}{NUTS2 Region name}
#'  \item{NUTS3}{NUTS3 Region code}
#'  \item{NUTS3NAME}{NUTS3 Region name}
#'  \item{GEOGID}{Irish region code}
#'  \item{MALE2011}{Male population (2011 Irish Census)}
#'  \item{FEMALE2011}{Female population (2011 Irish Census)}
#'  \item{TOTAL2011}{Total population (2011 Irish Census)}
#' }
#' @name RA
NULL

#' Generalised NUTS3 regions for the island of Ireland (including Northern Ireland)
#' 
#' A \link[sp]{SpatialPolygonsDataFrame} of NUTS3 regions in the island of Ireland,  with some associated variables
#' 
#' Data frame contains the following variables;
#' \describe{
#'  \item{NUTS_ID}{NUTS3 Region code}
#'  \item{STAT_LEVL_}{Statistical level}
#'  \item{SHAPE_AREA}{Region area}
#'  \item{NUTS2NAME}{Region perimeter}
#' }
#' @name all_ireland
NULL


#' House price data for Greater London
#' 
#' A \link[sp]{SpatialPolygonsDataFrame} of London Bouroughs (\code{londonborough}) and a 
#' \link[sp]{SpatialPointsDataFrame} of London house price data (\code{londonhp}).  The house price data is for 2001
#' and was supplied by the Nationwide Building Society.  There are 316 house price sales and 20
#' variables.   
#' 
#' The variables are:
#' \describe{
#'  \item{PURCHASE}{Purchase price (UKP)}
#'  \item{FLOORSZ}{Floor size (square metres)}
#'  \item{TYPEDETCH,TPSEMIDTCH,TYPETRRD,TYPEBNGLW,TYPEFLAT}{House type dummy: Detached, Semi, Terraced,Bungalow,Flat}
#'  \item{BLDPWW1,BLDINTW,BLDPOSTW,BLD60S,BLD70S,BLD80S,BLD90S}{Dummy for period of building:Pre WW1, Inter-War, Post WW2, or recent decade}
#'  \item{BATH2}{Two or more bathrooms - indicator}
#'  \item{BEDS2}{Two bedrooms - indicator}
#'  \item{GARAGE1}{Garage present - indicator}
#'  \item{UNEMPLOY}{Unemployment rate in census ward of property}
#'  \item{PROF}{Rate of employees in professional or managerial employment in census ward of property}
#' }
#' @name londonhp
NULL




get_item <- `[[`

get_area <- function(z) z@area

get_hole <- function(z) z@hole

get.IDs <- function(x) sapply(x@polygons, function(y) y@ID)

#' Clone data from a \link[sp]{SpatialPolygonsDataFrame}
#' 
#' Populate a transformed \link[sp]{SpatialPolygons} object with data from a 
#' \link[sp]{SpatialPolygonsDataFrame} with an identical set of IDs
#'
#' @usage clone_data(sp,spdf)
#' sp \%>\% clone_data(spdf)
#'
#' @param sp a \link[sp]{SpatialPolygons} object
#' @param spdf a \link[sp]{SpatialPolygonsDataFrame} object with matching IDs
#'
#' @return a \link[sp]{SpatialPolygonsDataFrame}  object with polygons from \code{sp} and data from \code{spdf}.
#' @export
#' @examples
#' data(RA)
#' RA.spdf %>% small_chop -> RA.sp
#' class(RA.sp)
#' RA.sp %>% clone_data(RA.spdf) -> RA.sp2
#' RA.sp2 %>% data.frame %>% head
clone_data <- function(sp,spdf) SpatialPolygonsDataFrame(sp,data.frame(spdf))

#' Fuse data from to \link[sp]{Spatial} object
#' 
#' Populate a transformed \link[sp]{SpatialPolygons} object with data from a 
#' matrix or array with a set of names or rownames matching the \link[sp]{SpatialPolygons} object IDs
#'
#' @usage fuse_data(sp,v)
#' sp \%>\% fuse_data(v)
#'
#' @param sp a \link[sp]{SpatialPolygons} object
#' @param v a variables whose names or rownames match thev IDs for \code{sp}
#'
#' @return a \link[sp]{SpatialPolygonsDataFrame}  object with polygons from \code{sp} and data from \code{v}.
#' @export
#' @examples
#' data(RA)
#' RA.spdf %>% small_chop -> RA.sp
#' class(RA.sp)
#' RA.sp %>% clone_data(RA.spdf) -> RA.sp2
#' RA.sp2 %>% data.frame %>% head
fuse_data <- function(sp,v) {
  if (is.data.frame(v))  return(SpatialPolygonsDataFrame(sp,v))
  if (is.matrix(v)) return(SpatialPolygonsDataFrame(sp,data.frame(v)))
  v2 <- data.frame(v)
  colnames(v2) <- deparse(substitute(v))
  return(SpatialPolygonsDataFrame(sp,v2))
}





.Hstroke <- function(x1,y1,x2,y2,rough,lmin=0,anchor=FALSE,...) {
  
  r <- .rough2rad(rough)
  dmin <- .rough2rad(lmin)
  flitter <- runif(1,0.7,0.8)
  dx <- x2 - x1
  dy <- y2 - y1
  l <- sqrt(dx^2+dy^2)
  X <- x1 + dx*c(0.0,0.5,flitter,1.0)
  Y <- y1 + dy*c(0.0,0.5,flitter,1.0)
  dl <- c(0,cumsum(sqrt(diff(X)^2+diff(Y)^2)))
  angles <- runif(4,0,2*pi)
  
  radius <- rep(r,4)
  radius[2] <- l / 200
  radius <- pmin(radius,l/10)
  angles[3] <- atan2(dy,dx) + pi/2*sample(c(-1,1),1)
  if (anchor) radius[1] <- 0
  if (l < dmin) radius[2:3] <- 0
  
  radius <- radius*sqrt(runif(4))
  X <- X + cos(angles)*radius
  Y <- Y + sin(angles)*radius
  xh <- spline(dl,X,method='natural',n=40)$y
  yh <- spline(dl,Y,method='natural',n=40)$y
  lines(xh,yh,...)
  return(invisible(list(x=xh[length(xh)],y=yh[length(yh)])))
}


.rough2rad <- function(rough) {
  xin <- par('pin')[1]
  xus <- par('usr')[1:2]
  xus <- xus[2] - xus[1]
  return(rough*xus/xin)
} 


Hstroke <- function(x,y,rough=0.05,overdraw=TRUE,lmin=0,...) {
  .Hstroke(x[1],y[1],x[2],y[2],rough=rough,lmin=lmin,...)
  if (overdraw) .Hstroke(x[1],y[1],x[2],y[2],rough=rough,lmin=lmin,...)
}

Hpath <- function(x,y,rough=0.05,lmin=0,overdraw=TRUE,...) {
  anchor <- .Hstroke(x[1],y[1],x[2],y[2],rough=rough,lmin=lmin,...)
  for (i in 3:length(x)) anchor <- .Hstroke(anchor$x,anchor$y,x[i],y[i],rough=rough,lmin=lmin,anchor=TRUE,...)
  if (!overdraw) return()
  anchor <- .Hstroke(x[1],y[1],x[2],y[2],rough=rough,lmin=lmin,...)
  for (i in 3:length(x)) anchor <- .Hstroke(anchor$x,anchor$y,x[i],y[i],rough=rough,lmin=lmin,anchor=TRUE,...)  
}

Hpolygon <-  function(x,y,rough=0.05,overdraw=TRUE,col=rgb(0,0,0,0.4),border='black',...) {
  polygon(x,y,col=col,border=NA)
  Hpath(x,y,rough=rough,overdraw=overdraw,col=border,...)
}

.Hcurve <- function(x,y,rough=0.05,simplify=-1,...) {
  dl <- cumsum(sqrt(diff(x)^2+diff(y)^2))
  sl <- .rough2rad(simplify)
  keep <- c(TRUE,dl>sl)
  x <- x[keep] 
  y <- y[keep]
  dl <- c(0,dl)[keep]
  l <- length(x)
  r <- .rough2rad(rough)
  angles <- runif(l,0,2*pi)
  radius <- rep(r,l)
  radius <- radius*sqrt(runif(l))
  X <- x + cos(angles)*radius
  Y <- y + sin(angles)*radius
  xh <- spline(dl,X,method='natural',n=40*l)$y
  yh <- spline(dl,Y,method='natural',n=40*l)$y
  lines(xh,yh,...)
}

Hcurve <- function(x,y,rough=0.05,overdraw=TRUE,simplify=-1,...) {
  .Hcurve(x,y,rough=rough,simplify=simplify,...)
  if (overdraw) .Hcurve(x,y,rough=rough,simplify=simplify,...)
}

.Hcircle <- function(x,y,r,rough=0.05,overdraw=TRUE,flourish=0,...) {
  angs <- rev(2*c(0:7,1,2)/8 + 2*flourish)
  X <- x + r*sinpi(angs)
  Y <- y + r*cospi(angs)
  l <- length(x)
  r2 <- .rough2rad(rough)
  angles <- runif(l,0,2*pi)
  radius <- rep(r2,l)
  radius <- radius*sqrt(runif(l))
  X <- X + cos(angles)*radius
  Y <- Y + sin(angles)*radius
  X[2] <- X[9]
  Y[2] <- Y[9]
  dl <- c(0,cumsum(sqrt(diff(X)^2+diff(Y)^2)))
  xh <- spline(dl,X,method='natural',n=300)$y
  yh <- spline(dl,Y,method='natural',n=300)$y
  lines(xh,yh,...)
}

Hcircle <- function(x,y,r,rough=0.05,overdraw=TRUE,flourish=0,...) {
  .Hcircle(x,y,r,rough=rough,flourish=flourish,...)
  if (overdraw) .Hcircle(x,y,r,rough=rough,flourish=flourish,...)
}

Hregpoly <- function(x,y,r,n=4,orient=0,rough=0.05,overdraw=TRUE,border='black',
                     col=rgb(0.4,0.4,0.4),pitch=0.1,
                     style=c('scribble','solid','hatch','outline'),...) {
  rpx <- cospi(2*c(0:(n-1),0)/n + 2*orient)
  rpy <- sinpi(2*c(0:(n-1),0)/n + 2*orient)
  style <- match.arg(style)
  switch(style,
         scribble=Hscribble(x+r*rpx,y+r*rpy,col=col,border=border,rough=rough,pitch=pitch,overdraw=overdraw,...),
         solid=Hpolygon(x+r*rpx,y+r*rpy,col=col,border=border,rough=rough,overdraw=overdraw,...),
         hatch=Hhatch(x+r*rpx,y+r*rpy,col=col,border=border,rough=rough,pitch=pitch,overdraw=overdraw,...),
         outline=if(!is.na(border)) Hpath(x+r*rpx,y+r*rpy,rough=rough,overdraw=overdraw,col=border,...))
}

Hdemo <- function() {
  oldmar <- par('mar')
  par(mar=c(0,0,0,0))
  plot(c(-1,1),c(-1,1),type='n',asp=1,axes=FALSE,xlab='',ylab='')
  sqx <- c(-1,-1,1,1,-1)
  sqy <- c(-1,1,1,-1,-1)
  Hpath(sqx*0.9,sqy*0.9)
  Hregpoly(-0.5,0.5,0.3,3,orient=0.25,col=rgb(0,0.5,0.7,0.4),border='blue',style='solid')
  swx <- seq(-1,1,l=20)
  swy <- sinpi(swx)
  Hcurve(swx*0.8,swy*0.8,col='red',lwd=2,simplify=0.5)
  Hcircle(0.5,-0.5,0.35,lwd=5,col='slategrey',overdraw=FALSE)
  Hregpoly(0.5,-0.5,0.25,6,orient=0.5,lwd=3,col='darkgreen')
  Hregpoly(-0.4,-0.5,0.2,3,lwd=3,col='darkred',rough=0.01,pitch=0.05,border=NA,orient=0.75)
  Hregpoly(0.45,0.3,0.2,7,orient=0.25,col='chocolate',border='darkorange',style='hatch',rough=0.025)
  Htext(-0.03,0.03,"Sine Wave",srt=70,col='olivedrab')
  Htext(0,-0.95,"(c) Chris Brunsdon 2015")
  par(mar=oldmar)
}

.lineclip <- function(x1,y1,x2,y2,pol,tol=x1/100000) {
  if (abs(x1 - x2) < tol && abs(y1 - y2) < tol) return()
  L <- Line(cbind(c(x1,x2),c(y1,y2)))
  Ls <- Lines(list(L),ID="hatch")
  SL <- SpatialLines(list(Ls))
  inter <- gIntersection(pol,SL)
  if (! is(inter,"SpatialLines")) return()
  get.xy <- function(x) x@coords
  return(lapply(inter@lines[[1]]@Lines,get.xy))
}

Hhatch <- function(x,y,rough=0.05,overdraw=TRUE,col=rgb(0.4,0.4,0.4),border='black',pitch=0.1,...) {
  polyg <- as(cbind(x,y),'gpc.poly')
  polyg <- as(polyg,'SpatialPolygons')
  bb <- polyg@bbox
  lhs <- bb[1,1]
  rhs <- bb[1,2]
  btm <- bb[2,1]
  top <- bb[2,2]
  if (rhs - lhs < top - btm) {
    rhs <- lhs + top - btm
  } else {
    top <- btm + rhs - lhs
  }
  seqx <- seq(lhs,rhs,by=0.025)
  rhs <- max(seqx)
  seqy <- rev(seq(btm,top,by=0.025))
  top <- max(seqy)
  for (i in 1:length(seqx))  {
    hatch <- .lineclip(lhs,seqy[i],seqx[i],top,polyg)
    if (!is.null(hatch)) for (item in hatch) lines(item,col=col)
  }
  for (i in 2:length(seqx))  {
    hatch <- .lineclip(seqx[i],btm,rhs,seqy[i],polyg)
    if (!is.null(hatch)) for (item in hatch) lines(item,col=col)
  }
  if(!is.na(border)) Hpath(x,y,rough=rough,overdraw=overdraw,col=border,...)
} 


Hscribble <- function(x,y,rough=0.05,lmin=0,overdraw=TRUE,border='black',col=NA,pitch=0.1,...) {
  polyg <- as(cbind(x,y),'gpc.poly')
  polyg <- as(polyg,'SpatialPolygons')
  bb <- polyg@bbox
  lhs <- bb[1,1]
  rhs <- bb[1,2]
  btm <- bb[2,1]
  top <- bb[2,2]
  if (rhs - lhs < top - btm) {
    rhs <- lhs + top - btm
  } else {
    top <- btm + rhs - lhs
  }
  p <- .rough2rad(pitch)
  seqx <- seq(lhs,rhs,by=p)
  rhs <- max(seqx)
  seqy <- rev(seq(btm,top,by=p))
  top <- max(seqy)
  for (i in 1:length(seqx))  {
    hatch <- .lineclip(lhs,seqy[i],seqx[i],top,polyg)
    if (!is.null(hatch)) for (item in hatch) Hstroke(item[,1],item[,2],rough=rough,col=col,lmin=lmin,overdraw=FALSE,...)
  }
  for (i in 1:length(seqx))  {
    hatch <- .lineclip(seqx[i],btm,rhs,seqy[i],polyg)
    if (!is.null(hatch)) for (item in hatch) Hstroke(item[,1],item[,2],rough=rough,col=col,lmin=lmin,overdraw=FALSE,...)
  }
  if (!is.na(border)) Hpath(x,y,rough=rough,lmin=lmin,overdraw=overdraw,col=border,...)
} 


#' Title with 'handwritten' font
#' 
#' Works like the \link[graphics]{title} command,  but chooses a hand writing styled font.
#'
#' @usage hand_title(txt,fontfam='am',...)
#' txt \%>\% hand_title(...)
#'
#' @param txt a character string to write as the title
#' @param fontfam Handwritten font family - see \link{get_fonts}
#' @param ... other parameters passed on to the \link[graphics]{title} function
#'
#' @return No value returned
#' @export
#' @examples
#' # Here make_canvas is inserted in a pipeline after a simplified and tidied 
#' # map of Irish NUTS3 regions is created.  A sketchy rendition is then added to the
#' # canvas.
#' get_fonts()
#' data(RA) 
#' RA.spdf %>% small_chop %>% gSimplify(tol=11000) %>% tidy_it %>% make_canvas %>% sketch_it(col='orange')
#' 'NUTS3 Regions in Ireland' %>% hand_title(fontfam='gr')
hand_title <- function(lbl,fontfam='am',...) {
  showtext.begin()
  fam <- par('family')
  par(family=fontfam)
  title(lbl,...)
  par(family=fam)
  showtext.end()
}

Htitle <- function(...)
  if (names(dev.cur()) == 'pdf') {
    title(...)
  } else {
    title(...,family='Bradley Hand Bold')
  }



#' Text with 'handwritten' font
#' 
#' Works like the \link[graphics]{text} command,  but chooses a hand writing styled font.
#' Also,  parameter order is changed so that the text label comes first - useful for pipelining with \code{\%>\%}.
#'
#' @usage hand_text(txt,x,y,fontfam='am',,...)
#' txt \%>\% hand_text(x,y,fontfam='am',...)
#'
#' @param txt a character string to write as the title
#' @param x x coordinate of text location
#' @param y y coordinate of text location - if missing, loks in \code{x} for both coordinates
#' @param fontfam Handwritten font family - see \link{get_fonts}
#' @param ... other parameters passed on to the \link[graphics]{text} function
#'
#' @return No value returned
#' @export
#' @examples
#' # Here make_canvas is inserted in a pipeline after a simplified and tidied 
#' # map of Irish NUTS3 regions is created.  A sketchy rendition is then added to the
#' # canvas.
#' get_fonts()
#' data(RA) 
#' RA.spdf %>% small_chop %>% gSimplify(tol=11000) %>% tidy_it %>% make_canvas %>% sketch_it(col='lightgrey',lwd=0.5)
#' RA.spdf %>% coordinates -> label_points
#' RA.spdf$NUTS3NAME %>% hand_text(label_points,col='darkred',fontfam='pm',cex=0.7)
hand_text <- function(lbl,x,y=NULL,fontfam='am',...) {
  showtext.begin()
  fam <- par('family')
  par(family=fontfam)
  text(x,y,label=lbl,...)
  par(family=fam)
  showtext.end()
}


Htext <- function(lbl,x,y=NULL,...)
  if (names(dev.cur()) == 'pdf') {
    text(x,y,label=lbl,...)
  } else {
    text(x,y,label=lbl,...,family='Bradley Hand Bold')
  }


Hpdf <- function(...) pdf(...,family='Amatic')

#' Sketchy compass with 'handwritten' font
#' 
#' Adds a compass (very crude) to a hand drawn map.
#' @usage hand_compass <- function (x, y, r, rot = 0, cex = 1, north_name =  "N", fontfam='am', overdraw=FALSE, ...) 
#' x \%>\% hand_compass(...)
#'
#' @param x x-coordinate for compass centre
#' @param y y-coordinate for compass centre
#' @param r compass 'radius' in map units
#' @param rot compass rotation 
#' @param cex compass scale 
#' @param north_name Name for north to write on the compass
#' @param overdraw whether to use overdraw when drawing compass lines
#' @param fontfam Handwritten font family - see \link{get_fonts}
#' @param ... other parameters passed on to the \link{sketch_it} function
#'
#' @return No value returned
#' @export
#' @examples
#' # Here make_canvas is inserted in a pipeline after a simplified and tidied 
#' # map of Irish NUTS3 regions is created.  A sketchy rendition is then added to the
#' # canvas.
#' get_fonts()
#' data(RA) 
#' RA.spdf %>% small_chop %>% gSimplify(tol=11000) %>% tidy_it %>% clone_data(RA.spdf) -> RA.spdf2
#' shadecol <- ifelse(RA.spdf2$NUTS2 == 'IE01','indianred','dodgerblue')
#' 
#' RA.spdf2 %>% make_canvas %>% sketch_it(col=shadecol)
#' # Add a north arrow with 'North' written in Irish
#' hand_compass(353685,424980,30000,fontfam = 'pm',north_name="ó thuaidh")
#' 
hand_compass <- function (x, y, r, rot = 0, cex = 1, north_name =  "N", fontfam='am', overdraw=FALSE, rough=0.01, ...) 
{
  oldcex <- par(cex = cex)
  mheight <- strheight("M")
  xylim <- par("usr")
  plotdim <- par("pin")
  xmult <- (xylim[2] - xylim[1])/(xylim[4] - xylim[3]) * plotdim[2]/plotdim[1]
  point.angles <- seq(0, 2 * pi, l = 5)[1:4] + pi * rot/180
  crspans <- rep(c(mheight * 3, mheight/6), length.out = 5)
  atten <- c(0.2,1,0.2,0.2)
  xpoints <- cos(point.angles) * r * atten + x
  ypoints <- sin(point.angles) * r * atten + y
  Hstroke(xpoints[c(1,3)], ypoints[c(1,3)],overdraw=overdraw,rough=rough,...)
  Hstroke(xpoints[c(2,4)], ypoints[c(2,4)],overdraw=overdraw,rough=rough,...)
  txtxpoints <- cos(point.angles) * 1.3 * crspans[1] * r * atten + x
  txtypoints <- sin(point.angles) * 1.3 * crspans[1] * r * atten + y
  x_shift <- (xpoints[2] - x)*0.2
  y_shift <- (ypoints[2] - y)*0.2
  x_arrow <- xpoints[2] - x_shift * c(1,0,1) + y_shift * c(1,0,-1) * 0.5
  y_arrow <- ypoints[2] - y_shift * c(1,0,1) + x_shift * c(1,0,-1) * 0.5
  Hpath(x_arrow,y_arrow,overdraw=overdraw,rough=rough,...)
  hand_text(north_name,xpoints[2]+x_shift, ypoints[2]+y_shift,fontfam=fontfam)
  par(oldcex)
}


#' Remove internal holes in polygons
#'
#' Remove internal holes in a \link[sp]{SpatialPolygons} or \link[sp]{SpatialPolygonsDataFrame} object leaving topolgy intact
#' 
#' @usage hole_chop(spdf)
#' spdf \%>\% hole_chop
#'
#' @param spdf  a \link[sp]{SpatialPolygons} or \link[sp]{SpatialPolygonsDataFrame} object
#'
#' @return a \link[sp]{SpatialPolygons}  object with internal holes removed (IDs are the same as \code{spdf})
#' @export
#' @examples
#' data(RA)
#' RA.spdf %>% hole_chop %>% plot
hole_chop <- function(x) {
  x %>% proj4string %>% CRS -> p4s 
  holechop0 <- function(x) ! x@hole
  holechop1 <- function(x) Filter(holechop0,x@Polygons) %>% Polygons(x@ID)
  lapply(x@polygons,holechop1) %>% SpatialPolygons(proj4string=p4s) }


close_poly <- function(xy) {
  xy <- cbind(xy$x,xy$y)
  if (all(xy[nrow(xy),] == xy[1,])) return(xy)
  return(xy[c(1:nrow(xy),1),])
}


ismax <- function(x) x == max(x)

#' Cut out small islands and enclaves
#' 
#' Remove satellite polygons in a \link[sp]{SpatialPolygons} or \link[sp]{SpatialPolygonsDataFrame} object.
#' For each compound polygon,  remove all but the largest component.  Useful for generalised representations.
#'
#' @usage small_chop(spdf)
#' spdf \%>\% small_chop
#'
#' @param spdf a \link[sp]{SpatialPolygons} or \link[sp]{SpatialPolygonsDataFrame} object
#'
#' @return a \link[sp]{SpatialPolygons}  object with satellite polygons removed (IDs are the same as \code{spdf})
#' @export
#' @examples
#' data(RA)
#' RA.spdf %>% small_chop %>% plot
#' 'MULTIPOLYGON (((3 2, 4.5 4, 1 4, 3 2)),((15 5, 40 10, 10 20, 5 10, 15 5)))' %>% readWKT -> p1
#' 'darksalmon' %>% adjustcolor(alpha.f=0.5) -> salmon
#' old.mf <- par('mfrow')
#' par(mfrow=c(1,2))
#' p1 %>% plot(col=salmon); title("Before")
#' p1 %>% small_chop %>% plot(col=salmon); title("After")
#' par(mfrow=old.mf)
small_chop <- function(x,thresh) {
  x %>% proj4string %>% CRS -> p4s 
  if (missing(thresh)) 
    smallchop0 <- function(x) sapply(x,get_area) %>% ismax 
  else
    smallchop0 <- function(x) sapply(x,get_area) >= thresh
  smallchop1 <- function(x)  subset(x@Polygons,smallchop0(x@Polygons)) %>% Polygons(x@ID)
  lapply(x@polygons,smallchop1) %>% SpatialPolygons(proj4string=p4s) }



#' Sketchy drawing of a \link[sp]{SpatialPolygons} or \link[sp]{SpatialPolygonsDataFrame} object
#' 
#' Draws a \link[sp]{SpatialPolygons} or \link[sp]{SpatialPolygonsDataFrame} object in a 'hand drawn'
#' style. 
#'
#' @usage sketch_it(spdf,rough=0.05,...)
#' spdf \%>\% sketch_it(rough=0.05,...)
#'
#' @param spdf  a \link[sp]{SpatialPolygons} or \link[sp]{SpatialPolygonsDataFrame} object
#' @param rough controls the 'roughness' of the sketched polygon edges
#' @param ... parameters passed to \code{\link[graphics]{lines}}
#'
#' @return the input \code{spdf} - useful for pipelines
#' @export
#' @examples
#' "POLYGON ((30 10, 40 40, 20 40, 10 20, 30 10))" %>% readWKT -> p1
#' # Create a blank canvas with extent containing p1
#' p1 %>% make_canvas %>% sketch_it(col='indianred')
sketch_it <- function(x,rough=0.05,lmin=0,...) {
  params <- append(list(rough=rough,lmin=lmin),list(...))
  if (all(sapply(params,length) == 1)) return(do.call(sketch_it_all,append(list(x=x),params)))
  for (i in 1:length(x)) {
    these_params <- lapply(params,elt,i)
    do.call(sketch_it_all,append(list(x=x[i,]),these_params))
  }
  x  %>% invisible
}

elt <- function(arr,i) {
  if (length(arr) == 1) return(arr)
  return(arr[i])
}

sketch_it_all <- function(x,rough=0.05,lmin=0,...) {
  sketch_it0 <- function(x,rough,lmin,...) Hscribble(x@coords[,1],x@coords[,2],rough=rough,lmin=lmin,...)
  sketch_it1 <- function(x,rough,lmin,...) lapply(x@Polygons,sketch_it0,rough=rough,lmin=lmin,...)  
  lapply(x@polygons,sketch_it1,rough=rough,lmin=lmin,...)
  x  %>% invisible
}


sketch_it_old <- function(x,rough=0.05,lmin=0,...) {
  sketch_it0 <- function(x,rough,lmin,...) Hscribble(x@coords[,1],x@coords[,2],rough=rough,lmin=lmin,...)
  sketch_it1 <- function(x,rough,lmin,...) lapply(x@Polygons,sketch_it0,rough=rough,lmin=lmin,...)  
  lapply(x@polygons,sketch_it1,rough=rough,lmin=lmin,...)
  x  %>% invisible
}

#' 'Curvify' polygon-based objects.
#'
#' Curved caracture from a \link[sp]{SpatialPolygons} or \link[sp]{SpatialPolygonsDataFrame} 
#' object, controlled by a shape parameter.  This can pass through the nodes of the
#' original object (-1 < shape parameter < 0) or go near to them (0 < shape parameter < 1).  
#' 
#' @usage curve_it(spdf,s)
#' spdf %>% curve_it(s)
#'
#' @details This is based on the \code{\link[graphics]{xspline}} function. In particular,
#' the shape parameter is the same as in that function.
#'
#' @param spdf a \link[sp]{SpatialPolygons} or \link[sp]{SpatialPolygonsDataFrame} object
#' @param s \code{shape} parameter as in \link[graphics]{xspline}
#'
#' @return a \link[sp]{SpatialPolygons} curved caricature 
#' @export
#' @examples 
#' 'indianred' %>% adjustcolor(alpha.f=0.3) -> ired
#' "POLYGON((0 0,0 2,1 3.5,3 3,4 1,3 0,0 0))" %>% readWKT -> p1
#' p1 %>% make_canvas %>% plot_it(col=ired)
#' p1 %>% curve_it(1) %>% plot_it(col=ired,lty=2)
#' p1 %>% curve_it(0.5) %>% plot_it(col=ired,lty=2)
curve_it <- function(x,s) { 
  x %>% proj4string %>% CRS -> p4s 
  curve_it0 <- function(x,s) xspline(x@coords,shape=s,open=FALSE,draw=FALSE) %>% close_poly %>% Polygon
  curve_it1 <- function(x,s) lapply(x@Polygons,function(q) curve_it0(q,s)) %>% Polygons(x@ID)
  lapply(x@polygons,function(q) curve_it1(q,s)) %>% SpatialPolygons(proj4string=p4s) }


#' Find the outline of a group of polygons 
#'
#' Outline from a \link[sp]{SpatialPolygons} or \link[sp]{SpatialPolygonsDataFrame} object
#' - any internal holes are filled in.
#'  
#' @usage outline_it(spdf)
#' spdf %>% outline_it
#' 
#' @param spdf A \link[sp]{SpatialPolygons} or \link[sp]{SpatialPolygonsDataFrame}
#'
#' @return A \link[sp]{SpatialPolygons} outline of the input object
#' @export
#' @examples
#' data(RA)
#' RA.spdf %>% small_chop %>% gSimplify(tol=11000) %>% tidy_it %>% outline_it %>% make_canvas %>% plot_it
outline_it <- function(spdf) {
  spdf %>% gUnaryUnion %>% hole_chop 
}


#' Plotting function - pipeline friendly
#' 
#' Works like \code{plot} but returns the input object (good in pipelines).
#' Requires \link{make_canvas} to have been called (probably in a pipeline).
#' 
#' @usage plot_it(spdf,...)
#' spdf %>% plot_it(...)
#'
#' @param spdf A \link[sp]{SpatialPolygons} or \link[sp]{SpatialPolygonsDataFrame}
#' @param ... Other parameters to pass on to \link[graphics]{plot}
#'
#' @return as \code{spdf}
#' @export
#' @examples
#' data(all_ireland)
#' all_ireland %>% make_canvas %>% plot_it
plot_it <- function(spdf,...) {
  plot(spdf,add=TRUE,...)
  spdf %>% invisible
}

#' Create a new canvas
#'
#' Create a new plot whose extent fits a given \link[sp]{Spatial} object. Returns the input spatial object,
#' which is very useful for pipelines.
#' 
#' @usage make_canvas(spdf)
#' spdf \%>\% make_canvas
#' 
#' @param spdf A \link[sp]{SpatialPolygons} or \link[sp]{SpatialPolygonsDataFrame} object 
#'
#' @return Echos \code{spdf} - this is useful for pipelines using \code{\%>\%} 
#' @export
#' @examples
#' # Here make_canvas is inserted in a pipeline after a simplified and tidied 
#' # map of Irish NUTS3 regions is created.  A sketchy rendition is then added to the
#' # canvas.
#' data(RA) 
#' RA.spdf %>% small_chop %>% gSimplify(tol=11000) %>% tidy_it %>% make_canvas %>% sketch_it(col='orange')
make_canvas <- function(spdf) {
  par(mar=c(0.5,0.5,3,0.5))
  plot(spdf,border=NA)
  return(spdf)
}

#' Generalise an object
#'
#' Apply a generalisation algorithm to a \link[sp]{SpatialPolygons} or \link[sp]{SpatialPolygonsDataFrame} object.
#' 
#' @usage generalise_it(spdf,tol)
#' spdf %>% generalise_it(tol)
#' 
#' @param spdf  A \link[sp]{SpatialPolygons} or \link[sp]{SpatialPolygonsDataFrame} object 
#' @param tol The tolerance of the generalising algorithm.  The higher this is,  the cruder the generalisation.  For caricaturing,  it should be reasonably high - eg 10000 for the Irish National Grid in metres.
#'
#' @return A generalised \link[sp]{SpatialPolygons} object
#' @export
#' @examples
#' data(RA)
#' RA.spdf %>% small_chop %>% generalise_it(10000) %>% plot
generalise_it <- function(spdf,tol) {
  gSimplify(spdf,tol=tol)
}

#' Make all polygons in an object convex
#' 
#' Replace polygons in \link[sp]{SpatialPolygons} or \link[sp]{SpatialPolygonsDataFrame} with convex hulls
#'
#' @usage hull_it(spdf)
#' spdf %>% hull_it
#'
#' @param spdf a \link[sp]{SpatialPolygons} or \link[sp]{SpatialPolygonsDataFrame} object
#'
#' @return a \link[sp]{SpatialPolygons} object with convex hulls of polygons (may overlap)
#' @export
#' @examples
#' "MULTIPOLYGON (((30 20, 45 40, 35 35, 25 35, 10 40, 30 20)),
#'    ((15 5, 40 8, 40 10, 35 8, 10 20, 5 10, 15 5)))" %>% readWKT -> p1
#' 'lavender' %>% adjustcolor(alpha.f=0.5) %>% lav
#' p1 %>% plot(col=lav)
#' p1 %>% hull_it %>% plot(col=lav,add=TRUE)
hull_it <- function(x) {
  x %>% proj4string %>% CRS -> p4s 
  hull_it0 <- function(x) x@coords[chull(x@coords),] %>% Polygon 
  hull_it1 <- function(x) lapply(x@Polygons,hull_it0) %>% Polygons(x@ID)
  lapply(x@polygons,hull_it1) %>% SpatialPolygons(proj4string=p4s)
  }

#' Tidy up an object with overlapping polygons
#'
#' Cut out overlaps in a \link[sp]{SpatialPolygons} or \link[sp]{SpatialPolygonsDataFrame} object
#'
#' @param x a \link[sp]{SpatialPolygons} or \link[sp]{SpatialPolygonsDataFrame} object
#'
#' @return a \link[sp]{SpatialPolygons} object with overlaps cut out
#' @export
#' @examples
#' # Create an object with two overlapping polygons
#' "MULTIPOLYGON (((30 20, 45 40, 10 40, 30 20)))" %>% readWKT(id="George") -> george
#' "MULTIPOLYGON (((15 5, 40 10, 41 35, 10 20, 5 10, 15 5)))" %>% readWKT(id="Mildred") -> mildred
#' rbind(george,mildred) -> p1
#' c("George","Mildred") -> ID
#' 
#' # Create a color scheme based on IDs of polygons in p1
#' 'olivedrab' %>% adjustcolor(alpha.f=0.5) -> olive 
#' 'lemonchiffon' %>%  adjustcolor(alpha.f=0.5) -> lemon
#' c(George=olive,Mildred=lemon) -> cammo
#' 
#' # Useful for the demo
#' function(x) sapply(slot(x,'polygons'), function(y) slot(y,'ID')) -> get.IDs
#' 
#' par('mfrow') -> old.mf
#' par(mfrow=c(1,2))
#' p1 %>% plot(col=cammo[p1 %>% get.IDs]); 'Before' %>% title
#' p1 %>% tidy_it -> p1_tidy
#' p1_tidy %>% plot(col=cammo[p1_tidy %>% get.IDs]); 'After' %>% title
#' par(mfrow=old.mf)
tidy_it <- function(x) {
  IDs <- sapply(slot(x,"polygons"),function(y) slot(y,"ID")) # maybe get.IDs?
  first <- TRUE
  repeat {
    clashes <- gOverlaps(x,byid=TRUE,returnDense=FALSE)
    if (first) {
      first <- FALSE
      non_clasher <- sapply(clashes,length) == 0
    } else {
      non_clasher[1] <- TRUE
      non_clasher <- non_clasher | (sapply(clashes,length) == 0)
    }
    if (all(non_clasher)) break
    areas <- gArea(x,byid=TRUE)
    areas[non_clasher] <- Inf
    i <- which.min(areas)
    x <- rbind(x[i],gDifference(x,x[i],byid=TRUE))
    non_clasher <- c(non_clasher[i],non_clasher[-i])
    IDs <- c(IDs[i],IDs[-i])
    spChFIDs(x) <- IDs
  }
  spChFIDs(x) <- IDs
  return(x)
}

#' Dilate (expand) polygons in an object
#'
#' Dilate the polygons in a \link[sp]{SpatialPolygons} or \link[sp]{SpatialPolygonsDataFrame} object
#'
#' @usage dilate_it(spdf,r)
#' spdf %>% dilate_it(r)
#' 
#' @param spdf A \link[sp]{SpatialPolygons} or \link[sp]{SpatialPolygonsDataFrame} object
#' @param r Radius of dilation
#'
#' @return A \link[sp]{SpatialPolygons} object with the dilation applied
#' @export 
#' @examples
#' "POLYGON ((30 10, 40 40, 20 40, 10 20, 30 10))" %>% readWKT -> p1
#' dsg <- adjustcolor('darkseagreen',alpha.f=0.5)
#' p1 %>% dilate_it(3) %>% plot(col=dsg)
#' p1 %>% plot(col=dsg,add=TRUE)
#' 
#' # A sketchy version
#' p1 %>% dilate_it(3) %>% make_canvas %>% sketch_it(col='navyblue')
dilate_it <- function(spdf,r) {
  gBuffer(spdf,width=r,byid=TRUE)
}

#' Contract polygons in an object
#'
#' @param spdf A \link[sp]{SpatialPolygons} or \link[sp]{SpatialPolygonsDataFrame} object
#' @param r  Radius of contraction
#'
#' @return A \link[sp]{SpatialPolygons} object with contraction applied
#' @export
#' @examples
#' "POLYGON ((30 10, 40 40, 20 40, 10 20, 30 10))" %>% readWKT -> p1
#' dv <- adjustcolor('darkviolet',alpha.f=0.5)
#' p1 %>% dilate_it(3) %>% plot(col=dv)
#' p1 %>% plot(col=dv,add=TRUE)
#' 
#' # A sketchy version
#' p1 %>% contract_it(3) %>% make_canvas %>% sketch_it(col='chocolate')
contract_it <- function(spdf,r) {
  gBuffer(spdf,width=-r,byid=TRUE)
}


#' Install some Google handwriting-style fonts
#'
#' Use this function once to install some hand-written style fonts
#' (from Google Fonts \url{https://www.google.com/fonts}) into R.  
#' 
#' @details For this function to run successfully, computer must be connected to the internet.
#' At the time of writing,  \pkg{caricRture} uses \pkg{showtext} to handle the fonts.  That package doesn't
#' work well with the RStudio graphics device - better to use \code{quartz} or \code{X11} to write to a floating
#' window if using RStudio. Each font has a two-letter abbreviation,  which can be used as a \code{family} parameter
#' in \code{par}.  The abbreviations and associated fonts are: 
#' \describe{
#' \item{am}{Amatic SC}
#' \item{dk}{Dekko}
#' \item{gr}{Covered By Your Grace}
#' \item{hl}{Handlee}
#' \item{pm}{Permanent Marker}
#' \item{rs}{Rock Salt}
#' \item{ws}{Waiting for the Sunrise}
#' }
#' 
#'
#' @return Nothing is returned
#' @export
#' @examples
#' get_fonts() # Thats basically it - once run,  fonts will be installed
get_fonts <- function() {
  font.add.google("Dekko","dk")
  font.add.google("Amatic SC", "am")
  font.add.google("Covered By Your Grace","gr")
  font.add.google("Handlee","hl")
  font.add.google("Permanent Marker","pm")
  font.add.google("Rock Salt","rs")
  font.add.google("Waiting for the Sunrise","ws")
}


#' Transform to OpenStreetMap (*Google Mercator*) Projection
#' 
#' Transforms a \link[sp]{Spatial} object 
#' to 'Google Mercator' projection - this makes the object work well with a
#' backdrop.
#' 
#' @usage to_osm(spdf)
#' spdf %>% to_osm
#'
#' @param spdf a \link[sp]{Spatial*} or \link[sp]{Spatial*DataFrame} object 
#'
#' @return \code{spdf} transformed to 'Google Mercator' projection
#' @export
#' @examples
#' data(all_ireland) 
#' # Transform to osm projection,  enjoy nice rectangular image...
#' all_ireland %>% to_osm %>% 
#'   make_backdrop(type='nps') %>% plot_it(border='navyblue')
to_osm <- function(spdf) {
  spTransform(spdf,osm()) 
}

#' Make an OpenStreetMap, Stamen or other map backdrop
#'
#' Create a backdrop for sketchy maps,  based on the extent of \code{spdf}.  The backdrop
#' can be from OpenStreetMap, Google, Stamen, ESRI, bing, Apple or other sources
#' depending on location. 
#'
#' @usage make_backdrop(spdf,bw,sepia,...)
#' spdf %>% make_backdrop(bw,sepia,...)
#'
#' @param spdf a \link[sp]{SpatialPolygons} or \link[sp]{SpatialPolygonsDataFrame} object
#' @param bw A variable indicating whether to create a monochrome backdrop - 0 indicates use colour, 1 is standard brightness, other values vary brightness
#' @param sepia  A variable indicating whether to create a sepia backdrop - 0 indicates use colour, 1 is standard brightness, other values vary brightness - this overwrites \code{bw}
#' @param ...  parameters passed on to \link[OpenStreetMap]{openmap} 
#' 
#' @details This function requires that \code{spdf} has a well-defined \code{proj4string} in
#' order to match the backdrop map with the extent of the spatial object.
#' 
#' @return Echos \code{spdf} - this is useful for pipelines using \code{\%>\%} 
#' @export
#' @examples
#' # Here make_canvas is inserted in a pipeline after a simplified and tidied 
#' # map of Irish NUTS3 regions is created.  A sketchy rendition is then added to the
#' # backdrop of type osm-bw (monochrome OpenStreetMap)
#' data(RA) 
#' RA.spdf %>% small_chop %>% gSimplify(tol=11000) %>% tidy_it %>% 
#'   make_backdrop(type='osm',sepia=0.8) %>% sketch_it(col='darkred',lwd=2,rough=0.08)
make_backdrop <- function(spdf,bw=0,sepia=0,...) {
  spdf %>% spTransform('+init=epsg:4326') %>% slot('bbox') -> bx
  ul <- c(bx[2,2],bx[1,1])
  lr <- c(bx[2,1],bx[1,2])
  mp <- openmap(ul,lr,...)
  all_tiles <- mp$tiles
  if (bw != 0) {
    TC <- 1
    for (a_tile in all_tiles) {
      a_tile$colorData %>% col2rgb %>% divide_by(256) -> ctabl
      c(0.2125,0.7154,0.0721) %*% ctabl %>% multiply_by(bw) %>% pmin(1) %>% grey -> gv
      mp$tiles[[TC]]$colorData <- gv
      TC <- TC + 1
    }
  }
  if (sepia != 0) {
    m2rgb <- function(m) rgb(m[1,],m[2,],m[3,])
    TC <- 1
    for (a_tile in all_tiles) {
      a_tile$colorData %>% col2rgb %>% divide_by(256) -> ctabl
      c(0.2125,0.7154,0.0721) %*% ctabl -> ctabl
      c(1.3510,1.2030,0.9370) %*% ctabl %>% multiply_by(sepia) %>% pmin(1) %>% m2rgb -> gv 
      mp$tiles[[TC]]$colorData <- gv
      TC <- TC + 1
    }
  }
  tmp <- openproj(mp,proj4string(spdf))
  plot(tmp)
  spdf %>% invisible
}

#' Draw a sketchy choropleth map
#'
#' @param sp A  \link[sp]{SpatialPolygonsDataFrame} object
#' @param v A variable (or expression) to control the colouring of the map
#' @param shading A shading scheme - see \link[GISTools]{auto.shading}
#' @param ... other parameters passed to \link[GISTools]{choropleth}
#'
#' @return The input \code{sp} - useful for pipelining
#' @export
#' @examples
#' x <- 7
#' 
choro_sketch_it <- function (sp, v, shading = auto.shading, ...) 
{
  try(v <- eval(substitute(v),envir=sp@data),silent=TRUE)
  if (is.function(shading)) {
    sh <- shading(v)
  } else {
    sh <- shading
  }
  i = sh$cols[1 + findInterval(v, sh$breaks)]
  sketch_it(sp, col = i, ...)
  invisible(sh)
}


legend_shapes <- function(tlx,tly,side,n,drop) {
  square <- function(x0,y0,l) {
    pl <- cbind(c(x0,x0,x0+l,x0+l,x0),c(y0,y0+l,y0+l,y0,y0))
    Polygon(pl) }
  pols <- vector('list',n)
  for (i in 1:n) pols[[i]] <- Polygons(list(square(tlx,tly-drop*(i-1),side)),ID=i)
  SpatialPolygons(pols)
}

#' Sketchy legend with 'handwritten' font
#' 
#' Similar to the \link[graphics]{legend} command,  
#' but chooses a hand writing styled font,  and draws sketchy legend
#' symbols
#' 
#' @usage hand_legend <- function(labels,tlx,tly,side,gap,cex=1,fontfam='am',...)
#' labels \%>\% hand_legend(...)
#'
#' @param labels a character string array of legends labels
#' @param tlx top left x coordinate of legend
#' @param tly top left y coordinate of legend
#' @param fontfam Handwritten font family - see \link{get_fonts}
#' @param side length of size of square in legend
#' @param gap gap between bottom of one legend and top of next
#' @param cex scale of legend text 
#' @param ... other parameters passed on to the \link{sketch_it} function
#' 
#'
#' @return No value returned
#' @export
#' @examples
#' # Here make_canvas is inserted in a pipeline after a simplified and tidied 
#' # map of Irish NUTS3 regions is created.  A sketchy rendition is then added to the
#' # canvas.
#' get_fonts()
#' data(RA) 
#' RA.spdf %>% small_chop %>% gSimplify(tol=11000) %>% tidy_it %>% clone_data(RA.spdf) -> RA.spdf2
#' shadecol <- ifelse(RA.spdf2$NUTS2 == 'IE01','indianred','dodgerblue')
#' 
#' RA.spdf2 %>% make_canvas %>% sketch_it(col=shadecol)
#' c('NUTS2 I','NUTS2 II') %>% hand_legend(-22450,427691,45000,15000,col=c('indianred','dodgerblue'),cex=1.8)
#' 
hand_legend <- function(labels,tlx,tly,side,gap,cex=1,fontfam='am',...) {
  drop <- side + gap
  n <- length(labels)
  lshp <- legend_shapes(tlx,tly,side,n,drop)
  lshp %>% sketch_it(...)
  y_locs <- tly + side/2 - (0:(n-1))*drop
  x_locs <- tlx + side*1.1 
  for (i in 1:n) hand_text(labels[i],x_locs,y_locs[i],cex=cex, pos=4, fontfam=fontfam)
}


# # Some demo stuff
# # 
# par(mfrow=c(2,2),mar=c(1,1,6,1)/4)
# RA.spdf %>% smallchop %>% gSimplify(tol=12000) -> RA.gen
# RA.gen %>% tidy_it %>% gBuffer(width=-1000,byid=TRUE) %>% 
#     plot(col=adjustcolor('navyblue',alpha.f=0.4))
# title('London Olympics')
# RA.gen %>% gBuffer(width=4000,byid=TRUE) %>% tidy_it %>% gBuffer(width=0,byid=TRUE) %>% 
#   plot(col=adjustcolor('lawngreen',alpha.f=0.5))
# title('Rounded Lines')
# (RA.gen %>% tidy_it %>% curve_it(1) %>% gBuffer(width=1,byid=TRUE) %>% tidy_it -> blob) %>% 
#     plot(col=adjustcolor('indianred',alpha.f=0.4))
# title('Loose Blobs')
# blob %>% gBuffer(width=5500,byid=TRUE) %>% tidy_it %>%  
#     plot(col=adjustcolor('gold',alpha.f=0.4))
# title('Tight Blobs')
# 
# RA.gen %>% tidy_it %>% plot
