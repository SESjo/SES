#' SESplot
#' 
#' To draw relevant chart of an object generated with \code{importSES()} 
#' such as tdr', 'statdives', 'tdr3D' or 'ststdives3D'.
#' 
#' @inheritParams SESplot.statdives
#' @param ... Other parameters to be passed to methods.
#' @details Includes bathymetry taken from 
#' \url{https://www.ga.gov.au/products/servlet/controller?event=GEOCAT_DETAILS&catno=71552}. 
#' Available isobaths range from -200 to -4400 meters by 200 m.
#' @import maptools fields maps 
#' @family SESplot
#' @export
#' @examples
#' \dontrun{
#' require(maps)
#' path <- system.file("extdata", package="SES")
#' pathname <- file.path(path, "2011-16_SES_example_accelero.mat")
#' ses <- importSES(pathname)
#' 
#' # Example: class 'ses' / 'statdives'
#' SESplot(ses, isobath=-1000, colorvar=ses$stat$Catch.numb)
#' library("RColorBrewer")
#' mycol <- brewer.pal(n=10, name="BrBG")
#' SESplot(ses, ses$stat$Dive.dur, pts.args=list(pch=""), implt.args=list(col=mycol), img.args=list(nx=150, ny=150))
#' 
#' # Example: class 'tdr'
#' SESplot(ses$tdr, colorvar=ses$tdr$Light, cond=ses$tdr$Dive.id == 650)
#' SESplot(ses$tdr, colorvar=ses$tdr$is.Catch, cond=ses$tdr$Dive.id == 650)
#' 
#' # No provided data for 'ses3D': Syntax memo
#' # SESplot(ses3d) # Same as for classic 'ses' class
#' # 3D-Scatterplot of the 1015th dive with colored variable.
#' # SESplot(ses3d$tdr, cond=ses3d$tdr$Dive.id==1015, colorvar=ses3d$tdr$Distance)
#' }
SESplot <- function(obj, colorvar=NULL, isobath=NULL, ...){
  UseMethod("SESplot")
}

#' SESplot.ses
#' 
#' Method for 'ses' objects.
#' 
#' @inheritParams SESplot.statdives
#' @param ... Other parameters to be passed to \code{SESplot.statdives}.
#' @details Includes bathymetry taken from 
#' \url{https://www.ga.gov.au/products/servlet/controller?event=GEOCAT_DETAILS&catno=71552}. 
#' @S3method SESplot ses
SESplot.ses <- function(obj, colorvar=NULL, cond=NULL, isobath=NULL, 
                        pts.args=list(), map.args=list(), img.args=list(), 
                        implt.args=list(), plt.args=list(), ...) {
  findVars(c("Ind.id", "stat"), obj)
  legend <- try(findVars("colorvarname", list(...), type='check'))
  if (inherits(legend, "try-error")) {
    legend <- gsub('^.*\\$', '', deparse(substitute(colorvar)))
    SESplot.statdives(stat, colorvar, cond, isobath, pts.args, 
                      map.args, img.args, implt.args, plt.args, 
                      colorvarname = legend, ...)
  } else {
    SESplot.statdives(stat, colorvar, cond, isobath, pts.args, 
                      map.args, img.args, implt.args, plt.args, ...)
  }
  title(main=paste(Ind.id, ": ", nrow(stat), "dives"), line=2)
}

#' SESplot.statdives
#' 
#' Method for 'statdives' objects.
#' 
#' @param obj An object to plot
#' @param colorvar Optionnal \code{obj} variable to plot with the color of symbols.
#' @param cond Optionnal logical vector to subset the rows of \code{obj}.
#' @param isobath Isobaths to draw (Bathymetry) . Default is NULL (none). 
#' Isobath at -1000 m corresponds approximatively to the Kerguelen shell 
#' boarder. Slow down the function (shapefile loading). Available isobaths range 
#' from -200 to -4400 meters by 200 m.
#' @param pts.args Other parameters to be passed to \code{\link{points}}. 
#' Defaults: \code{list(pch = 19, cex = .1)}.
#' @param map.args Other parameters to be passed to \code{\link{map}}. 
#' Defaults: \code{list(fill = TRUE, col = "gray")}.
#' @param img.args Other parameters to be passed to \code{\link{as.image}}.
#' Notice the possibility to set \code{nx} and \code{ny} arguments.
#' @param implt.args Other parameters to be passed to \code{\link{image.plot}}.
#' Defaults: \code{list(legend.width = 1, legend.mar = 7.8, legend.shrink = .7, 
#' legend.lab = deparse(substitute(colorvar)), legend.line = 3.5)} with 
#' \code{par(mar = c(4.5,4.5,3.5,8.5))}. Notice the possibility to set \code{zlim}, 
#' \code{col}, \code{breaks} arguments.
#' @param plt.args Other parameters to be passed to \code{\link{plot}}.
#' Notice the possibility to set \code{xlim} and \code{ylim} arguments.
#' @param colorvarname Name for the legend. For compatibility when called from 
#' \code{SESplot.ses()}
#' @details Includes bathymetry taken from 
#' \url{https://www.ga.gov.au/products/servlet/controller?event=GEOCAT_DETAILS&catno=71552}. 
#' If this error show up 'data set ‘worldMapEnv’ not found' 
#' try \code{require(maps)}.
#' @import maptools fields maps
#' @S3method SESplot statdives
SESplot.statdives <- function(obj, colorvar=NULL, cond=NULL, isobath=NULL, pts.args=list(), 
                              map.args=list(), img.args=list(), implt.args=list(), plt.args=list(),
                              colorvarname) {
  if (missing(colorvarname)){
    colorvarname <- deparse(substitute(colorvar))
    colorvarname <- gsub('^.*\\$', '', colorvarname)
  }
  # 	if (is.logical(colorvar)) colorvar <- as.numeric(colorvar)
  nas <- apply(is.na(obj), 1, any)
  obj <- obj[!nas, ] ; colorvar <- colorvar[!nas]
  if (!is.null(cond)){
    obj <- obj[cond & !nas, ]
    colorvar <- colorvar[cond]
  }
  if (inherits(obj, "statdives3D")) {
    findVars(c("Lat.i", "Lon.i", "Dive.id"), obj, varnames=c("Lat", "Lon", "Time"))
  } else {
    findDefaultVars(c("Lat", "Lon", "Time"), obj, type.obj="stat", substring=FALSE)
  }
  # Settings
  opar <- par("mar") ; on.exit(par(opar))
  par(mar=c(4.5,4.5,3.5,8.5))
  defaults <- list(pts.args=list(pch=19, cex=.1), map.args=list(fill=TRUE, col="gray"), 
                   img.args=list(na.rm=TRUE),
                   implt.args=list(legend.width=1, legend.mar=7.8, legend.shrink=.7,
                                   legend.lab=colorvarname, legend.line=3.5),
                   plt.args=list())
  chkArgs <- function(args, def) c(args, def[!names(def) %in% names(args)])
  for (i in seq_along(defaults)) {
    assign(names(defaults)[i], chkArgs(get(names(defaults)[i]), defaults[[i]]))
  }
  # Call plot functions
  do.call('plot', c(list(Lat ~ Lon, type="n", xlab="Longitude", ylab="Latitude"), plt.args))
  if (!is.null(colorvar)) {
    im <- do.call('as.image', c(list(colorvar, x=data.frame(Lon, Lat)), img.args))
    do.call('image.plot', c(list(im, add=TRUE), implt.args))
  }
  do.call('points', c(list(Lat ~ Lon), pts.args))
  mtext(text=paste(range(Time), collapse="  -->  "), side=3, line=.75)
  do.call('map', c(list(add=TRUE), map.args))
  points(Lon[1], Lat[1], col="deeppink3", pch="*", cex=3)
  if (!is.null(isobath)) {
    bath <- readShapeSpatial(file.path(system.file("extdata", package="SES"), "bath.shp"))
    bath <- bath[bath$CONTOUR %in% isobath, ]
    lines(bath, lty=2)
  }
}

#' SESplot.tdr3D
#' 
#'  Method for 'tdr3D' objects.
#'  
#' @inheritParams SESplot.statdives
#' @param scatter.args Other parameters to be passed to \code{plot3d} if 
#' \code{dev} is set to \code{'rgl'}, to \code{scatterplot3d} otherwise.
#' Defaults: \code{list(pch = 19, cex.symbol = .1, mar = c(4.2,4.2,3.5,9)))}
#' @param colscale.args Other parameters to be passed to \code{\link{color.scale}}. 
#' Defaults: \code{list()}.
#' @param implt.args Other parameters to be passed to \code{\link{image.plot}}. 
#' Defaults: \code{list(legend.width = 1, legend.mar = 7.8, legend.shrink = .7, 
#' legend.lab = colorvarname, legend.line = 3.5)}.
#' @param dev The device to use: to choose in c('rgl', 'default'). See 
#' consequences in details.
#' @details With a \code{colorvar} of type 'integer', 'logical' or 'factor' color 
#' the scale can be controled with \code{palette()} while type 'double' must be 
#' handled with \code{colscale.args} argument. When \code{dev} is set \code{'rgl'}
#' the figure can be rotated and zoomed interactively but no color legend can 
#' be added.
#' @import fields rgl
#' @S3method SESplot tdr3D
#' @examples
#' \dontrun{
#' SESplot(ses3d$tdr, cond=ses3d$tdr$Dive.id==1015, colorvar=ses3d$tdr$Distance)
#' # Use manipulate to change view angle
#' require(manipulate)
#'  manipulate(SESplot(ses3d$tdr, cond=ses3d$tdr$Dive.id==2500,
#'   scatter.args=list(angle=angle)), angle=slider(0, 360))
#' }
SESplot.tdr3D <- function(obj, colorvar=NULL, cond=NULL, isobath=NULL,
                          scatter.args=list(), colscale.args=list(), implt.args=list(), colorvarname, dev=c('rgl', 'default')){
  if (missing(colorvarname)) {colorvarname <- deparse(substitute(colorvar))}
  if (!is.null(cond)) {
    obj <- obj[cond, ]
    colorvar <- colorvar[cond]
  }
  findDefaultVars(c("Lat", "Lon", "Time", "Depth"), obj, type.obj="tdr3D")
  # Settings
  defaults <- list(scatter.args=list(pch=19, cex.symbol=.1, mar=c(3.5,3.5,2.5,9)), 
                   colscale.args=list(),
                   implt.args=list(legend.width=1, legend.mar=7.8, legend.shrink=.7,
                                   legend.lab=colorvarname, legend.line=3.5))
  chkArgs <- function(args, def) c(args, def[!names(def) %in% names(args)])
  for (i in seq_along(defaults)) {
    assign(names(defaults)[i], chkArgs(get(names(defaults)[i]), defaults[[i]]))
  }
  # Colors
  if (!is.null(colorvar)){
    if (is.integer(colorvar)){
      if(any(colorvar == 0L) & !any(colorvar == 1L)){
        colorvar[which(colorvar == 0L)] <- 1L
      } else if (any(colorvar == 0L) & any(colorvar == 1L)) {
        stop("0 is not a color.")
      } else if (nval(colorvar) == 1){colorvar <- rep(1L, length(Depth))}
    }
    cols <- switch(typeof(colorvar),
                   double = do.call('color.scale', c(list(z=colorvar), colscale.args)),
                   logical  = colorvar + 1,
                   character = as.numeric(as.factor(colorvar)),
                   factor = as.numeric(colorvar),
                   integer = colorvar)
  } else {
    cols <- rep(1, length(Depth))
  }
  # Plot
  if (pmatch(dev, 'default', nomatch=0)){
    sp3 <- do.call(getFromNamespace('scatterplot3d', 'scatterplot3d'),
                   c(list(Lon, Lat, Depth, color=cols, 
                          xlab="Lon", ylab="Lat", zlab="Depth"), scatter.args))
                   sp3$points3d(Lon[1], Lat[1], Depth[1], col="deeppink3",
                                pch="*", cex=3)
                   if (!is.null(isobath)) {for (iso in isobath) {sp3$plane3d(iso, 0, 0)}}
                   if (!is.null(colorvar) & is.numeric(colorvar)) {
                     do.call('image.plot', c(list(legend.only=TRUE, add=TRUE,
                                                  z=colorvar, col=unique(cols[order(colorvar)])), implt.args))
                   }
                   sp3$points3d(Lon, Lat, rep(min(pretty(Depth)), length(Lon)), type='l', lty="dotted")
                   invisible(sp3)
  }else if (pmatch(dev, 'rgl', nomatch=0)){
    do.call(plot3d, c(list(Lon[-1], Lat[-1], Depth[-1], col=cols, xlab="Lon", ylab="Lat", zlab="Depth"), scatter.args))
    points3d(Lon[1], Lat[1], Depth[1], col="deeppink3", size=12, add=TRUE)
    lines3d(Lon, Lat, rep(min(pretty(Depth)), length(Lon)), lty=2)
    grid3d(c("x", "y", "z"))
    if (!is.null(isobath)) {for (iso in isobath) {planes3d(1, 1, -1, iso, alpha=.5, col="plum2")}}
  }
}

#' SESplot.tdr
#' 
#' Method for with 'tdr' objects.
#' 
#' @inheritParams SESplot.tdr3D
#' @param plt.args Other parameters to be passed to \code{plot}.
#' @details With a \code{colorvar} of type 'integer', 'logical' or 'factor' color 
#' the scale can be controled with \code{palette()} while type 'double' must be 
#' handled with \code{colscale.args} argument.
#' @S3method SESplot tdr
SESplot.tdr <- function(obj, colorvar=NULL, cond=NULL, isobath=NULL,
                        plt.args=list(), colscale.args=list(), implt.args=list()){
  colorvarname <- deparse(substitute(colorvar))
  if (!is.null(cond)) {
    obj <- obj[cond, ]
    colorvar <- colorvar[cond]
  }
  findDefaultVars(c("Time", "Depth"), obj, type.obj="tdr")
  # Settings
  defaults <- list(plt.args=list(pch=19, cex=.2), 
                   colscale.args=list(),
                   implt.args=list(side=3, legend.width=1, legend.mar=7.8, legend.shrink=.7,
                                   legend.lab=colorvarname, legend.line=3.5))
  chkArgs <- function(args, def) c(args, def[!names(def) %in% names(args)])
  for (i in seq_along(defaults)) {
    assign(names(defaults)[i], chkArgs(get(names(defaults)[i]), defaults[[i]]))
  }
  # Colors
  if (!is.null(colorvar)){
    if (is.integer(colorvar)){
      if(any(colorvar == 0L) & !any(colorvar == 1L)){
        colorvar[which(colorvar == 0L)] <- 1L
      } else if (any(colorvar == 0L) & any(colorvar == 1L)) {
        stop("0 is not a color.")
      } else if (nval(colorvar) == 1){colorvar <- rep(1L, length(Depth))}
    }
    cols <- switch(typeof(colorvar),
                   double = do.call('color.scale', c(list(z=colorvar), colscale.args)),
                   logical  = colorvar + 1,
                   character = as.numeric(as.factor(colorvar)),
                   factor = as.numeric(colorvar),
                   integer = colorvar)
  } else {
    cols <- rep(1, length(Depth))
  }
  if (sign(median(Depth)) == 1) Depth <- -1 * Depth
  do.call('plot', c(list(Depth ~ Time, col=cols), plt.args))
  if (!is.null(isobath)) {lines(h=isobath, lty=2)}
  if (!is.null(colorvar) & is.double(colorvar)) {
    opar <- par("mar") ; on.exit(par(opar))
    par(mar=c(4.2,4.2,3.5,7.9))
    do.call('image.plot', c(list(legend.only=TRUE, add=TRUE,
                                 z=colorvar, col=unique(cols[order(colorvar)])), implt.args))
  }
}