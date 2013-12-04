#' summary.ses
#' @description Compute summary statistics of a ses object.
#' @param ses An ses object
#' @export
summary.ses <- function(ses){
	
	
	
}

#' plot.ses
#' @description A \code{plot()} method to draw relevant chart of a ses object.
#' @param ses An ses object.
#' @param pch Plotting character.
#' @param cex Plotting character size.
#' @param fill Should the terrestrial area be filled with color.
#' @param m.col The color to use in order to fill the terrestrial area.
#' @param ... Other parameters to be passed to \code{plot()} or \code{par()}.
#' @details Bathymetry taken from \url{https://www.ga.gov.au/products/servlet/controller?event=GEOCAT_DETAILS&catno=71552}. Isobaths from -200 to -4400 metters.
#' @export
#' @examples 
#' path <- system.file("extdata", package="SES")
#' pathname <- file.path(path, "2011-16_SES_example_accelero.mat")
#' ses <- importSES(pathname)
#' plot(ses)
plot.ses <- function(ses, pch=19, cex=1, fill=TRUE, m.col="gray", isobath=-seq(1000, 4000, 1000), ...){
	
	findVars(c("Ind.id", "stat"), ses)
	findVars(c("Lat", "Lon", "Time"), stat)
	require(maps)
	plot(Lat ~ Lon, pch=pch, cex=cex*0.3, ...)
	title(main=paste(Ind.id, ": ", nrow(stat), "dives"), line=2)
	mtext(text=paste(range(Time), collapse="  -->  "), side=3, line=.75)
	points(Lat ~ Lon, data=stat[1, ], col="deeppink3", pch="*", cex=4)
	map(add=TRUE, fill=fill, col=m.col)
	bath <- readShapeSpatial(file.path(system.file("extdata", package="SES"), "bath.shp"))
	bath <- bath[bath$CONTOUR %in% isobath, ]
	plot(bath, new=TRUE)
	
}


# writeSpatialShape(b, fn="./inst/extdata/bath")

# bath <- list(x=1:10, y=1:10,
# 			 range=c(68, 79, -56, -48),
# 			 names=seq(200, 2000, by=200))
# class(bath) <- "map"