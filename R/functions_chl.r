#' mZeu2layer
#' 
#' Assign to a record the SEAPODYM layer it belongs ond using the depth expressed in multiple of Zeu.7
#' 
#' @param mZeu The depth expressed as multiple of the euphotic depth.
#' @details Layers limits taken to be 1.5, 4.5 and 10.5 time Zeu.
#' @family chl
#' @export
mZeu2layer <- function(mZeu){
  Layer <- rep(NA, length(mZeu))
  Layer[mZeu > 4.5] <- "Bathy" ; Layer[is.na(Layer) & mZeu > 1.5] <- "Meso" ; Layer[is.na(Layer)] <- "Epi"
  Layer[mZeu == Inf] <- NA
  return(as.character(Layer))
}


#' modelMorel
#' 
#' Predict euphotic depth (m) from the surface chlorophyll concentration.
#' 
#' @param Chl Vector giving the chlorophyll a concentration in the surface layer.
#' @param stat A statdives object. 'chl' argument is ignored. The values are taken from this statdive object.
#' @param append Should the variable be returned with the entire statdives object ?
#' @references Morel and Berthon (1989)
#' @family chl
#' @export
modelMorel <- function(Chl, stat=NULL, append=TRUE){
  if (!is.null(stat)) {findVars("Chl", stat)}
  Zeu <- rep(NA, length(Chl))
  cond <- !is.na(Chl) ; Chl <- Chl[cond]
  Chl.tot.strat <- 38*Chl^(0.425)
  Chl.tot.strat[Chl > 1] <- 40.2*Chl[Chl > 1]^(0.507)
  Zeu[cond] <- 200*Chl.tot.strat^(-0.293)
  if (!is.null(stat) && append){
    stat$Zeu <- Zeu
    return(stat)
  } else {return(Zeu)}
}
