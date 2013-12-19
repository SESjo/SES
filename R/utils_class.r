#' is.ses
#' 
#' Test if an object belong to the 'ses' class
#' 
#' @param x The object
#' @export
#' @family is.thing
is.ses <- function(x){
  inherits(x, 'ses')
}

#' is.SES
#' 
#' Test if an object belongs to the family of SES package object.
#' 
#' @param x The object
#' @export
#' @family is.thing
is.SES <- function(x){
  inherits(x, c('ses', 'tdr', 'stat', 'stat3D', 'tdr3D'))
}

#' is.tdr
#' 
#' Test if an object belong to the 'tdr' class
#' 
#' @param x The object
#' @export
#' @family is.thing
is.ses <- function(x){
  inherits(x, 'tdr')
}

#' is.tdr3D
#' 
#' Test if an object belong to the 'tdr3D' class
#' 
#' @param x The object
#' @export
#' @family is.thing
is.ses <- function(x){
  inherits(x, 'tdr3D')
}

#' is.stat
#' 
#' Test if an object belong to the 'stat' class
#' 
#' @param x The object
#' @export
#' @family is.thing
is.ses <- function(x){
  inherits(x, 'stat')
}

#' is.stat3D
#' 
#' Test if an object belong to the 'stat3D' class
#' 
#' @param x The object
#' @export
#' @family is.thing
is.ses <- function(x){
  inherits(x, 'stat3D')
}