# My package tools
is.error <- function(x) inherits(x, "try-error")
where <- function(x, f) vapply(x, f, logical(1))
"%wo%" <- function(x, y) x[!x %in% y] # x without y, asymetric
"%wi%" <- function(x, y) x[x %in% y] # x within y, equivalent to intersect, symetric

#' datenum2posx
#' @description Utility to convert Matlab numeric detes to R's POSIXct format
#' @param x The atomic vector of matlab dates.
#' @author Yves
#' @export
datenum2posx <- function(x){as.POSIXct((x - 719529)*24*3600, tz="UTC", origin="1970-01-01")}


#' SESname
#' @description Extract the ID of an individual from a string that contains it.
#' @param text Text containing the seal ID.
#' @return TheiID as character
#' @author Yves
#' @export
#' @examples 
#' SESname("Path/to/seal/file/2011-12_some_ses_file.txt")
SESname <- function(text){
  regmatches(text, regexpr('20+[0-9]{2}-[0-9]{2}', text))
}


#' replaceMissing
#' @description Replace values in an atomic vector.
#' @param x The atomic vector
#' @param na.0 The value to be replaced. Default is NaN.
#' @param na.1 The replacement. Default is NA.
#' @author Yves
#' @export
#' @keywords internal
#' @examples
#' x <- sample(c(1:3,NaN), 20, replace=TRUE)
#' x 
#' replaceMissing(x)
replaceMissing <- function(x, na.0=NaN, na.1=NA) {
  if (is.nan(na.0)) x[is.nan(x)] <- na.1
  else if (is.na(na.0)) x[is.na(x)] <- na.1
  else x[is.na(x)] <- na.1
  x
}


#' findVars
#' @description Assign to given variable names the value of matching columns in an object. If no matching throw an error. If partial matching print a warning.
#' @param vars The list of variable to search and create in the current environment.
#' @param obj The object in which to search and grab the values.
#' @param varnames Optional. The names to give to the variables found if different from \code{vars}.
#' @param ... Arguments to be passed to \code{existsVars}.
#' @seealso \code{\link{existsVars}}
#' @author Yves
#' @export
#' @keywords internal
#' @examples
#' x <- data.frame(A=1:3, B=letters[1:3], c=c(TRUE, FALSE, TRUE))
#' fun <- function(x) {
#' 	vars <- c("A", "b", "C")
#' 	findVars(vars, x)
#' 	for (k in vars) print(get(k))
#' }
#' fun(x) # Check the warnings
findVars <- function(vars, obj, varnames=NULL, ...){
  idx <- existsVars(vars, obj, ...)
  if (is.null(varnames)){varnames <- vars}
  for (k in seq_along(idx)){
    if (is.data.frame(obj)) {assign(varnames[k], obj[ , idx[k]], envir=sys.frame(-1))}
    else if (is.list(obj)) {assign(varnames[k], obj[[idx[k]]], envir=sys.frame(-1))}
  }
}

#' existVar
#' @description Search for variables in an object. Throw an error if not when a variable is not found. Print a warning if partial matching only.
#' @param vars The list of variable names. Can be a regular expression.
#' @param obj The object in which to search.
#' @param idx Should the indexes of matching columns be returned ?
#' @param substring If \code{FALSE} then check that \code{pattern} and matching variables have the same number of characters.
#' @param ignore.case Should case variants of the pattern be investigated ?
#' @param ... Other arguments to be passed to \code{grepl}. Set \code{fixed} to \code{TRUE} if  \code{vars} is to be matched as is (not a regular expression).
#' @seealso \code{\link{findVars}}
#' @author Yves
#' @keywords internal
#' @export
existsVars <- function(vars, obj, idx=TRUE, substring=TRUE, ignore.case=TRUE, ...) {
  col.idx <- rep(NA, length(vars))
  col <- sapply(vars, grepl, x=names(obj), ...)
  if (!substring){
    for (j in 1:ncol(col)){
      n <- rep(nchar(vars[j]), nrow(col))
      n[which(col[ , j])] <- nchar(names(obj)[which(col[ , j])])
      col[n != nchar(vars[j]), j] <- FALSE
    }
  }
  obj.name <- as.expression(substitute(obj))
  for (j in 1:ncol(col)){
    if (!any(col[, j]) & ignore.case){
      jlower <- grepl(tolower(vars[j]), tolower(names(obj)))
      if (any(jlower)) {
        col.idx[j] <- ifelse(sum(jlower) == 1, which(jlower), NA)
        warning(paste(vars[j], "assumed to be",
                      names(obj)[col.idx[j]], "in", obj.name))
      } else {
        jupper <- grepl(toupper(vars[j]), toupper(names(obj)))
        if (any(jupper)) {
          col.idx[j] <- ifelse(sum(jupper) == 1, which(jupper), NA)
          warning(paste(vars[j], "assumed to be",
                        names(obj)[col.idx[j]], "in", obj.name))
        } else {
          col.idx[j] <- NA
        }
      }
    } else {
      if (length(which(col[, j])) > 1) {
        warning(paste0(paste("Several variables of", obj.name, "matched with", vars[j], ": "),
                       paste(names(obj)[which(col[ , j])], collapse=", "),
                       "Try with 'substring=FALSE'."))
        col.idx[j] <- NA
      }else if (length(which(col[, j])) == 0){
        col.idx[j] <- NA
      }else{
        col.idx[j] <- which(col[ , j])
      }
    }
  }
  if (any(is.na(col.idx))){
    stop(paste(vars[is.na(col.idx)], "not found in", obj.name))
  }
  idx && return(col.idx)
}
