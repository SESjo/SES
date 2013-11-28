# My package tools
is.error <- function(x) inherits(x, "try-error")
where <- function(x, f) vapply(x, f, logical(1))
"%wo%" <- function(x, y) x[!x %in% y] # x without y, asymetric
"%wi%" <- function(x, y) x[x %in% y] # x within y, equivalent to intersect, symetric

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


#' existVar
#' @description Assign to given variable names the value of matching columns in an object. If no matching throw an error. If partial matching print a warning.
#' @param vars The list of variable to search and create in the current environment.
#' @param obj The object in which to search and grab the values.
#' @seealso \code{\link{existsVars}}
#' @author Yves
#' @export
#' @examples
#' x <- data.frame(A=1:3, B=letters[1:3])
#' fun <- function(x){
#'    # Check that variables 'A' and 'b' exists in object 'x'.
#'    # Print a warning for 'b' instead of 'B'.
#'    findVars(c("A", "b"), x)
#'    # These variable are created in the current environment
#'    cat("A: ", A, "\nB: ", B)
#'    # 'C' does not exist in 'x'. Interupt execution.
#'    findVars("C", x)
#'  }
#'  fun(x)
findVars <- function(vars, obj){
  idx <- existsVars(vars, obj)
  for (k in seq_along(idx)){
    assign(vars[k], obj[ ,idx[k]], envir=sys.frame(-1))
  }
}

#' existVar
#' @description Search for variables in an object. Throw an error if not when a variable is not found. Print a warning if partial matching only.
#' @param vars The list of variable names.
#' @param obj The object in which to search.
#' @param idx Should the indexes of matching columns be returned ?
#' @seealso \code{\link{findVars}}
#' @author Yves
existsVars <- function(vars, obj, idx=TRUE) {
  col.idx <- rep(NA, length(vars))
  col <- sapply(vars, grepl, x=names(obj))
  obj.name <- as.expression(substitute(obj))
  for (j in 1:ncol(col)){
    if (!any(col[, j])){
      jlower <- grepl(tolower(vars[j]), tolower(names(obj)))
      if (any(jlower)) {
        col.idx[j] <- ifelse(sum(jlower) == 1, which(jlower), NA)
        warning(paste(vars[j], "assumed to be",
                      names(obj)[col.idx[j]], "in", obj.name, "\n \t"))
      } else {
        jupper <- grepl(toupper(vars[j]), toupper(names(obj)))
        if (any(jupper)) {
          col.idx[j] <- ifelse(sum(jupper) == 1, which(jupper), NA)
          warning(paste(vars[j], "assumed to be",
                        names(obj)[col.idx[j]], "in", obj.name, "\n \t"))
        } else {
          col.idx[j] <- NA
        }
      }
    } else {
      col.idx[j] <- which(col[ , j])
    }
  }
  if (any(is.na(col.idx))){
    stop(paste(vars[is.na(col.idx)], "not found in", obj.name, "\n \t"))
  }
  idx && return(col.idx)
}
