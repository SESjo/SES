#' findVars
#' @description Search and get variables in an object using its name. To use for error handling and initial checking in functions.
#' @param vars The list of variables to search and to create in the current environment.
#' @param object The object supposed to contain the variables.
#' @param varnames An Optional atomic vector of type 'character'. The names to give to the variables found when assigning them to the current environment. Default is the same as \code{vars}.
#' @param recursive Should the object (if it is a list) be investigated on its first depth level (\code{FALSE}) or until its maximun depth instead (\code{TRUE}).
#' @param ... Other arguments to be passed to \code{grepl}. Set \code{fixed} to \code{TRUE} if  \code{vars} is to be matched as is (not a regular expression).
#' @seealso \code{\link{existsVars}}, \code{\link{findVarlist}}
#' @author Yves
#' @export
#' @keywords internal
#' @examples
#' x <- data.frame(A=1:3, b=c(TRUE, FALSE, TRUE), C=letters[1:3], D=LETTERS[1:3])
#' l <- list(A=1:3, b=list(C=c(TRUE, FALSE, TRUE), D=letters[1:3]))
#' fun <- function(x) {
#' vars <- c("A", "B", "c", "D")
#' findVars(vars, x)
#' mget(vars)
#' }
#' str(fun(x))
#' str(fun(l))
#' \dontrun{
#' # Check the output / warnings / errors
#' x <- data.frame(A=1:3, B=c(TRUE, FALSE, TRUE), c=letters[1:3], C=LETTERS[1:3])
#' l <- list(A=1:3, B=list(cD=c(TRUE, FALSE, TRUE), D=letters[1:3]))
#' str(fun(x))
#' str(fun(l))
#' }
findVars <- function(vars, object, varnames=NULL, recursive=TRUE, ...){
  if (is.data.frame(object)) idx <- existsVars(vars, object, ...)
  if (is.null(varnames)){varnames <- vars}
  for (k in seq_along(vars)){
    if (is.data.frame(object)) {assign(varnames[k], object[ , idx[k]], envir=sys.frame(-1))}
    else if (is.list(object) && !recursive) {assign(varnames[k], object[[idx[k]]], envir=sys.frame(-1))}
    else if (is.list(object) && recursive) {
      assign(varnames[k], findVarlist(vars[k], object, ...), envir=sys.frame(-1))
    }
  }
}

#' existVar
#' @description Search for variables in an object. Throw an error if not when a variable is not found. Print a warning if partial matching only.
#' @param vars The list of variable names. Can be a regular expression.
#' @param obj The object in which to search.
#' @param idx Should the indexes of matching columns be returned ?
#' @param substring If \code{FALSE} then check that the \code{pattern} and the matching variables have the same number of characters.
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
      if (!substring) col[n != nchar(vars[j]), j] <- FALSE
      if (any(jlower)) {
        col.idx[j] <- ifelse(sum(jlower) == 1, which(jlower), NA)
        if (sum(jlower) == 1){
          warning(paste(vars[j], "assumed to be",
                        names(obj)[col.idx[j]], "in", obj.name))
        }else if (sum(jlower) > 1){
          warning(paste0(paste("Several variables of", obj.name, "matched with", vars[j], ": "),
                         paste(names(obj)[which(jlower)], collapse=", "),
                         ". Try with 'substring=FALSE'."))
        }
      } else {
        jupper <- grepl(toupper(vars[j]), toupper(names(obj)))
        if (!substring) col[n != nchar(vars[j]), j] <- FALSE
        if (any(jupper)) {
          col.idx[j] <- ifelse(sum(jupper) == 1, which(jupper), NA)
          if (sum(jupper) == 1){
            warning(paste(vars[j], "assumed to be",
                          names(obj)[col.idx[j]], "in", obj.name))
          }else if (sum(jupper) > 1){
            warning(paste0(paste("Several variables of", obj.name, "matched with", vars[j], ": "),
                           paste(names(obj)[which(jupper)], collapse=", "),
                           ". Try with 'substring=FALSE'."))
          }
        } else {
          col.idx[j] <- NA
        }
      }
    } else {
      if (length(which(col[, j])) > 1) {
        warning(paste0(paste("Several variables of", obj.name, "matched with", vars[j], ": "),
                       paste(names(obj)[which(col[ , j])], collapse=", "),
                       ". Try with 'substring=FALSE'."))
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
#' lnamesApply
#' @description Similar to rapply but only aims only the names of variables within \code{object}.
#' @param object A list.
#' @param fun a function (must return a logical).
#' @param ... Optional arguments to be passed to \code{fun}.
#' @return A list with the desired variable (\code{$var}) and the results of the function \code{fun} at the matching depth. Returns \code{NULL} if no match.
#' @seealso \code{\link{findVarlist}}
#' @export
#' @keywords internal
#' @author Yves
#' @examples
#' l <- list(a=1, b=list(c=2))
#' lnamesApply(l, grepl, pattern="a")
#' lnamesApply(l, grepl, pattern="b")
#' lnamesApply(l, grepl, pattern="c")
#' is.null(lnamesApply(l, grepl, pattern="C")) # TRUE
#' lnamesApply(l, grepl, pattern="C", ignore.case=TRUE)
lnamesApply <- function(object, fun, ...){
  if (typeof(object) != "list") stop("'object' must be a list")
  fun2vars <- do.call(fun, list(names(object), ...))
  names(fun2vars) <- names(object)
  
  # If match return result
  if (sum(fun2vars) == 1){
    return(list(var=object[[which(fun2vars)]], match=fun2vars))
  }else if (sum(fun2vars) > 1){
    return(list(var=object[which(fun2vars)], match=fun2vars))
  }
  
  # Otherwise continue further in the object tree
  for (idx in seq_along(object)){
    vars <- names(object[[idx]])
    if (is.null(vars)) next
    else ans <- lnamesApply(object[[idx]], fun, ...)
    if (!is.null(ans)) names(ans$match) <- vars
    if (any(ans$match)) return(ans)
  }
}

#' findVarlist
#' @description Search and get the content of a variable anywhere in a 'list' object using its name.
#' @param var Character givin the name of the variable to search.
#' @param object The object in which to search.
#' @param substring If \code{FALSE} then check that the \code{pattern} and the matching variables have the same number of characters.
#' @param ignore.case Should case variants of the pattern be investigated ?
#' @return the desired variable
#' @details Same warnings and error handling than \code{existsVars()}
#' @seealso \code{\link{findVars}}, \code{\link{existsVars}}, \code{\link{lnamesApply}}.
#' @export
#' @keywords internal
#' @author Yves
#' @examples
#' l <- list(a=1, b=list(c=2, cD=3))
#' findVarlist("a", l)
#' findVarlist("b", l)
#' findVarlist("c", l) # Error
#' findVarlist("c", l, substring=F)
#' findVarlist("d", l)
#' findVarlist("d", l, ignore.case=F) # Error
findVarlist <- function(var, object, substring=TRUE, ignore.case=TRUE, ...){
  ans <- lnamesApply(object, grepl, pattern=var, ignore.case, ...)
  match.names <- if (!is.null(ans$match)){names(ans$match)[ans$match]}else{NULL}
  
  # Check for ambiguities with deeper elements
  if (!is.null(match.names)){
    nameList <- unique(gsub("[0-9]$", "", names(c(object, recursive=TRUE))))
    if (any(grepl(paste0(var,'.+',var), nameList, ignore.case=TRUE))){
      stop(paste("Ambiguous matching with", var, "in", substitute(object),
                 ": several depth levels matching. Try 'ignore.case=FALSE'."))
    }
  }
  
  # Update resultsif substring=FALSE using number of characters
  if (!substring & !is.null(match.names)){
    if (length(match.names) > 1) mult.match <- match.names
    for (match in match.names){
      if (nchar(match) != nchar(var)) ans$match[match] <- FALSE
    }
    match.names <- if(any(ans$match)){names(ans$match)[ans$match]}else{NULL}
  }
  
  # Check the number of matching variables
  if (is.null(match.names)){
    stop(paste(var, "not found in", substitute(object)))
  }else if (length(match.names) > 1){
    stop(paste0(paste("Several variables of",  substitute(object), "matched with", var, ": "),
                paste(match.names, collapse=", "),
                ". Try with 'substring=FALSE'."))
  }
  if (exists("mult.match")) ans$var <- ans$var[[match.names]]
  
  # Print to the user the assumption maden if variable names are not exactly the same.
  if (match.names != var){
    warning(paste(var, "assumed to be", match.names, "in", substitute(object)))
  }
  
  return(ans$var)
}