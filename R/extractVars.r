#' findVars
#' @description Search and get variables in an object using its name. To use for error handling and initial checking in functions.
#' @param vars The list of variables to search and to create in the current environment.
#' @param object The object supposed to contain the variables.
#' @param varnames An Optional atomic vector of type 'character'. The names to give to the variables found when assigning them to the current environment. Default is the same as \code{vars}.
#' @param ... Other arguments to be passed to \code{checkVar}.
#' @seealso \code{\link{checkVar}}
#' @author Yves
#' @export
#' @keywords internal
#' @examples
#' x <- list(A=1, a=2, B=list(b=3, C=4, ca=5, abc=6, c=7))
#' # Example 1: Basic matching
#' findVars("C", x, ignore.case=FALSE) # Finds 'C'
#' findVars("c", x, mult=TRUE) # Finds 'C' 'ca' 'abc' 'c'
#' findVars("c", x, ignore.case=FALSE, substring=FALSE) # Finds 'c'
#' findVars("c", x, substring=FALSE, mult=TRUE, type="check") # check that 'C' 'c' exist without creating them
#' 
#' # Example 2: Regular expressions
#' findVars("c$", x, ignore.case=FALSE) # Finds 'ac' 'c'
#' \dontrun{
#' # Equivalents
#' findVars("c$", x, ignore.case=FALSE, mult=TRUE) # idem
#' findVars("c$", x, ignore.case=FALSE, varnames=c('C', 'AC')) # idem with custom names for assignment
#' }
#' 
#' # Example 3: Errors and warnings
#' \dontrun{
#' findVars("a", x) # Error
#' # Because you might want one of the following:
#' findVars("a", x, ignore.case=FALSE)
#' findVars("A", x, ignore.case=FALSE)
#' # Or both:
#' findVars(c("a", "A"), x, ignore.case=FALSE)
#' findVars("a", x, mult=TRUE) # Also possible but prints a warning.
#' 
#' # Notice the error when matching elements are not at same depth:
#' findVars(c("b", "B"), x, ignore.case=FALSE) # Works
#' findVars("b", x, mult=TRUE) # Error
#' }
findVars <- function(vars, object, varnames=NULL,
                     type=c("assign", "check"), mult=FALSE, ...){
  
  spChar <- c("^", "$", "[", "]", "\\.", "?", "*", "+", "\\")
  
  for (k in seq_along(vars)){
    
    # Check for a special character
    if (!mult & is.null(varnames)){
      for (char in spChar){
        mult <- ifelse(any(grepl(char, vars[k], fixed=TRUE)), TRUE, FALSE)
        if (mult) {
          message(paste0("A special character was found: ", char , " . Regular expression enabled. 'varnames' set to matched names."))
          break
        }
      }
    }
    if (!is.null(varnames) && length(varnames) > 1) {mult <- TRUE}
    
    # Perform extraction
    ans <- checkVar(vars[k], object, mult, ...)
    match.names <- names(ans$match)[ans$match]
    if (is.null(varnames) & mult) {varnames <- match.names}
    else if (is.null(varnames) & !mult) {varnames <- vars}
    for (kk in seq_along(match.names)){
      switch(match.arg(type),
             assign = assign(varnames[k + kk - 1], ans$var[[match.names[kk]]], envir=sys.frame(-1)),
             check = invisible())
    }
  }
}

#' checkVar
#' @description Search and get the content of a variable anywhere in a 'list' object using its name.
#' @param var Character givin the name of the variable to search.
#' @param object The object in which to search.
#' @param substring If \code{FALSE} then check that the \code{pattern} and the matching variables have the same number of characters.
#' @param ignore.case Should case variants of the pattern be investigated ?
#' @param mult Is \code{var} a regular expression pattern to match several elements in \code{object} variable names ?
#' @param ignore.depth.error Should the function check for ambiguities with the names of deeper element ?
#' @return the desired variable not simplified.
#' @seealso \code{\link{findVars}}, \code{\link{findVar}}.
#' @export
#' @keywords internal
#' @author Yves
#' @examples
#' l <- list(a=1, b=list(c=2, cD=3))
#' checkVar("a", l)
#' checkVar("b", l)
#' checkVar("c", l) # Error
#' checkVar("c", l, substring=F)
#' checkVar("d", l)
#' checkVar("d", l, ignore.case=F) # Error
checkVar <- function(var, object, mult=FALSE, substring=TRUE, ignore.case=TRUE, ignore.depth.error=FALSE){
  ans <- findVar(object, var, ignore.case)
  match.names <- if (!is.null(ans$match)){names(ans$match)[ans$match]}else{NULL}
  
  # Check for ambiguities with the names of deeper elements
  if (!is.null(match.names) & !ignore.depth.error){
    nameList <- unique(gsub("[0-9]$", "", names(c(object, recursive=TRUE))))
    if (any(grepl(paste0(var,'.*\\..*',var), nameList, ignore.case))){
      stop(paste0("Ambiguous matching with '", var, "' in ", substitute(object),
                  ": several depth levels matching. Try a more specific pattern or set 'ignore.case = FALSE' instead."))
    }
  }
  
  # Update results if substring = FALSE
  if (!substring & !is.null(match.names)){
    if (length(match.names) > 1) mult.match <- match.names
    for (match in match.names){
      if (nchar(match) != nchar(var)) ans$match[match] <- FALSE
    }
    match.names <- if(any(ans$match)){names(ans$match)[ans$match]}else{NULL}
  }
  
  # Check the number of matching variables
  if (is.null(match.names)){
    stop(paste0("'", var, "' not found in '", substitute(object), "'."))
  }else if (length(match.names) > 1 & !isTRUE(mult)){
    stop(paste0(paste0("Several variables of '",  substitute(object), "' matched with '", var, "': "),
                paste(match.names, collapse=", "),
                ".\n\t\tTry 'substring = FALSE' or 'ignore.case = FALSE' to be more specific. Set 'mult = TRUE' to extract all of them."))
  }
  
  # Print to the user the assumption maden if variable names are not exactly the same.
  for (match.var in match.names){
    if (match.var != var){
      warning(paste0("'", var, "' assumed to be '", match.var, "' in '", substitute(object), "'."))
    }
  }
  
  if (exists("mult.match") & !isTRUE(mult)) ans$var <- ans$var[match.names]
  
  return(ans)
}

#' findVar
#' @description Similar to rapply but only aims only the names of variables within \code{object}.
#' @param object A list.
#' @param pattern a function (must return a logical).
#' @param ... Optional arguments to be passed to \code{grepl}.
#' @return A list with the desired variable (\code{$var}) and the results of the function \code{fun} at the matching depth. Returns \code{NULL} if no match.
#' @seealso \code{\link{checkVar}}
#' @export
#' @keywords internal
#' @author Yves
#' @examples
#' l <- list(a=1, b=list(c=2))
#' findVar(l, grepl, pattern="a")
#' findVar(l, grepl, pattern="b")
#' findVar(l, grepl, pattern="c")
#' is.null(findVar(l, grepl, pattern="C")) # TRUE
#' findVar(l, grepl, pattern="C", ignore.case=TRUE)
findVar <- function(object, pattern, ...){
  if (typeof(object) != "list") stop("'object' must be a list")
  fun2vars <- grepl(pattern, names(object), ...)
  names(fun2vars) <- names(object)
  
  # If match return the result
  if (sum(fun2vars) != 0){return(list(var=object[which(fun2vars)], match=fun2vars))}
  
  # Otherwise continue further in the object tree
  for (idx in seq_along(object)){
    vars <- names(object[[idx]])
    if (is.null(vars)) next
    else ans <- findVar(object[[idx]], pattern, ...)
    if (!is.null(ans)) names(ans$match) <- vars
    if (any(ans$match)) return(ans)
  }
}