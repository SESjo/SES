# My package tools
is.error <- function(x) inherits(x, "try-error")
where <- function(x, f) vapply(x, f, logical(1))
"%wo%" <- function(x, y) x[!x %in% y] # x without y, asymetric
"%wi%" <- function(x, y) x[x %in% y] # x within y, equivalent to intersect, symetric
checkVar <- function(var, obj, idx=FALSE) {
  col.idx <- rep(NA, length(var))
  col <- sapply(var, grepl, x=names(obj))
  obj.name <- as.expression(substitute(obj))
  for (j in 1:ncol(col)){
    if (!any(col[, j])){
      jlower <- grepl(tolower(var[j]), tolower(names(obj)))
      if (any(jlower)) {
        col.idx[j] <- ifelse(sum(jlower) == 1, which(jlower), NA)
        warning(paste(var[j], "assumed to be",
                      names(obj)[col.idx[j]], "in", obj.name, "\n \t"))
      } else {
        jupper <- grepl(toupper(var[j]), toupper(names(obj)))
        if (any(jupper)) {
          col.idx[j] <- ifelse(sum(jupper) == 1, which(jupper), NA)
          warning(paste(var[j], "assumed to be",
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
    stop(paste(var[is.na(col.idx)], "not found in", obj.name, "\n \t"))
  }
  idx && return(col.idx)
}