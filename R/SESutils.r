# My package tools
is.error <- function(x) inherits(x, "try-error")
where <- function(x, f) vapply(x, f, logical(1))
"%wo%" <- function(x, y) x[!x %in% y] # x without y, asymetric
"%wi%" <- function(x, y) x[x %in% y] # x within y, equivalent to intersect, symetric
text2posx <- function(text){
  # Extract 8 consecutive numeric characters begining with 20 (years 20**)
  dates.str <- regmatches(text, regexpr('(20+[0-9]{6})', text))
  if (length(dates.str) == length(text)){
    return(as.POSIXct(dates.str, format="%Y%m%d"))
  }else{
    stop('Several expression matched.')
  }
}