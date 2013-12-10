#' print.fmtSES
#' @description S3 method for formatSES object of SES package
#' @param x The object to print
#' @S3method print fmtSES
print.fmtSES <- function(x){
  obj <- names(formatSES)[sapply(SES::formatSES, identical, y=x)]
  message("Read/Edit importation preferences")
  tmp <- edit(x)
  class(tmp) <-  class(x)
  if (!identical(tmp, x)){
    ans <- readline(prompt="Type y to save modifications > ")
    if (ans == "y")assign(obj, tmp, envir=as.environment("package:SES"))
    else message("Changes discarded.")
  }
}