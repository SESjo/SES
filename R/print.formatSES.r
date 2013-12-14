#' print.fmtSES
#' @description S3 method for formatSES object of SES package
#' @param x The object to print
#' @S3method print fmtSES
print.fmtSES <- function(x){
	obj <- whichformatSES(x)
	message("Read/Edit importation preferences")
	tmp <- edit(x)
	class(tmp) <-  class(x)
	if (!identical(tmp, x)){
		ans <- as.logical(pmatch(readline(prompt="Type 'yes' to save modifications > "), "yes", nomatch=FALSE))
		if (!ans) {
			message("Changes discarded.")
		}else{
			assign("formatSES", within(formatSES, assign(obj, tmp)), envir=.fmtSES)
		}
	}
	if (any(grepl(".fmtSES", search()))) detach(.fmtSES)
	attach(.fmtSES, warn.conflict=FALSE)
}

#' save.fmtSES
#' @description S3 method for formatSES object of SES package. Export formatSES setting to package home so that modifications are loaded at every startup.
#' @param x The object to save
#' @S3method save fmtSES
save.fmtSES <- function(x, verbose=FALSE) {
	if (inherits(x, "data.frame")){
		obj <- whichformatSES(x)
		if (!identical(obj, as.character())){
			y <- list()
			y[[obj]] <- x
			x <- y
		}
	}
	for (elt in x){
		inherits(elt, "fmtSES") || stop("Not a 'fmtSES' object.")
		obj <- whichformatSES(elt)
		filename <- paste0("formatSES.", obj, ".csv")
		path <- system.file("extdata", package="SES")
		!verbose || print(file.path(path, filename))
		write.table(elt, file.path(path, filename), sep=";", row.names=TRUE)
	}
}

#' whichformatSES
#' @description Return the name of formatSES element matching x.
#' @param x
#' @keywords internal
whichformatSES <- function(x){
	obj <- sapply(get("formatSES", envir=.fmtSES), equals, x)
	obj <- names(obj)[obj]
}

#' resetFormatSES
#' @description Reset formatSES to default (suggested alias).
#' @export
resetFormatSES <- function(...){
	for (elt in formatSES){
		obj <- whichformatSES(elt)
		tmp <- elt
		tmp$userAlias <- tmp$suggestedAlias
		assign("NewformatSES", within(formatSES, assign(obj, tmp)), envir=as.environment("package:SES"))
	}
	assign("formatSES", NewformatSES, envir=as.environment("package:SES"))
	for (elt in formatSES){
		save.fmtSES(elt, ...)
	}
}