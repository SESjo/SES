#' print.fmtSES
#' @description S3 method for formatSES object of SES package. Allow to view and edit \code{importSES()} settings.
#' @param x The object to print
#' @method print fmtSES
#' @export
#' @seealso \code{\link{save.fmtSES}}, \code{\link{reset.fmtSES}}
#' @author Yves
#' @examples
#' \dontrun{
#' # First attach the Settings environement
#' attach(efmtSES)
#' # Then type the name of the object to view or edit
#' formatSES$tdr
#' # Process to modif ...
#' # Save to package files for automatic load in the futur
#' save.fmtSES(formatSES$tdr)
#' # Reset default settings
#' reset.fmtSES()
#' }
print.fmtSES <- function(x){
	obj <- whichformatSES(x)
	message("Read/Edit importation preferences")
	tmp <- edit(x)
	class(tmp) <-  class(x)
	if (!identical(tmp, x)){
		ans <- as.logical(pmatch(readline(prompt="Save changes [y/N] ? "), "yes", nomatch=FALSE))
		if (!ans) {
			message("Changes discarded.")
		}else{
			assign("formatSES", within(formatSES, assign(obj, tmp)), envir=efmtSES)
		}
	}
	if (any(grepl("efmtSES", search()))) detach(efmtSES)
	attach(efmtSES, warn.conflict=FALSE)
}

#' save.fmtSES
#' @description A kind of \code{save} S3 method designed for formatSES object of the SES package. Export formatSES settings to package files so that user preferences are automaticaly loaded at startup.
#' @param x The object to save
#' @param verbose To print the filepath.
#' @param envir The formatSES environment.
#' @seealso \code{\link{reset.fmtSES}}, \code{\link{print.fmtSES}}
#' @author Yves
#' @export
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
		if(verbose) {print(file.path(path, filename))}
		write.table(elt, file.path(path, filename), sep=";", row.names=TRUE)
	}
}

#' reset.fmtSES
#' @description Reset formatSES to defaults settings (suggested alias column).
#' @param type \code{files} restore original settings in the package folder. \code{vars} restore the settings in this session but leave the package files unchanged.
#' @param ... arguments to be passed to \code{save.fmtSES}.
#' @seealso \code{\link{save.fmtSES}}, \code{\link{print.fmtSES}}
#' @author Yves
#' @export
reset.fmtSES <- function(type=c("files", "vars"), ...){
	with(efmtSES,
		 for (elt in formatSES){
		 	obj <- whichformatSES(elt)
		 	tmp <- elt
		 	tmp$userAlias <- tmp$suggestedAlias
		 	assign("formatSES", within(formatSES, assign(obj, tmp)), envir=efmtSES)
		 }
	)
	if (any(grepl("efmtSES", search()))) detach(efmtSES)
	attach(efmtSES, warn.conflict=FALSE)
	for (elt in get("formatSES", envir=efmtSES)){
		save.fmtSES(elt, ...)
	}
}

#' whichformatSES
#' @description Return the name of formatSES element matching x.
#' @param x
#' @export
#' @keywords internal
whichformatSES <- function(x){
	obj <- sapply(get("formatSES", envir=efmtSES), equals, x)
	obj <- names(obj)[obj]
}