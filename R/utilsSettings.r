#' renames
#' 
#' Extract and rename tdr and statdives variables according to formatSES instructions.
#' 
#' @param type The type of dataset
#' @param obj The object to process.
#' @param objtxt The original names. tdrcortxt or statdivestxt in the .mat file.
#' @family settings
#' @export
#' @keywords internal
renames <- function(type=c("tdr", "stat", "stat3D", "tdr3D"), obj, objtxt){
	FMT <- get("formatSES", envir=SESsettings)
	findVars(type, FMT, varnames="fmt", substring=FALSE)
	headers <- unlist(objtxt)
	newHeaders <- unname(fmt[headers, "userAlias"])
	if (ncol(obj) != length(newHeaders)){
		warning("The number of variables differs between 'statdives' and 'statdivestxt'. The nth first variable names are assumed to be the good ones.")
	}
	names(obj) <- newHeaders[1:ncol(obj)]
	if (any(match(fmt$userAlias[fmt$keep], names(obj), nomatch=0) == 0)){
		warning(paste0("The desired variable(s) ", 
					   paste(fmt$userAlias[fmt$keep][is.na(match(fmt$userAlias[fmt$keep], names(obj)))], collapse=" & "), 
					   " is(are) not available in statdives."))
	}
	# unique() needed to avoid duplicated columns with partial matching (e.g. lat and latitude)
	obj <- obj[ , unique(match(fmt$userAlias[fmt$keep], names(obj), nomatch=0))]
	obj
}

#' print.fmtSES
#' 
#' S3 method for formatSES object of SES package. Allow to view and edit \code{importSES()} settings.
#' 
#' @param x The object to print
#' @family settings
#' @method print fmtSES
#' @export
#' @examples
#' \dontrun{
#' # First attach the Settings environement
#' attach(SESsettings)
#' # Then type the name of the object to view or edit
#' formatSES$tdr
#' # Process to modif ...
#' # Save to package files for automatic load in the futur
#' save.fmtSES(formatSES$tdr)
#' # Reset default settings
#' reset.fmtSES()
#' }
print.fmtSES <- function(x){
	obj <- whichSESobj(x)
	message("Read/Edit importation preferences")
	tmp <- edit(x)
	class(tmp) <-  class(x)
	if (!identical(tmp, x)){
		ans <- as.logical(pmatch(readline(prompt="Save changes [y/N] ? "), "yes", nomatch=FALSE))
		if (!ans) {
			message("Changes discarded.")
		}else{
			assign("formatSES", within(formatSES, assign(obj, tmp)), envir=SESsettings)
		}
	}
	if (any(grepl("SESsettings", search()))) detach(SESsettings)
	attach(SESsettings, warn.conflict=FALSE)
}

#' save.fmtSES
#' 
#' A kind of \code{save} (S3) method designed for formatSES object of the SES package.
#' Export formatSES settings to package files so that user preferences are automaticaly
#' loaded at startup.
#' 
#' @param x The object to save
#' @param verbose To print the filepath.
#' @param envir The formatSES environment.
#' @family settings
#' @export
save.fmtSES <- function(x, verbose=FALSE) {
	if (inherits(x, "data.frame")){
		obj <- whichSESobj(x)
		if (!identical(obj, as.character())){
			y <- list()
			y[[obj]] <- x
			x <- y
		}
	}
	for (elt in x){
		inherits(elt, "fmtSES") || stop("Not a 'fmtSES' object.")
		obj <- whichSESobj(elt)
		filename <- paste0("formatSES.", obj, ".csv")
		path <- system.file("extdata", package="SES")
		if(verbose) {print(file.path(path, filename))}
		write.table(elt, file.path(path, filename), sep=";", row.names=TRUE)
	}
}

#' reset.fmtSES
#' 
#' Reset formatSES to defaults settings (suggested alias column).
#' 
#' @param type \code{files} restore original settings in the package folder. \code{vars} restore the settings in this session but leave the package files unchanged.
#' @param ... arguments to be passed to \code{save.fmtSES}.
#' @family settings
#' @export
reset.fmtSES <- function(type=c("files", "vars"), ...){
	with(SESsettings,
		 for (elt in formatSES){
		 	obj <- whichSESobj(elt)
		 	tmp <- elt
		 	tmp$userAlias <- tmp$suggestedAlias
		 	assign("formatSES", within(formatSES, assign(obj, tmp)), envir=SESsettings)
		 }
	)
	if (any(grepl("SESsettings", search()))) detach(SESsettings)
	attach(SESsettings, warn.conflict=FALSE)
	for (elt in get("formatSES", envir=SESsettings)){
		save.fmtSES(elt, ...)
	}
}

#' whichSESobj
#' 
#' Return the name of formatSES element matching x.
#' 
#' @param x
#' @family settings
#' @export
#' @keywords internal
whichSESobj <- function(x){
	obj <- sapply(get("formatSES", envir=SESsettings), equals, x)
	obj <- names(obj)[obj]
}

#' userHeader
#' 
#' Get the user headers associated with default headers of one of a package object.
#' 
#' @param header The default header.
#' @param type The type of the object.
#' @family settings
#' @export
#' @keywords internal
userHeader <- function(header, type){
	object <- get("formatSES", envir=SESsettings)
	object <- switch(type,
					 tdr = object[["tdr"]],
					 statdives = object[["stat"]],
					 stat = object[["stat"]],
					 tdr3D = object[["tdr3D"]],
					 stat3D = object[["stat3D"]])
	f <- function(h){
		ans <- unique(object$userAlias[object$suggestedAlias == h])
		if (identical(ans, character())) {
			stop(paste0("Default header '", h, "' not found in '", type, "'."))
		}
		ans
	}
	ans <- try(vapply(header, f, as.character(1)))
	if (is.error(ans)){
		stop("One of the default header is associated with several user headers. Please edit 'formatSES' to correct this.")
	}
	return(ans)
}
