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
