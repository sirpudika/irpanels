#' Helper function to easily calculate/display relative frequencies
#'
#' @param item a survey item
#' @param digits number of digits to be displayed
#'
#' @return relative frequencies of a survey item
#' @export
#'
#'
helper_percentage <- function(item, digits=1) {
  sprintf(paste0("%1.", digits, "f%%"), item*100)
}
