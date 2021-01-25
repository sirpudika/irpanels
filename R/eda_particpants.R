#' Helper function to quickly calculated the number of participants for the corresponding survey item
#'
#' @param data a data.frame object
#' @param item survey item
#' @param rm.dk a logical value indicating whether "Don't know" values (-8) should be stripped before the computation proceeds
#'
#' @return number of particpants
#' @export
#'
#' @examples
#' \dontrun{
#' eda_participants(data = w5, item = w5_q11)
#' }
#' @import dplyr

eda_participants <- function(data = currentwave, item, rm.dk = TRUE){

  if (isTRUE(rm.dk)) {
    data %>%
      filter({{item}} >= 0) %>%
      nrow()
  } else {
    data %>%
      filter({{item}} >= -8) %>%
      nrow()
  }

}
