#' Calculates a frequency table for a single survey item
#'
#' @param data a \code{data.frame} object
#' @param item a survey item
#' @param rm.dk a logical value indicating whether "Don't know" values (-8) should be stripped before the computation proceeds
#'
#' @return a frequency table
#' @export
#'
#' @examples
#' \dontrun{
#' eda_freqtable(data = w5, item = w5_q11, rm.dk = FALSE)
#' }
#' @import dplyr
eda_freqtable <- function(data = currentwave, item, rm.dk = TRUE){
  if (!is.data.frame(data)) stop("Make sure the input is a data.frame")
  if (isTRUE(rm.dk))  {
    df <- filter(data, {{item}} >= 0)
  } else  {
    df <- filter(data, {{item}} >= -8)
  }

  vec <- pull(df, {{item}})
  round(prop.table(table(vec, dnn = "Relative frequencies:")), digits = 2)
}
