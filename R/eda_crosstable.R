#' Calculates a frequency table for two survey items
#'
#' @param data a \code{data.frame} object
#' @param items survey items (in form of a vector)
#' @param rm.dk a logical value indicating whether "Don't know" values (-8) should be stripped before the computation proceeds
#' @param itemnames optional argument for specifying the survey items' names
#'
#' @return a table with relative frequencies (in percent)
#' @export
#'
#' @examples
#' \dontrun{
#' eda_crosstable(data = infert, items = c("education", "parity"), rm.dk = FALSE)
#' }
#' @import dplyr
eda_crosstable<- function(data, items, rm.dk = FALSE, itemnames = NULL){
  df <- select(data, !!items)
  if (isTRUE(rm.dk))  {
    df <- df %>%
      filter_if(is.numeric, all_vars(. >= 0))
  } else  {
    df <- df %>%
      filter_if(is.numeric, all_vars(. >= -8))
  }

  if (is.null(itemnames)) {
    table <- table(df)
  } else {
    table <- table(df, dnn = itemnames)
  }
  prop_table <- round(prop.table(table), digits = 4)
  prop_table*100
}
