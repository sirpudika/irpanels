#' Create simple crosstables of n variables
#'
#' @param vars vector of variables
#' @param df dataframe
#'
#' @return a crosstable
#' @export

#' @examples
#' crosstable(vars = c("gear", "carb"), df = mtcars)
crosstable<- function(vars, df){
  dframe <- df %>%
    dplyr::select(!!vars) %>%
    dplyr::filter_all(dplyr::all_vars(. > 0))

  table <- table(dframe)
  prop_table <- round(prop.table(table), digits = 2)

  prop_table
}
