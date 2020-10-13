#' Create simple crosstables of n variables
#'
#' Unfortunately, for the time being this function only works with two numeric variables with values above 0. Updates will follow.
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
