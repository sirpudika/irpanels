#' Number of participants for survey item
#'
#' @param data a \code{data.frame} object
#' @param item relevant survey item (i.e. w5_q17)
#'
#' @return number of participants ("Don't knows"-answers included) for item
#'
#' @import dplyr
#'
#' @export
#'
n_par <- function(data, item){

  n <- data %>%
    select({{item}}) %>%
    filter({{item}} >= -9) %>%
    count() %>%
    pull()

  participants <- paste("\nGrafik basiert auf N =", n)

}
