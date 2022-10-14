#' Number of participants for survey item
#' 
#' @description 
#' Number of participants for survey item
#' To be used with @backref plot_bar_h, @backref plot_bar_v, @backref plot_groupbar_h and @backref plot_groupbar_v.
#' For @backref plot_multiple_h and @backref plot_rank_v, use the function @backref n_par_by.
#'
#' @param data a \code{data.frame} object
#' @param item relevant survey item (i.e. w5_q17)
#' @param lang language (German = "DE" (default), English = "EN", anything else goes neutral)
#'
#' @return Number of participants ("Don't knows"-answers included) for item
#'
#' @import dplyr
#'
#' @export
#'

n_par <- function(data, item, lang = "DE"){

  n <- data %>%
    select({{item}}) %>%
    filter({{item}} >= -9) %>%
    count() %>%
    pull()

  n_text <- case_when(lang == "DE" ~ "Grafik basiert auf N = ",
                      lang == "EN" ~ "Plot is based on N = ",
                      TRUE ~ "N = ")

  participants <- paste0("\n", n_text, n)
}
