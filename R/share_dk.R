#' Calculate share of "Don't know"-answers for survey item
#'
#' @param data a \code{data.frame} object
#' @param item relevant survey item (i.e. w5_q17)
#'
#' @return relative frequency of "Don't know"-answers, if applicable to item
#' @export
#'
#' @import dplyr
#'
#'
share_dk <- function(data, item){

  share_num <- data %>%
    filter({{item}} >= -9) %>%
    select({{item}}) %>%
    mutate(dk = ifelse({{item}} %in% c(-8, -9), 1, 0)) %>%
    group_by(dk) %>%
    count() %>%
    ungroup() %>%
    mutate(total = sum(n),
           share = round((n/total)*100, digits = 1)) %>%
    filter(dk == 1) %>%
    pull(share)

  text <- "\nAnteil von «Weiss nicht»: "


  output <- paste0(text, share_num, "%")
}
