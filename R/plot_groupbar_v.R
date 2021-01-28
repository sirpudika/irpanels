#' Grouped vertical barplot
#'
#' @param data a \code{data.frame} object
#' @param item a survey item
#' @param dk_share share of "Don't knows". Check \code{eda_freqtable} for share.
#' @param grouper grouping variable
#' @param barpadding optional argument to adjust padding between bars
#' @param barwidth optional argument to define the bar's width
#' @param legendtitle optional argument to define a legend title
#' @param ... further arguments of \code{scale_fill_discrete} like \code{labels}
#'
#' @return a grouped vertical barplot
#' @export
#'
#' @import dplyr
#' @import ggplot2
#'
plot_groupbar_h <- function(data = currentwave, item, dk_share = "?", grouper, barpadding = 0.1, barwidth = 0.5, legendtitle = "", ...){
  data %>%
    filter({{item}} > -8,
           {{grouper}} > -8) %>%
    group_by({{item}}, {{grouper}}) %>%
    count() %>%
    group_by({{item}}) %>%
    mutate(freq = n/sum(n)) %>%
    ggplot(aes(x = as.factor({{item}}), y = .data$freq, fill = as.factor({{grouper}}))) +
    geom_col(position = position_dodge2(padding = barpadding), width = barwidth) +
    scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
    scale_fill_discrete(...) +
    labs(title = "",
         subtitle = "",
         fill = legendtitle,
         caption = paste0('Graphik exklusive "Weiss nicht" Antworten: Anteil ', dk_share, '%')) +
    theme_sep() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(linetype = "dashed"),
          legend.position = "bottom",
          plot.caption = element_text(face = "italic"))

}
