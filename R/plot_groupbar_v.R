#' Grouped vertical barplot
#'
#' @param data a \code{data.frame} object
#' @param item a survey item
#' @param by grouping variable
#' @param lang optional argument for language (German = "DE" (default), English = "EN")
#' @param barpadding optional argument to adjust padding between bars
#' @param barwidth optional argument to define the bar's width
#' @param legendtitle optional argument to define a legend title
#' @param textsize optional argument to adjust the text's size
#' @param min_textsize optional argument to set the minimum text size
#' @param ... further arguments of \code{scale_fill_manual} like \code{labels}
#'
#' @return a grouped vertical barplot
#' @export
#'
#' @import dplyr
#' @import ggplot2
#' @import ggfittext
#'
plot_groupbar_v <- function(data, item, by, lang = "DE", barpadding = 0.1, barwidth = 0.5, legendtitle = "", textsize = 8, min_textsize = 5, ...){
  data %>%
    filter({{item}} > -8,
           {{by}} > -8) %>%
    group_by({{item}}, {{by}}) %>%
    count() %>%
    group_by({{item}}) %>%
    mutate(freq = n/sum(n)) %>%
    ggplot(aes(x = as.factor({{item}}), y = .data$freq, fill = as.factor({{by}}), label = helper_percentage(.data$freq, 1))) +
    geom_col(position = position_dodge2(padding = barpadding)) +
    geom_bar_text(size = textsize,
                  min.size = min_textsize,
                  family = "Roboto",
                  position = "dodge",
                  fullheight = TRUE,
                  color = "white",
                  contrast = TRUE) +
    scale_y_continuous(labels = scales::label_percent(accuracy = 1),
                       expand = expansion(add = 0.8)) +
    scale_fill_manual(...) +
    labs(title = "",
         subtitle = "",
         fill = legendtitle,
         caption = n_par(data = data, item = ensym(item), by = {{by}}, lang = lang)) +
    theme_sep() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(linetype = "dashed"),
          legend.position = "bottom",
          plot.caption = element_text(color = "grey"),
          axis.text.y = element_blank())

}
