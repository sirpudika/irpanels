#' Grouped horizontal barplot
#'
#' @param data a \code{data.frame} object
#' @param item a survey item
#' @param by grouping variable
#' @param barpadding optional argument to adjust padding between bars
#' @param legendtitle optional argument to define a legend title
#' @param ... further arguments of \code{scale_fill_discrete} like \code{labels}
#'
#' @return a grouped horizontal barplot
#' @export
#'
#' @import dplyr
#' @import ggplot2
#' @import forcats
#' @import ggfittext

#'
plot_groupbar_h <- function(data, item, by, barpadding = 0.1, legendtitle = "", ...){

  data %>%
    filter({{item}} > -8,
           {{by}} > -8) %>%
    group_by({{item}}, {{by}}) %>%
    count() %>%
    group_by({{item}}) %>%
    mutate(freq = n/sum(n)) %>%
    ggplot(aes(x = fct_rev(as.factor({{item}})), y = .data$freq, fill = as.factor({{by}}), label = helper_percentage(.data$freq, 1))) +
    geom_col(position = position_dodge2(padding = barpadding)) +
    geom_bar_text(family = "Roboto", min.size = 3, position = "dodge", fullheight = TRUE, color = "white", contrast = TRUE) +
    scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
    scale_fill_manual(...) +
    coord_flip() +
    labs(title = "",
         subtitle = "",
         fill = legendtitle,
         caption = n_par(data, {{item}})) +
    theme_sep() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(linetype = "dashed"),
          legend.position = "bottom",
          plot.caption = element_text(color = "grey"),
          axis.text.x = element_blank())
}
