#' Simple horizontal barplot
#'
#' @param data a \code{data.frame} object
#' @param item a survey item
#' @param lang optional argument for language (German = "DE" (default), English = "EN")
#' @param barcolor optional argument to set the color of the bar
#' @param barwidth optional argument to define the width of the bar
#' @param texthjust optional argument to adjust the text's horizontal alignment
#' @param textsize optional argument to adjust the text's size
#' @param min_textsize optional argument to set the minimum text size
#'
#'
#' @return a horizontal barplot
#' @export
#'
#' @import ggplot2
#' @importFrom rlang .data
#' @import forcats
#' @import ggfittext
#'
plot_bar_h <- function(data, item, lang = "DE", barcolor = "#1F407A", barwidth = 0.8, texthjust = 2, textsize = 8, min_textsize = 5){
  data %>%
    filter({{item}} > -8) %>%
    group_by({{item}}) %>%
    count() %>%
    ungroup() %>%
    mutate(freq = n/sum(n)) %>%
    ggplot(aes(x = fct_rev(as.factor({{item}})), y = .data$freq, label = helper_percentage(.data$freq, 1))) +
    geom_col(fill = barcolor, width = barwidth) +
    geom_bar_text(size = textsize,
                  min.size = min_textsize,
                  family = "Roboto",
                  padding.x = grid::unit(texthjust, "mm"),
                  outside = TRUE) +
    labs(title = "",
         subtitle = "",
         caption = n_par(data, dQuote({{item}}), lang = lang)) +
    coord_flip(clip = "off") +
    theme_sep() +
    theme(axis.text.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(linetype = "dashed"),
          plot.caption = element_text(color = "grey"))

}
