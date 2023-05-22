#' Simple horizontal barplot
#'
#' @param data a \code{data.frame} object
#' @param item a survey item
#' @param weights optional argument to weight output by survey weights
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
#' @import pollster
#'
plot_bar_h <- function(data, item, weights, 
                       lang = "DE", barcolor = "#1F407A", barwidth = 0.8, 
                       texthjust = 2, textsize = 8, min_textsize = 5){
  
  if(missing(weights)){
    data$weight <- 1
  } else {
    data <- data %>% 
      mutate(weight = {{weights}})
  }
  
  data %>%
    filter({{item}} > -1) %>% 
    topline(variable = {{item}}, weight = weight) %>%
    ggplot(aes(x = Response, y = `Valid Percent`, 
               label = paste0(round(`Valid Percent`, 1), "%"))) +
    geom_col(fill = barcolor, width = barwidth) +
    geom_bar_text(size = textsize,
                  min.size = min_textsize,
                  family = "Roboto",
                  padding.x = grid::unit(texthjust, "mm"),
                  outside = TRUE) +
    labs(caption = n_par(data = data, item = ensym(item), lang = lang)) +
    coord_flip(clip = "off") +
    theme_sep() +
    theme(axis.text.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(linetype = "dashed"),
          plot.caption = element_text(color = "grey"))

}
