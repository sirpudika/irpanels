#' Simple vertical barplot
#'
#' @param data a \code{data.frame} object
#' @param item a survey item
#' @param weights optional argument to weight output by survey weights
#' @param lang optional argument for language (German = "DE" (default), English = "EN")
#' @param barcolor optional argument to set the color of the bar
#' @param barwidth optional argument to define the width of the bar
#' @param textvjust optional argument to adjust the text's horizontal alignment
#' @param textsize optional argument to adjust the text's size
#' @param min_textsize optional argument to set the minimum text size
#'
#' @return a vertical barplot
#' @export
#'
#' @import ggplot2
#' @import ggfittext
#' @import pollster
#'
#'
plot_bar_v <- function(data, item, weights,
                       lang = "DE", barcolor = "#1F407A", barwidth = 0.8, 
                       textvjust = 2, textsize = 8, min_textsize = 5){
  if(missing(weights)){
    data$weight <- 1
  } else {
    data <- data %>% 
      mutate(weight = {{weights}})
  }
  
  data %>%
    filter({{item}} > -1) %>% 
    topline(variable = {{item}}, weight = weight) %>%
    ggplot(aes(x = .data$Response, y = .data$`Valid Percent`, 
               label = paste0(round(.data$`Valid Percent`, 1), "%"))) +
    geom_col(fill = barcolor, width = barwidth) +
    geom_bar_text(size = textsize,
                  min.size = min_textsize,
                  family = "Roboto",
                  padding.y = grid::unit(textvjust, "mm"),
                  outside = TRUE) +
    labs(caption = n_par(data = data, item = ensym(item), lang = lang)) +
    theme_sep() +
    theme(axis.text.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(linetype = "dashed"),
          plot.caption = element_text(color = "grey"))

}
