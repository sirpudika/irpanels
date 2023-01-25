#' Grouped horizontal barplot
#'
#' @param data a \code{data.frame} object
#' @param item a survey item
#' @param by grouping variable
#' @param lang optional argument for language (German = "DE" (default), English = "EN")
#' @param barpadding optional argument to adjust padding between bars
#' @param barposition optional argument to determine the positioning of the bars (default: "dodge")
#' @param legendtitle optional argument to define a legend title
#' @param textsize optional argument to adjust the text's size
#' @param min_textsize optional argument to set the minimum text size
#' @param ... further arguments of \code{scale_fill_manual} like \code{labels}
#'
#' @return a grouped horizontal barplot
#' @export
#'
#' @import dplyr
#' @import ggplot2
#' @import forcats
#' @import ggfittext
#'

plot_groupbar_h <- function(data, item, by, lang = "DE", 
                            barpadding = 0.1, barposition = "dodge", legendtitle = "", textsize = 8, min_textsize = 5, ...){
  
  if(missing(barposition) | barposition == "dodge"){
    barposition <- position_dodge2(padding = barpadding)
    out <- TRUE
    percent_position <- "right"
    barwidth <- NULL
  } else if(barposition == "stack"){
    out <- FALSE
    percent_position <- "center"
    barwidth <- 0.5
  } else {
    out <- TRUE
    percent_position <- "right"
    barwidth <- NULL
  }
  
  data %>%
    filter({{item}} > -8,
           {{by}} > -8) %>%
    group_by({{item}}, {{by}}) %>%
    count() %>%
    group_by({{item}}) %>%
    mutate(freq = n/sum(n)) %>%
    ggplot(aes(x = fct_rev(as.factor({{item}})), y = .data$freq, 
               fill = as.factor({{by}}), label = irpanels::helper_percentage(.data$freq, 1))) +
    geom_col(position = barposition, width = barwidth) +
    geom_bar_text(family = "Roboto",
                  size = textsize,
                  min.size = min_textsize,
                  position = barposition,
                  outside = out,
                  place = percent_position,
                  fullheight = TRUE,
                  color = "white",
                  contrast = TRUE) +
    scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
    scale_fill_manual(...) +
    coord_flip() +
    labs(title = "",
         subtitle = "",
         fill = legendtitle,
         caption = n_par(data, item = ensym(item), by = {{by}}, lang = lang)) +
    guides(fill = guide_legend(reverse=TRUE)) +
    irpanels::theme_sep() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(linetype = "dashed"),
          legend.position = "bottom",
          plot.caption = element_text(color = "grey"),
          axis.text.x = element_blank())
}

