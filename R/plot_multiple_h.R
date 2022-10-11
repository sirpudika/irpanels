#' Horizontal barplot for multiple items
#'
#' @param data a \code{data.frame} object
#' @param item a vector listing the survey items
#' @param by optional argument for distinction by subgroup
#' @param treat optional argument for distinction by treatment group
#' @param lang optional argument for language (German = "DE" (default), English = "EN")
#' @param barwidth optional argument to define the width of the bar
#' @param ncol.wrap optional argument to set the number of facet_wrap columns when distinguishing by subgroup or treatment group
#' @param textsize optional argument to adjust the text's size
#' @param min_textsize optional argument to set the minimum text size
#' @param percent_position optional argument to set position of percentage within bar ("center", "left", "top", etc.)
#' @param legend.pos optional argument to set position of legend
#'
#' @return a vertical barplot for ranking
#' @export
#'
#' @import tidyverse
#' @import ggfittext
#'
#'

plot_multiple_h <- function(data, item, by = NULL, treat = NULL, lang = "DE",
                            barwidth = 0.6, ncol.wrap = 1, 
                            textsize = 8, min_textsize = 5, percent_position = "center",
                            legend.pos = "bottom"){
  
  # error message if question or labels are not predefined
  environment <- ls(.GlobalEnv)
  stopifnot("Labels ('item_labels') must be defined." = ("item_labels" %in% environment),
            "Question text ('question') must be defined." = ("question" %in% environment))
  
  # compute numbers
  if(is.null(treat) & is.null(by)) { #item without treatment or subgroups
    plot <- data %>% 
      dplyr:: select(all_of(item)) %>% 
      pivot_longer(all_of(item), names_to = "variable", values_to = "value") %>%
      filter(value > 0) %>% 
      group_by(variable, value) %>% 
      count() %>% 
      group_by(variable) %>% 
      mutate(total_item = sum(n),
             freq_rel = n/total_item,
             percentage = paste0(round(freq_rel*100, 1), "%"))
  } else if(!is.null(treat) & is.null(by)) { #item with treatment groups
    plot <- data %>% 
      dplyr::select(all_of(item), treat = all_of(treat)) %>% 
      pivot_longer(all_of(item), names_to = "variable", values_to = "value") %>% 
      filter(value > 0) %>% 
      group_by(treat, variable, value) %>% 
      count() %>% 
      group_by(treat, variable) %>%
      mutate(total_item = sum(n),
             freq_rel = n/total_item,
             percentage = paste0(round(freq_rel*100, 1), "%"))
  } else if(is.null(treat) & !is.null(by)) { #item with subgroups
    plot <- data %>% 
      dplyr::select(all_of(item), by = all_of(by)) %>% 
      pivot_longer(all_of(item), names_to = "variable", values_to = "value") %>% 
      filter(value > 0 & !is.na(by)) %>% 
      group_by(by, variable, value) %>% 
      count() %>% 
      group_by(by, variable) %>%
      mutate(total_item = sum(n),
             freq_rel = n/total_item,
             percentage = paste0(round(freq_rel*100, 1), "%"))
  } else { #item with both treatment and subgroups
    plot <- data %>% 
      dplyr::select(all_of(item), treat = all_of(treat), by = all_of(by)) %>% 
      pivot_longer(all_of(item), names_to = "variable", values_to = "value") %>% 
      filter(value > 0 & !is.na(by)) %>% 
      group_by(treat, by, variable, value) %>% 
      count() %>% 
      group_by(treat, by, variable) %>%
      mutate(total_item = sum(n),
             freq_rel = n/total_item,
             percentage = paste0(round(freq_rel*100, 1), "%"))
  }
  
  # give proper labels (item text)
  for(i in 1:length(item)){ 
    plot$variable <- ifelse(plot$variable == item[i], item_labels[i], plot$variable)
  }
  
  # set caption according to language
  caption <- ifelse(lang == "DE", 
                    paste("Fragetext: «", question, "»\n",
                          n_par(data = data, item = item, by = by, treat = treat),
                          sep = ""),
                    paste("Question text: «", question, "»\n",
                          n_par(data = data, item = item, by = by, treat = treat, 
                                lang = "EN"),
                          sep = ""))
  
  # plot
  p <- ggplot(plot, aes(freq_rel, factor(variable, levels = rev(item_labels)),
                        fill = factor(value), label = percentage)) +
    geom_col(width = barwidth) +
    ggfittext::geom_bar_text(outside = FALSE,
                             position = position_stack(vjust = 0.5),
                             place = percent_position,
                             size = textsize, 
                             min.size = min_textsize) +
    scale_x_continuous(labels = scales::percent) +
    labs(x = NULL, y = NULL, caption = caption) +
    irpanels::theme_sep() +
    theme(plot.caption = element_text(color = "grey"),
          plot.caption.position =  "plot",
          plot.title = element_text(hjust = 0.5),
          legend.position = legend.pos,
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(linetype = "dashed"),
          panel.grid.minor.x = element_line(linetype = "dashed")) +
    guides(fill = guide_legend(keywidth = 0.4, keyheight = 0.8, 
                               label = T, reverse = T, title = NULL))
  
  # print plot
  if(is.null(treat) & is.null(by)) { #item without treatment or subgroups
    p
  } else if(!is.null(treat) & is.null(by)) { #item with treatment groups
    p + facet_wrap(~ treat, ncol = ncol.wrap)
  } else if(is.null(treat) & !is.null(by)) { #item with subgroups
    p + facet_wrap(~ by, ncol = ncol.wrap)
  } else { #item with both treatment and subgroups
    p + facet_grid(treat ~ by)
  }
}
