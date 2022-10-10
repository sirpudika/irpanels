#' Vertical barplot to rank items
#'
#' @param data a \code{data.frame} object
#' @param item a survey item
#' @param by optional argument for distinction by subgroup
#' @param treat optional argument for distinction by treatment group
#' @param lang optional argument for language (German = "DE" (default), English = "EN")
#' @param n.items optional argument to set the number of items per rank (default: all items; if fewer: items with highest values)
#' @param ncol.wrap optional argument to set the number of facet_wrap columns when distinguishing by subgroup or treatment group
#' @param textsize optional argument to adjust the text's size
#' @param min_textsize optional argument to set the minimum text size
#' @param percent_position optional argument to set position of percentage within bar ("center", "left", "top", etc.)
#' @param barwidth optional argument to define the width of the bar
#' @param legend.pos optional argument to set position of legend
#'
#' @return a horizontal barplot for multiple items
#' @export
#'
#' @import tidyverse
#' @import ggfittext
#'

plot_rank_v <- function(data, item, by = NULL, treat = NULL, lang = "DE",
                        n.items = length(labs), ncol.wrap = 1,
                        textsize = 8, min_textsize = 5, percent_position = "center",
                        barwidth = 0.6, legend.pos = "right"){
  
  # error message if question or labels are not predefined
  environment <- ls(.GlobalEnv)
  stopifnot("Labels ('labs') must be defined." = ("labs" %in% environment),
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
             percentage = paste0(round(freq_rel*100, 1), "%"),
             rank = rank(desc(freq_rel))) %>% 
      ungroup() %>% 
      filter(rank <= n.items)
    
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
             percentage = paste0(round(freq_rel*100, 1), "%"),
             rank = rank(desc(freq_rel))) %>% 
      ungroup() %>% 
      filter(rank <= n.items)
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
             percentage = paste0(round(freq_rel*100, 1), "%"),
             rank = rank(desc(freq_rel))) %>% 
      ungroup() %>% 
      filter(rank <= n.items)
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
             percentage = paste0(round(freq_rel*100, 1), "%"),
             rank = rank(desc(freq_rel))) %>% 
      ungroup() %>% 
      filter(rank <= n.items) 
  }
  
  # give proper rank labels
  if(lang == "DE"){ #German
    for(i in 1:length(item)){
      plot$variable <- ifelse(plot$variable == item[i], paste("Rang\n", i), plot$variable)
    }
  } else { # English
    for(i in 1:length(item)){
      plot$variable <- ifelse(plot$variable == item[i], paste("Rank\n", i), plot$variable)
    }
  }
  
  # give proper labels (item text)
  for(i in unique(plot$value)){ 
    plot$value <- ifelse(plot$value == i, labs[i], plot$value)
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
  p <- ggplot(plot, aes(factor(variable, levels = unique(variable)), freq_rel,
                        fill = factor(value), label = percentage)) +
    geom_col(width = barwidth) +
    ggfittext::geom_bar_text(outside = FALSE,
                             position = position_stack(vjust = 0.5),
                             place = percent_position,
                             size = textsize, 
                             min.size = min_textsize) +
    labs(x = NULL, y = NULL, caption = caption) +
    irpanels::theme_sep() +
    scale_y_continuous(labels = scales::percent) +
    theme(plot.caption = element_text(color = "grey"),
          plot.caption.position =  "plot",
          plot.title = element_text(hjust = 0.5),
          legend.position = legend.pos,
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(linetype = "dashed"),
          panel.grid.minor.y = element_line(linetype = "dashed")) +
    guides(fill = guide_legend(keywidth = 0.4, keyheight = 0.8, 
                               label = T, reverse = F, title = NULL))
  
  # print plot
  if(is.null(treat) & is.null(by)) { #item without treatment or subgroups
    p
  } else if(!is.null(treat) & is.null(by)) { #item with treatment groups
    p + facet_wrap(~ treat, ncol = ncol.wrap)
  } else if(is.null(treat) & !is.null(by)) { #item with subgroups
    p + facet_wrap(~ by, ncol = ncol.wrap)
  } else { #item with both treatment and subgroups
    p +  facet_grid(treat ~ by)
  }
}
