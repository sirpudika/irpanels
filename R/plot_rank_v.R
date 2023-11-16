#' Vertical barplot to rank items
#'
#' @param data a \code{data.frame} object
#' @param item a vector lisiting the survey items
#' @param by optional argument for distinction by subgroup
#' @param treat optional argument for distinction by treatment group
#' @param weights optional argument to weight output by survey weights
#' @param item_labels a \code{vector} listing the item labels for the y-axis
#' @param question optional argument to add question text in caption
#' @param lang optional argument for language (German = "DE" (default), English = "EN")
#' @param n.items optional argument to set the number of items per rank (default: all items; if fewer: items with highest values)
#' @param ncol.wrap optional argument to set the number of facet_wrap columns when distinguishing by subgroup or treatment group
#' @param textsize optional argument to adjust the text's size
#' @param min_textsize optional argument to set the minimum text size
#' @param percent_position optional argument to set position of percentage within bar ("center", "left", "top", etc.)
#' @param barwidth optional argument to define the width of the bar
#' @param legend.pos optional argument to set position of legend
#' @param ... further arguments of \code{scale_fill_manual} like \code{labels}
#'
#' @return a horizontal barplot for multiple items
#' @export
#'
#' @import tidyverse
#' @import ggfittext
#' @import tidyr
#'

plot_rank_v <- function(data, item, by, treat, weights,
                        item_labels, question,
                        lang = "DE", n.items = length(item_labels), ncol.wrap = 1,
                        textsize = 8, min_textsize = 5, percent_position = "center",
                        barwidth = 0.6, legend.pos = "right", ...){
  
  # Are item labels and question provided?
  environment <- ls(.GlobalEnv)
  if(missing(item_labels) & "item_labels" %in% environment){
    item_labels <- get("item_labels", envir = .GlobalEnv)
  } else if (missing(item_labels)) {
    stop("Labels ('item_labels') must be defined.")
  }
  
  if(missing(question) & "question" %in% environment){
    question <- get("question", envir = .GlobalEnv)
    
    if(grepl("Fragetext: «", question) | grepl("Question text: «", question)){
      question_text <- paste0(question, "\n")
    } else {
      question_text <- ifelse(lang == "DE",
                              paste0("Fragetext: «", question, "»\n"),
                              paste0("Question text: «", question, "»\n"))
    }
    
  } else if (missing(question)) {
    question <- NA
    question_text <- ""
    
  } else {
    
    if(grepl("Fragetext: «", question) | grepl("Question text: «", question)){
      question_text <- paste0(question, "\n")
    } else {
      question_text <- ifelse(lang == "DE",
                              paste0("Fragetext: «", question, "»\n"),
                              paste0("Question text: «", question, "»\n"))
    }
  }
  
  # create weights column (if set to 1, no weighting occurs)
  if(missing(weights)){
    data$weight <- 1
  } else {
    data <- data %>% 
      mutate(weight = {{weights}})
  }
  
  # compute numbers
  if(missing(treat) & missing(by)) { #item without treatment or subgroups
    plot <- data %>% 
      dplyr::select(all_of(item), weight) %>% 
      pivot_longer(all_of(item), names_to = "variable", values_to = "value") %>%
      filter(value > 0) %>% 
      group_by(variable, value) %>% 
      count(wt = weight) %>% 
      group_by(variable) %>% 
      mutate(total_item = sum(n),
             freq_rel = n/total_item,
             percentage = paste0(round(freq_rel*100, 1), "%"),
             rank = rank(desc(freq_rel))) %>% 
      ungroup() %>% 
      filter(rank <= n.items)
    
    # set caption according to language
    caption <- ifelse(lang == "DE", 
                      paste0(question_text,
                             irpanels::n_par(data = data, item = item)),
                      paste0(question_text,
                             irpanels::n_par(data = data, item = item, 
                                             lang = "EN")))
    
  } else if(!missing(treat) & missing(by)) { #item with treatment groups
    plot <- data %>% 
      dplyr::select(all_of(item), treat = {{treat}}, weight) %>% 
      pivot_longer(all_of(item), names_to = "variable", values_to = "value") %>% 
      filter(value > 0) %>% 
      group_by(treat, variable, value) %>% 
      count(wt = weight) %>% 
      group_by(treat, variable) %>%
      mutate(total_item = sum(n),
             freq_rel = n/total_item,
             percentage = paste0(round(freq_rel*100, 1), "%"),
             rank = rank(desc(freq_rel))) %>% 
      ungroup() %>% 
      filter(rank <= n.items)
    
    # set caption according to language
    caption <- ifelse(lang == "DE", 
                      paste0(question_text,
                             irpanels::n_par(data = data, item = item, 
                                             treat = {{treat}})),
                      paste0(question_text,
                             irpanels::n_par(data = data, item = item, 
                                             treat = {{treat}},
                                             lang = "EN")))
    
  } else if(missing(treat) & !missing(by)) { #item with subgroups
    plot <- data %>% 
      dplyr::select(all_of(item), by = {{by}}, weight) %>% 
      pivot_longer(all_of(item), names_to = "variable", values_to = "value") %>% 
      filter(value > 0 & !is.na(by)) %>% 
      group_by(by, variable, value) %>% 
      count(wt = weight) %>% 
      group_by(by, variable) %>%
      mutate(total_item = sum(n),
             freq_rel = n/total_item,
             percentage = paste0(round(freq_rel*100, 1), "%"),
             rank = rank(desc(freq_rel))) %>% 
      ungroup() %>% 
      filter(rank <= n.items)
    
    # set caption according to language
    caption <- ifelse(lang == "DE", 
                      paste0(question_text,
                             irpanels::n_par(data = data, item = item, 
                                             by = {{by}})),
                      paste0(question_text,
                             irpanels::n_par(data = data, item = item, 
                                             by = {{by}},
                                             lang = "EN")))
    
  } else { #item with both treatment and subgroups
    plot <- data %>% 
      dplyr::select(all_of(item), treat = {{treat}}, by = {{by}}, weight) %>% 
      pivot_longer(all_of(item), names_to = "variable", values_to = "value") %>% 
      filter(value > 0 & !is.na(by)) %>% 
      group_by(treat, by, variable, value) %>% 
      count(wt = weight) %>% 
      group_by(treat, by, variable) %>%
      mutate(total_item = sum(n),
             freq_rel = n/total_item,
             percentage = paste0(round(freq_rel*100, 1), "%"),
             rank = rank(desc(freq_rel))) %>% 
      ungroup() %>% 
      filter(rank <= n.items) 
    
    # set caption according to language
    caption <- ifelse(lang == "DE", 
                      paste0(question_text,
                             irpanels::n_par(data = data, item = item, 
                                             by = {{by}}, treat = {{treat}})),
                      paste0(question_text,
                             irpanels::n_par(data = data, item = item, 
                                             by = {{by}}, treat = {{treat}},
                                             lang = "EN")))
    
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
    plot$value <- ifelse(plot$value == i, item_labels[i], plot$value)
  }
  
  # The color stacking should be in the order of how often smt is considered rank 1
  # (or, if it never appears in rank 1, then how often rank 2 or 3)
  fill_order <- plot %>% 
    arrange(variable, rank, .keep_all = TRUE) %>% 
    pull(value) %>% 
    unique() %>% 
    rev()
  
  # plot
  p <- ggplot(plot, aes(factor(variable, levels = unique(variable)), freq_rel,
                        fill = factor(value, levels = fill_order), 
                        label = percentage)) +
    geom_col(width = barwidth) +
    ggfittext::geom_bar_text(outside = FALSE,
                             position = position_stack(vjust = 0.5),
                             place = percent_position,
                             size = textsize, 
                             min.size = min_textsize) +
    labs(x = NULL, y = NULL, caption = caption) +
    irpanels::theme_sep() +
    scale_fill_manual(...) +
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
  if(missing(treat) & missing(by)) { #item without treatment or subgroups
    p
  } else if(!missing(treat) & missing(by)) { #item with treatment groups
    p + facet_wrap(~ treat, ncol = ncol.wrap)
  } else if(missing(treat) & !missing(by)) { #item with subgroups
    p + facet_wrap(~ by, ncol = ncol.wrap)
  } else { #item with both treatment and subgroups
    p +  facet_grid(treat ~ by)
  }
}
