#' Number of participants for survey item
#' 
#' @description 
#' Number of participants for survey item, distinguishable by subgroup and/or treatment group.
#' 
#' To be used with `plot_multiple_h()` and  `plot_rank_v()`.
#' 
#' For `plot_bar_h()`, `plot_bar_v()`, `plot_groupbar_h()` and `plot_groupbar_v()`, use the function `n_par()`.
#'
#'
#' @param data a \code{data.frame} object
#' @param item relevant survey item (i.e. w5_q17)
#' @param by optional argument for distinction by subgroup
#' @param treat optional argument for distinction by treatment group
#' @param lang language (German = "DE" (default), English = "EN", anything else goes neutral)
#'
#' @return Number of participants ("Don't knows"-answers included) for item
#'
#' @import dplyr
#'
#' @export
#'

n_par_by <- function(data, item, by = NULL, treat = NULL, lang = "DE"){
  
  # count
  if(is.null(by) & is.null(treat)){ # without treatment or subgroup
    count <- data %>% 
      dplyr::select(item) %>% 
      pivot_longer(item, 
                   names_to = "question", values_to = "value") %>% 
      filter(value > -1 & !is.na(value)) %>% 
      group_by(question) %>% 
      count() %>% 
      pull(n) %>% 
      unique()
  } else if(is.null(treat)){ # with subgroup
    count <- data %>% 
      dplyr::select(item, by = by) %>% 
      pivot_longer(item, 
                   names_to = "question", values_to = "value") %>% 
      filter(value > -1 & !is.na(value)) %>% 
      pivot_longer(by,
                   names_to = "category", values_to = "subgroup") %>% 
      filter(!is.na(subgroup)) %>% 
      group_by(question, category, subgroup) %>% 
      count() %>% 
      pull(n) %>% 
      unique()
  } else if(is.null(by)){ # with treatment group
    count <- data %>% 
      dplyr::select(item, treat = treat) %>% 
      pivot_longer(item, 
                   names_to = "question", values_to = "value") %>% 
      filter(value > -1 & !is.na(value)) %>% 
      pivot_longer(treat,
                   names_to = "treatment", values_to = "group") %>% 
      filter(!is.na(group)) %>% 
      group_by(question, treatment, group) %>% 
      count() %>% 
      pull(n) %>% 
      unique()
  } else { # with treatment and subgroup
    count <- data %>% 
      dplyr::select(item, by = by, treat = treat) %>% 
      pivot_longer(item, 
                   names_to = "question", values_to = "value") %>% 
      filter(value > -1 & !is.na(value)) %>% 
      pivot_longer(treat,
                   names_to = "treatment", values_to = "group") %>% 
      filter(!is.na(group)) %>% 
      pivot_longer(by,
                   names_to = "category", values_to = "subgroup") %>% 
      filter(!is.na(subgroup)) %>% 
      group_by(question, treatment, group, category, subgroup) %>% 
      count() %>% 
      pull(n) %>% 
      unique()
  }
  
  # print
  if(lang == "DE"){
    if(length(count) == 1 & length(item) == 1){paste("Grafik basiert auf N = ", count, sep = "")} 
    else if(length(count) == 1 & is.null(by) & is.null(treat)){paste("Grafik basiert auf N = ", count, " (pro Unterfrage)", sep = "")}
    else if(length(count) == 1){paste("Grafik basiert auf N = ", count, " (pro Unterfrage und Untergruppe)", sep = "")}
    else if(is.null(by) & is.null(treat)){paste("Die Anzahl Befragter variiert je nach Unterfrage zwischen ", min(count), " und ", max(count), ".", sep = "")}
    else {paste("Die Anzahl Befragter variiert je nach Unterfrage und Untergruppe zwischen ", min(count), " und ", max(count), ".", sep = "")}
  } else if(lang == "EN"){
    if(length(count) == 1 & length(item) == 1){paste("Plot is based on N = ", count, sep = "")} 
    else if(length(count) == 1 & is.null(by) & is.null(treat)){paste("Plot is based on N = ", count, " (per subitem)", sep = "")}
    else if(length(count) == 1){paste("Plot is based on N = ", count, " (per subgroup and subitem)", sep = "")}
    else if(is.null(by) & is.null(treat)){paste("The number of participants varies by subitem between ", min(count), " and ", max(count), ".", sep = "")}
    else {paste("The number of participants varies by subitem and subgroup between ", min(count), " and ", max(count), ".", sep = "")}
  } else {
    paste("N =", max(count))
  }
  
}
