#' Summary Plot of responses to multiple choice questions for codebook
#'
#' @param metadata a \code{data.frame} object with survey metadata
#' @param response a \code{data.frame} object with survey response data
#' @param multi.vars numeric vector indexing columns of variables in metadata
#' @param na_sep a boolean indicating use of SEP coding rules (defaults to TRUE)
#'
#' @return plot with counts of responses
#' @export
#'
#' @import ggplot2
#'

cb_sumplot_mcq = function(metadata, response, multi.vars, na_sep = TRUE){

  # store names and variables in a list (only choose those of type multiple choice)
  names = list()
  variables = list()
  sums = list()
  for(i in multi.vars) {
    if(as.character(metadata[i, "Question type"]) == "Multiple Choice") {
      name = as.character(metadata[i, "Variable name"])
      name = gsub("\\\\", "", name) # for the case that escape == F, remove backslashes
      names = append(names, name)
      variables[[name]] = list(response[[name]])
      sums[[name]] = sum(response[[name]], na.rm = T) # count the number of 1 responses
    }
  }

  # number of NA is the same for every item
  if(na_sep) {
    na = sum(variables[[1]][[1]] < -9)
    len = sum(variables[[1]][[1]] >= -9)
  } else {
    na = sum(is.na(variables[[1]][[1]]))
    len = sum(!is.na(variables[[1]][[1]]))
  }

  na_text = paste0("Number of NA values: ", na)
  n_plot_text = paste0("Number of plotted values: ", len)


  df = data.frame('count' = unlist(sums, use.names = F), 'names' = names(sums))

  caption = paste(n_plot_text, na_text, sep = "\n")

  barplot = ggplot(data = df, aes(x = names, y = count, fill = "dark")) +
    geom_bar(stat = "identity", alpha = 0.6) +
    scale_fill_manual(values = c("#404040", "#C0C0C0")) +
    labs(y = "N respondents",
         caption = caption,
         title = "Summary Plot for Multiple Choice Question",
         subtitle = "See description of the different options on the following pages") +
    theme_minimal() +
    theme(legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.y = element_text(size = 10),
          axis.title.x = element_blank(),
          plot.caption.position = "plot")

  print(barplot)

  cat("\n")
  cat("\n")
}
