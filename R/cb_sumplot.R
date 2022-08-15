#' Summary Plot of responses for codebook
#'
#' @param metadata a \code{data.frame} object with survey metadata
#' @param response a \code{data.frame} object with survey response data
#' @param num.var single numeric value indexing column of variable in metadata
#' @param na_sep a boolean indicating use of SEP coding rules (defaults to TRUE)
#' @param stats a string indicating the type of plotted statistic (count, density)
#'
#' @return plot with counts or density of responses
#' @export
#'
#' @import ggplot2
#'
cb_sumplot = function(metadata, response, num.var, na_sep = TRUE, stats){

  name = as.character(metadata[num.var, "Variable name"])
  variable = response[[name]]

  if(na_sep == TRUE) {
    na = sum(variable < -9)
  } else {
    na = sum(is.na(variable))
  }

  na_text = paste0("Number of NA values: ", na)

  if(stats == "count") {

    if(na_sep == TRUE) {
      variable = variable[variable >= -9]
    } else {
      variable = variable[!is.na(variable)]
    }

    df = data.frame(variable = variable, color = NA)

    df$fill = ifelse(df$variable <= -8, "light", "dark")

    df$variable = as.factor(df$variable)

    n_plot_text = paste0("Number of plotted values: ", length(variable))

    caption = paste(n_plot_text, na_text, sep = "\n")

    # Bar plot: value distribution
    barplot = ggplot(data = df) +
      geom_bar(aes(x = variable, fill = fill), alpha = 0.6) +
      scale_fill_manual(values = c("#404040", "#C0C0C0")) +
      labs(y = "N respondents",
           caption = caption) +
      theme_minimal() +
      theme(legend.position = "none",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.title.y = element_text(size = 10),
            axis.title.x = element_blank(),
            plot.caption.position = "plot")

    print(barplot)

  } else if(stats == "density") {

    if(na_sep) {
      has_dk = "-8" %in% colnames(metadata) && !is.na(metadata[metadata[, "Variable name"] == name, "-8"])
      has_no = "-9" %in% colnames(metadata) && !is.na(metadata[metadata[, "Variable name"] == name, "-9"])
    } else {
      has_dk = FALSE
      has_no = FALSE
    }

    if(has_dk) {
      dk = sum(variable == -8)
      dk_text = paste0("Number of Don't know values: ", dk)
    }

    if(has_no) {
      no = sum(variable == -9)
      no_text = paste0("Number of None values: ", no)
    }

    if(na_sep) {
      variable = variable[variable > -8]
    } else {
      variable = variable[!is.na(variable)]
    }

    n_plot = length(variable)
    n_plot_text = paste0("Number of plotted values: ", n_plot)

    mean = mean(variable, na.rm = TRUE)
    median = median(variable, na.rm = TRUE)

    df = data.frame(variable = variable, mean = mean, median = median)

    if(has_dk && has_no) {
      caption = paste(n_plot_text, dk_text, no_text, na_text, sep = "\n")}

    if(has_dk && !has_no) {
      caption = paste(n_plot_text, dk_text, na_text, sep = "\n")}

    if(!has_dk && has_no) {
      caption = paste(n_plot_text, no_text, na_text, sep = "\n")}

    if(!has_dk && !has_no) {
      caption = paste(n_plot_text, na_text, sep = "\n")}

    denseplot = ggplot(data = df) +
      geom_density(aes(x = variable)) +
      geom_vline(aes(xintercept = mean, linetype = "Mean"), alpha = 0.6) +
      geom_vline(aes(xintercept = median, linetype = "Median"), alpha = 0.6) +
      scale_linetype_manual(breaks = c("Mean", "Median"),
                            values = c("dashed", "dotted"),
                            name = element_blank()) +
      labs(y = "Density",
           caption = caption) +
      theme_minimal() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 10),
            plot.caption.position = "plot")

    print(denseplot)

  } else {
    stop("No valid argument for 'stats' (needs to be either 'count' or 'density')")
  }
}
