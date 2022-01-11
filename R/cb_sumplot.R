#' Summary Plot of responses for codebook
#'
#' @param metadata
#' @param response
#' @param num.var
#' @param na_sep
#' @param stats
#'
#' @return
#' @export
#'
#' @import ggplot2
#'
cb_sumplot = function(metadata, response, num.var, na_sep = TRUE, stats){

  name = as.character(metadata[1, num.var])
  variable = response[[name]]

  if(stats == "count"){

    if(na_sep == TRUE){
      variable = ifelse(variable < -9, "NA", variable)}

    df = data.frame(variable = variable, color = NA)

    df$fill = ifelse(df$variable == "NA", "light", "dark")

    df$variable = fct_relevel(as.factor(df$variable), "NA")

    caption = paste0("Number of total respondents: ", length(variable))

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

  }else if(stats == "density"){

    has_dk = ifelse(!is.na(metadata["-8", metadata["Variable name", ] == name]), "yes", "no")
    has_no = ifelse(!is.na(metadata["-9", metadata["Variable name", ] == name]), "yes", "no")

    na = sum(variable < -9)
    na_text = paste0("Number of NA values: ", na)

    # Add if has_no ...
    if(has_dk == "yes"){
      dk = sum(variable == -8)
      dk_text = paste0("Number of Don't know values: ", dk)}

    if(has_no == "yes"){
      no = sum(variable == -9)
      no_text = paste0("Number of None values: ", no)}

    if(na_sep == TRUE){
      variable = variable[variable > -8]}

    n_plot = length(variable)
    n_plot_text = paste0("Number of plotted values: ", n_plot)

    mean = mean(variable)
    median = median(variable)

    df = data.frame(variable = variable, mean = mean, median = median)

    if(has_dk == "yes" & has_no == "yes"){
      caption = paste(n_plot_text, dk_text, no_text, na_text, sep = "\n")}

    if(has_dk == "yes" & has_no == "no"){
      caption = paste(n_plot_text, dk_text, na_text, sep = "\n")}

    if(has_dk == "no" & has_no == "yes"){
      caption = paste(n_plot_text, no_text, na_text, sep = "\n")}

    if(has_dk == "no" & has_no == "no"){
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

  }
}
