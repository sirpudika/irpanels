#' Create simple barplots
#'
#' @param df dataframe
#' @param var variable
#' @param title plot title
#' @param ... any other option for geom_bar
#'
#' @return
#' @export
#'
#' @examples
#' simple_barplot(df = mtcars, var = gear, title = "Gears", width = 0.3)
simple_barplot <- function(df, var, title, ...){

  dat <- df %>%
    dplyr::filter({{var}} > 0) %>%
    dplyr::group_by({{var}}) %>%
    dplyr::count() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(freq_rel = round(n/sum(n), digits = 2))

  ggplot2::ggplot(data = dat, ggplot2::aes(x = as.factor({{var}}), y = freq_rel)) +
    ggplot2::geom_bar(stat = "identity", fill = "#1F407A", ...) +
    ggplot2::labs(title = title) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
          panel.grid.major.y = ggplot2::element_line(linetype = "dashed"))

}
