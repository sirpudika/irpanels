#' The SEP theme
#'
#' This function, strongly inspired by Bob Rudis' \href{https://github.com/hrbrmstr/hrbrthemes}{hrbrthemes}, aims to standardize the appearance of the plots in SEP Reports. The function is written to be highly adaptable. Thus, it is more of a basic idea than an actual template for the plots. Note, this function currently only works if the "Roboto" family is installed on your system (publicly available here: link)!
#'
#' @param base_family base font family
#' @param base_size base font size
#' @param plot_title_family plot title family
#' @param plot_title_size plot title size
#' @param plot_title_face plot title face
#' @param plot_title_margin plot title margin
#' @param subtitle_family subtitle font family
#' @param subtitle_size subtitle size
#' @param subtitle_face subtitle face
#' @param subtitle_margin subtitle margin
#' @param strip_text_family strip text family
#' @param strip_text_size strip text size
#' @param strip_text_face strip text face
#' @param caption_family caption font family
#' @param axis_text_size axis text size
#' @param axis_title_family axis title font family
#' @param axis_title_size axis title size
#' @param axis_title_face axis title face
#'
#' @return the sep theme
#' @export
#'
#' @examples
#' \dontrun{
#'
#' ## set as theme
#' theme_set(theme_sep())
#'
#' ## or use locally
#'
#' library(ggplot2)
#' ggplot(data = mtcars, aes(x = gear, y = am)) +
#' geom_point() +
#' theme_sep()
#' }

theme_sep <- function (base_family = "Roboto", base_size = 11.5, plot_title_family = base_family,
                      plot_title_size = 18, plot_title_face = "bold", plot_title_margin = 10,
                      subtitle_family = base_family, subtitle_size = 12, subtitle_face = "plain",
                      subtitle_margin = 15, strip_text_family = base_family, strip_text_size = 12,
                      strip_text_face = "plain", caption_family = base_family,
                      axis_text_size = base_size, axis_title_family = subtitle_family,
                      axis_title_size = 9, axis_title_face = "plain")
{
  template <- ggplot2::theme_minimal(base_family = base_family,
                                     base_size = base_size) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
          panel.grid.minor = ggplot2::element_blank(),
          axis.title = ggplot2::element_blank())

  return(template)
}
