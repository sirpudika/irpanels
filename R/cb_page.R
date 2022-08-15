#' Page with Codebook Table for one variabe
#'
#' @param metadata  a \code{data.frame} object with survey metadata
#' @param num.var single numeric value indexing column of variable in metadata
#' @param comment optional character argument to add text below table
#' @param lbl.space optional argument to adjust table space between variable information and German value labels (defaults to 1em)
#' @param lblen.space optional argument to adjust table space between German value labels and English value labels (defaults to 1em)
#' @param mis.space optional argument to adjust table space between English value labels and missing labels (defaults to 1em)
#' @param escape optional argument to escape special characters when producing HTML or LaTeX tables. If FALSE, you need to escape all special characters in the table text (defaults to TRUE)
#' @param add_sumplot optional argument to add a summary plot below the table (defaults to FALSE)
#' @param response optional argument needed for summary plot: a \code{data.frame} object with survey response data (defaults to NULL)
#' @param stats optional argument needed for summary plot: a string indicating the type of plotted statistic (count, density) (defaults to empty string)
#'
#' @return codebook page for one variable in Latex format
#' @export
#'
cb_page <-
  function(metadata, num.var,
           comment = "", lbl.space = "1em", lblen.space = "1em", mis.space = "1em", escape = TRUE,
           add_sumplot = FALSE, response = NULL, stats = "") {

    # error checks
    if (add_sumplot && (is.null(response) || stats == "")) {
      stop("Missing arguments for the cb_sumplot function")
    }

    cb_table(metadata = metadata, num.var = num.var, lbl.space = lbl.space,
                 lblen.space = lblen.space, mis.space = mis.space, escape = escape)

    cat("\n")
    cat(comment, sep = "\n")
    cat("\n")

    if (add_sumplot) {
      cb_sumplot(metadata, response, num.var, stats = stats)
    }

    cat("\\newpage")
    cat("\n")
  }

