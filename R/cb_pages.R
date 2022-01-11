#'  Pages with Codebook Tables for multiple variables
#'
#' @param metadata  a \code{data.frame} object with survey metadata
#' @param multi.vars numeric vector indexing columns of variables in metadata
#' @param comment optional character argument to add text below table
#' @param lbl.space optional argument to adjust table space between variable information and German value labels (defaults to 1em)
#' @param lblen.space optional argument to adjust table space between German value labels and English value labels (defaults to 1em)
#' @param mis.space optional argument to adjust table space between English value labels and missing labels (defaults to 1em)
#'
#' @return codebook pages for multiple variables in Latex format
#' @export
#'
cb_tab_multi <-
  function(multi.vars, metadata,
           comment = "", lbl.space = "1em", lblen.space = "1em", mis.space = "1em")
  {
    for(var in multi.vars){
      cat("###", as.character(metadata[1, var]), sep = " ")
      cat("\n")
      print(cb_tab(metadata = metadata, num.var = var, lbl.space = lbl.space,
                   lblen.space = lblen.space, mis.space = mis.space))
      cat("\n")
      cat(comment, sep = "\n")
      cat("\n")
      cat("\\newpage")
    }}
