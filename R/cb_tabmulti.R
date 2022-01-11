#'  Creates Page with Table in Codebook for multiple variabes
#'
#' @param data  a \code{data.frame} object
#' @param multi.vars numeric vector referring to column indeces
#' @param comment optional string argument printed below the codebook table
#' @param lbl.space optional argument to adjust table space between variable information and value labels
#' @param lblen.space optional argument to adjust table space between value labels and value labels in English
#' @param mis.space optional argument to adjust table space between value labels in English and missing labels
#'
#' @return codebook pages for multiple variables in Latex format
#' @export
#'
cb_tab_multi <-
  function(multi.vars, data = upanel.meta,
           comment = "", lbl.space = "1em", lblen.space = "1em", mis.space = "1em")
    {
    for(var in multi.vars){
      cat("###", as.character(data[1, var]), sep = " ")
      cat("\n")
      print(cb_tab(data = data, num.var = var, lbl.space = lbl.space,
                   lblen.space = lblen.space, mis.space = mis.space))
      cat("\n")
      cat(comment, sep = "\n")
      cat("\n")
      cat("\\newpage")
    }}
