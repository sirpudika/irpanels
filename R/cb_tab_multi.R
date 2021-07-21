#'  Creates Page with Table in Codebook for multiple variabes
#'
#' @param num.var add the number of the variable
#' @param data  a \code{data.frame} object
#' @param comment add a comment under the codebook table
#' @param lbl.space lbl.space placeholder
#' @param lblen.space lblen.space placeholder
#' @param mis.space mis.space placeholder
#' @param multi.vars multi.vars placeholder
#'
#' @return cb_tab_multi object
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
