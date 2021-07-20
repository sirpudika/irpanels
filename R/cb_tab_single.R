#' Creates Page with Table in Codebook for a single variabe
#'
#' @param num.var num.var placeholder
#' @param data  a \code{data.frame} object
#' @param comment comment placeholder
#' @param lbl.space lbl.space placeholder
#' @param lblen.space lblen.space placeholder
#' @param mis.space mis.space placeholder
#' @param .cbtab .cbtab placeholder
#'
#' @return Latex document with Table from Kable<
#' @export
#'
cb_tab_single <-
  function(num.var, data = upanel.meta, .cbtab = cbtab,
           comment = "", lbl.space = "1em", lblen.space = "1em", mis.space = "1em"){
    cat("###", as.character(data[1, num.var]), sep = " ")
    cat("\n")
    print(.cbtab(num.var = num.var, lbl.space = lbl.space,
                 lblen.space = lblen.space, mis.space = mis.space))
    cat("\n")
    cat(comment, sep = "\n")
    cat("\n")
    cat("\\newpage")
  }
