#' Creates a Codebook Table for a given variable (row)
#'
#' @param data a \code{data.frame} object
#' @param num.var single numeric value referring to column index
#' @param .meta numeric vector referring to row indices of variable information
#' @param .codes numeric vector referring to row indices of variable codes and labels
#' @param .codes.en numeric vector referring to row indices of variable codes and labels in English
#' @param .miscodes miscodes placeholder
#' @param .str_dk a character vector including all response options that are coded as -8 ("Don't Know" category)
#' @param .str_no a character vector including all response options that are coded as -9 ("None" category)
#' @param lbl.space optional argument to adjust table space between variable information and value labels
#' @param lblen.space optional argument to adjust table space between value labels and value labels in English
#' @param mis.space optional argument to adjust table space between value labels in English and missing labels
#'
#' @return codebook table in Latex format
#' @export
#'
#' @importFrom kableExtra kable pack_rows kable_styling column_spec
#'
cb_tab <-
  function(data = upanel.meta, num.var,
           .meta = meta, .codes = codes, .codes.en = codes.en, .miscodes = miscodes,
           .str_dk = str_dk, .str_no = str_no,
           lbl.space = "1em", lblen.space = "1em", mis.space = "1em"){

    values = c(.codes[!is.na(data[.codes, num.var])],
               .codes.en[!is.na(data[.codes.en, num.var])])

    dkno_code = cb_dkno(data, num.var, .str_dk, .str_no)[-1]

    if(length(values) == 0){

      data[sort(c(.meta, values, dkno_code, .miscodes)), num.var] %>%
        ##define table format
        kable("latex", col.names = NULL, booktabs = T, longtable = T) %>%
        ##group . missing labels
        pack_rows("Missing Labels",c(length(.meta)+length(dkno_code)+1),
                              c(length(.meta)+length(dkno_code)+length(.miscodes)),
                              latex_gap_space = mis.space, bold = F, italic = T) %>%
        ##define format specifications
        kable_styling(latex_options = c("striped", "hold_position" )) %>%
        column_spec(2, width = "35em")
    }else{
      data[sort(c(.meta, values, dkno_code, .miscodes)), num.var] %>%
        ##define table format
        kable("latex", col.names = NULL, booktabs = T, longtable = T) %>%
        ##group value labels (German)
        pack_rows("Value Labels", length(.meta)+1,
                              c(length(.meta)+c(length(values)/2)+c(length(dkno_code)/2)),
                              latex_gap_space = lbl.space) %>%
        ##group value labels (English)
        pack_rows("Value Labels (EN)",c(length(.meta)+c(length(values)/2)+c(length(dkno_code)/2)+1),
                              c(length(.meta)+length(values)+length(dkno_code)),
                              latex_gap_space = lblen.space) %>%
        ##group . missing labels
        pack_rows("Missing Labels",c(length(.meta)+length(values)+length(dkno_code)+1),
                              c(length(.meta)+length(values)+length(dkno_code)+length(.miscodes)),
                              latex_gap_space = mis.space, bold = F, italic = T) %>%
        ##define format specifications
        kable_styling(latex_options = c("striped", "hold_position" )) %>%
        column_spec(2, width = "35em")
    }
  }
