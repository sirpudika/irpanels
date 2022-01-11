#' Codebook Table for one variable
#'
#' @param metadata a \code{data.frame} object with survey metadata
#' @param num.var single numeric value indexing column of variable in metadata
#' @param .meta numeric vector indexing rows of variable information in metadata (defaults to meta)
#' @param .codes numeric vector indexing rows of German value labels in metadata (defaults to codes)
#' @param .codes.en numeric vector indexing rows of English value labels in metadata (defaults to codes.en)
#' @param .miscodes numeric vector indexing rows of missing labels in metadata (defaults to miscodes)
#' @param lbl.space optional argument to adjust table space between variable information and German value labels (defaults to 1em)
#' @param lblen.space optional argument to adjust table space between German value labels and English value labels (defaults to 1em)
#' @param mis.space optional argument to adjust table space between English value labels and missing labels (defaults to 1em)
#'
#' @return codebook table in Latex format
#' @export
#'
#' @importFrom kableExtra kable pack_rows kable_styling column_spec
#'
cb_tab <-
  function(metadata, num.var,
           .meta = meta, .codes = codes, .codes.en = codes.en, .miscodes = miscodes,
           lbl.space = "1em", lblen.space = "1em", mis.space = "1em"){

    values = c(.codes[!is.na(metadata[.codes, num.var])],
               .codes.en[!is.na(metadata[.codes.en, num.var])])

    if(length(values) == 0){

      metadata[sort(c(.meta, values, .miscodes)), num.var] %>%
        ##define table format
        kable("latex", col.names = NULL, booktabs = T, longtable = T) %>%
        ##group missing labels
        pack_rows("Missing Labels", length(.meta)+1,
                  c(length(.meta)+length(.miscodes)),
                  latex_gap_space = mis.space, bold = F, italic = T) %>%
        ##define format specifications
        kable_styling(latex_options = c("striped", "hold_position" )) %>%
        column_spec(2, width = "35em")
    }else{
      metadata[sort(c(.meta, values, .miscodes)), num.var] %>%
        ##define table format
        kable("latex", col.names = NULL, booktabs = T, longtable = T) %>%
        ##group value labels (German)
        pack_rows("Value Labels", length(.meta)+1,
                  c(length(.meta)+c(length(values)/2)),
                  latex_gap_space = lbl.space) %>%
        ##group value labels (English)
        pack_rows("Value Labels (EN)",c(length(.meta)+c(length(values)/2)+1),
                  c(length(.meta)+length(values)),
                  latex_gap_space = lblen.space) %>%
        ##group . missing labels
        pack_rows("Missing Labels",c(length(.meta)+length(values)+1),
                  c(length(.meta)+length(values)+length(.miscodes)),
                  latex_gap_space = mis.space, bold = F, italic = T) %>%
        ##define format specifications
        kable_styling(latex_options = c("striped", "hold_position" )) %>%
        column_spec(2, width = "35em")
    }
  }
