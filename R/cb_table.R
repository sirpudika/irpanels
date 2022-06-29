#' Codebook Table for one variable
#'
#' @param metadata a \code{data.frame} object with survey metadata
#' @param num.var single numeric value indexing column of variable in metadata
#' @param .meta numeric vector indexing rows of variable information in metadata (defaults to meta)
#' @param .codes_de numeric vector indexing rows of German value labels in metadata (defaults to codes_de)
#' @param .codes_en numeric vector indexing rows of English value labels in metadata (defaults to codes_en)
#' @param .miscodes numeric vector indexing rows of missing labels in metadata (defaults to miscodes)
#' @param lbl.space optional argument to adjust table space between variable information and German value labels (defaults to 1em)
#' @param lblen.space optional argument to adjust table space between German value labels and English value labels (defaults to 1em)
#' @param mis.space optional argument to adjust table space between English value labels and missing labels (defaults to 1em)
#' @param escape optional argument to escape special characters when producing HTML or LaTeX tables. True is save mode (defaults to TRUE)
#'
#' @return codebook table in Latex format
#' @export
#'
#' @importFrom kableExtra kable pack_rows kable_styling column_spec
#'
cb_table <-
  function(metadata, num.var,
           .meta = meta, .codes_de = codes_de, .codes_en = codes_en, .miscodes = miscodes,
           lbl.space = "1em", lblen.space = "1em", mis.space = "1em", escape = TRUE){

    values = c(.codes_de[!is.na(metadata[num.var, .codes_de])],
               .codes_en[!is.na(metadata[num.var, .codes_en])])

    getinfo = metadata[num.var, sort(c(.meta, values, .miscodes))]

    df = data.frame(names = colnames(getinfo), values = as.character(getinfo[1, ]))

    name = as.character(metadata[num.var, "Variable name"])

    if(length(values) == 0){

      cbtable = df %>%
        ##define table format
        kableExtra::kable("latex", col.names = NULL, booktabs = T, longtable = T) %>%
        ##group missing labels
        kableExtra::pack_rows("Missing Labels", length(.meta)+1,
                  c(length(.meta)+length(.miscodes)),
                  latex_gap_space = mis.space, bold = F, italic = T) %>%
        ##define format specifications
        kableExtra::kable_styling(latex_options = c("striped", "hold_position" )) %>%
        kableExtra::column_spec(2, width = "35em")

      cat("###", name, sep = " ")
      print(cbtable)

    }else{

      cbtable = df %>%
        ##define table format
        kableExtra::kable("latex", col.names = NULL, booktabs = T, longtable = T) %>%
        ##group value labels (German)
        kableExtra::pack_rows("Value Labels", length(.meta)+1,
                  c(length(.meta)+c(length(values)/2)),
                  latex_gap_space = lbl.space) %>%
        ##group value labels (English)
        kableExtra::pack_rows("Value Labels (EN)",c(length(.meta)+c(length(values)/2)+1),
                  c(length(.meta)+length(values)),
                  latex_gap_space = lblen.space) %>%
        ##group . missing labels
        kableExtra::pack_rows("Missing Labels",c(length(.meta)+length(values)+1),
                  c(length(.meta)+length(values)+length(.miscodes)),
                  latex_gap_space = mis.space, bold = F, italic = T) %>%
        ##define format specifications
        kableExtra::kable_styling(latex_options = c("striped", "hold_position" )) %>%
        kableExtra::column_spec(2, width = "35em")

      cat("###", name, sep = " ")
      print(cbtable)

    }
  }
