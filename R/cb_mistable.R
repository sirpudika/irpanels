#' Codebook Table for missing labels
#'
#' @param metadata a \code{data.frame} object with survey metadata
#' @param num.var single numeric value indexing column of variable in metadata (defaults to 1 as missing codes are usually available for the first variable)
#' @param .miscodes numeric vector indexing rows of missing labels in metadata (defaults to miscodes)
#'
#' @return table with missing labels
#' @export
#'
#'@importFrom kableExtra kable kable_styling column_spec
#'
cb_mistable <- function(metadata, num.var = 1, .miscodes = miscodes){

  getinfo = metadata[num.var, .miscodes]

  df = data.frame(names = colnames(getinfo), values = as.character(getinfo[1, ]))

  mistable = df %>%
    kableExtra::kable("latex", col.names = NULL, booktabs = T) %>%
    kableExtra::kable_styling(latex_options = c("striped", "hold_position" )) %>%
    kableExtra::column_spec(2, width = "35em")

  print(mistable)
}
