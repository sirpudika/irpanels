#' Detect "Don't Know" and "None" Labels
#'
#' This is mainly a helper function for the creation of the codebook table.
#' It detects whether a given variable has "Don't Know" or "None"  response labels.
#'
#' @param data a \code{data.frame} object
#' @param num.var single numeric value referring to column index
#' @param .str_dk a character vector including all response options that are coded as -8 ("Don't Know" category)
#' @param .str_no a character vector including all response options that are coded as -9 ("None" category)
#'
#' @return named integer vector
#' @export
#'
cb_dkno <- function(data = upanel.meta, num.var, .str_dk = str_dk, .str_no = str_no){

  is_dk <- sum(sapply(.str_dk, grepl, data["-8", num.var]))
  is_no <- sum(sapply(.str_no, grepl, data["-9", num.var]))
  is_dkno <- sum(is_dk, is_no)


  dk_num = which(rownames(upanel.meta) == "-8")
  no_num = which(rownames(upanel.meta) == "-9")
  dk_num_en = which(rownames(upanel.meta) == "-8 (EN)")
  no_num_en = which(rownames(upanel.meta) == "-9 (EN)")

  if(is_dk > 0 & is_no > 0){
    dkno_code <- c(is_dkno, dk_num, no_num, dk_num_en, no_num_en)
    names(dkno_code) <- c("N. of DK/None Values",
                          "Position of DK Code",
                          "Position of None Code",
                          "Position of DK Code (EN)",
                          "Position of None Code (EN)")}
  else{
    if(is_dk > 0 & is_no == 0){
      dkno_code <- c(is_dkno, dk_num, dk_num_en)
      names(dkno_code) <- c("N. of DK/None Values",
                            "Position of DK Code",
                            "Position of DK Code (EN)")}
    else{
      if(is_dk == 0 & is_no > 0){
        dkno_code <- c(is_dkno, no_num, no_num_en)
        names(dkno_code) <- c("N. of DK/None Values",
                              "Position of None Code",
                              "Position of None Code (EN)")}
      else{
        dkno_code <- c(is_dkno)
        names(dkno_code) <- c("N. of DK/None Values")
      }}}

  return(dkno_code)
}
