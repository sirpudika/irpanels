#' Loading the relevant survey into your environment
#'
#' The functions of the package are written in such a way that once the survey is loaded into the global environment, the survey does not need to be specified each time a function is run. Consequently, the first step in the workflow is to use this function to read in the relevant survey (in data.frame format).
#'
#' @param data A data.frame object
#'
#' @return Loads the relevant object into the global environment
#' @export
#'
#' @examples
#' eda_loadtoenv(mtcars)
eda_loadtoenv <- function(data){

  if (!is.data.frame(data)) stop("data must be a data.frame object")
  currentwave <<- data
}
