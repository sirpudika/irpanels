helper_percentage <- function(x, digits=1) {
  sprintf(paste0("%1.", digits, "f%%"), x*100)
}

