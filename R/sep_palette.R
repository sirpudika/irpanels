#' All colors
#'
#' @export
sep_palettes <- list(
Standard = c("#1F407A", "#485A2C", "#1269B0", "#72791C", "#91056A", "#6F6F6F", "#A8322D", "#007A96", "#956013"),
Darkblues = c("#385C9B", "#748DB9", "#A0B1D0", "#CDD6E6", "#E6EbF3"),
Greens = c("#67804E", "#8DA07A", "#B3C0A7", "#D9DfD3" ,"#ECEFE9"),
Lightblues = c("#4187C0", "#71A5D0", "#A0C3DF", "#D0E1EF" ,"#E7F0F7"),
Lightgreens = c("#8E9449", "#AAAF77", "#C7C9A4", "#E3E4D2" ,"#F1F1E8"),
Pinks = c("#A73788", "#BD69A5", "#D39BC3", "#E9CDE1" ,"#F4E6F0"),
BluePink = c("#BD69A5", "#D39BC3", "lightgrey", "#748DB9", "#385C9B")
)
#' ETH color palettes
#'
#' This is a non-exhaustive selection of official color palettes as listed on the ETH Zurich website. For further information, see: \href{https://ethz.ch/services/en/service/communication/corporate-design/colours.html}{ETH Colors}. This function was largely adopted from Karthik Ram's great \href{https://github.com/karthik/wesanderson}{wesanderson} package.
#'
#' @param name Name of the palette
#' @param n Number of numbers to be chosen from the palette
#'
#' @return ETH color palette of your choice
#' @export
#'
#' @examples
#' sep_palette("Darkblues")
#' sep_palette("Pinks", n = 3)
#
sep_palette <- function(name, n) {

  pal <- sep_palettes[[name]]
  if (is.null(pal))
    stop("Palette not found.")
  if (missing(n)) {
    n <- length(pal)
  }
  if (n > length(pal)) {
    stop("Not enough colors.")
  }
  out <- pal[1:n]
  structure(out, class = "palette", name = name)
}
