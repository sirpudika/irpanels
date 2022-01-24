## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE,
  fig.retina = 4,
  fig.height = 6, 
  fig.width = 8
)

## ---- eval=FALSE--------------------------------------------------------------
#  devtools::install_github("bonschorno/sep")

## ----setup--------------------------------------------------------------------
library(sep)
library(ggplot2)
library(kableExtra)
library(readxl)

## ---- eval=FALSE--------------------------------------------------------------
#  load("upanel_w5.RData")
#  upanelmeta_raw = read_excel("upanel_w5_metadata.xlsx")
#  upanelmeta = data.frame(t(upanelmeta_raw))

## ---- eval=FALSE--------------------------------------------------------------
#  meta = 1:13
#  codes_de = 14:15
#  codes_en = 16:17
#  miscodes = 18:23

## ---- results='asis', eval=FALSE----------------------------------------------
#  cb_table(metadata = upanelmeta, num.var = 14)

## ---- results='asis', eval=FALSE----------------------------------------------
#  cb_sumtab(metadata = upanelmeta, response = w5, num.var = 14, type = "numeric")

## ---- results='asis', eval=FALSE----------------------------------------------
#  cb_sumplot(metadata = upanelmeta, response = w5, num.var = 14, stats = "count")

