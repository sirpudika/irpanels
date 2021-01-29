## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE
)

## -----------------------------------------------------------------------------
# install.packages("devtools") 
#devtools::install_github("bonschorno/sep")

## ----setup, include=FALSE-----------------------------------------------------
library(sep)

## -----------------------------------------------------------------------------
load("upanel_w5.RData")
eda_loadtoenv(w5)

## -----------------------------------------------------------------------------
eda_freqtable(item = w5_q27)
eda_freqtable(item = w5_q27, rm.dk = T)

