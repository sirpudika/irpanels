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
library(dplyr)
library(ggplot2)

## ---- echo=FALSE--------------------------------------------------------------
ls("package:sep")

## -----------------------------------------------------------------------------
load("upanel_w5.RData")

## -----------------------------------------------------------------------------
eda_freqtable(w5, item = w5_q27)
eda_freqtable(w5, item = w5_q27, rm.dk = T)

## -----------------------------------------------------------------------------
eda_crosstable(w5, items = c("w5_q2", "w5_q11"))
eda_crosstable(w5, items = c("w5_q2", "w5_q11"), rm.dk = TRUE)

## -----------------------------------------------------------------------------
plot_bar_v(w5, item = w5_q11)

## -----------------------------------------------------------------------------
plot_bar_h(w5, item = w5_q11) +
    scale_x_discrete(labels = rev(c("Item One", "Item Two", "Item Three", "Item Four", "Item Five")))

## -----------------------------------------------------------------------------
plot_groupbar_v(w5, item = w5_q11, by = w5_q2, labels = c("Female", "Male"), values = sep_palette("Standard")) +
 scale_x_discrete(labels = c("Item One", "Item Two", "Item Three", "Item Four", "Item Five"))

## -----------------------------------------------------------------------------
plot_groupbar_h(w5, item = w5_q11, by = w5_q2, labels = c("Female", "Male"), values = sep_palette("Darkblues")) +
 scale_x_discrete(labels = rev(c("Item One", "Item Two", "Item Three", "Item Four", "Item Five")))

