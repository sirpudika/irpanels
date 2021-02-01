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

## -----------------------------------------------------------------------------
load("upanel_w5.RData")
eda_loadtoenv(w5)

## -----------------------------------------------------------------------------
eda_freqtable(item = w5_q27)
eda_freqtable(item = w5_q27, rm.dk = T)

## -----------------------------------------------------------------------------
eda_crosstable(items = c("w5_q2", "w5_q11"))

## -----------------------------------------------------------------------------
eda_participants(item = w5_q11)
eda_participants(item = w5_q27)

## -----------------------------------------------------------------------------
plot_bar_v(item = w5_q11)
plot_bar_h(item = w5_q11)

## -----------------------------------------------------------------------------
plot_bar_h(item = w5_q11, dk_share = 12) +
    scale_x_discrete(labels = c("Item One", "Item Two", "Item Three", "Item Four", "Item Five"))

## -----------------------------------------------------------------------------
eda_freqtable(item = w5_q11)
plot_groupbar_h(item = w5_q11, grouper = w5_q2, dk_share = 12, labels = c("Male", "Female")) +
  scale_x_discrete(labels = c("Item One", "Item Two", "Item Three", "Item Four", "Item Five"))

## -----------------------------------------------------------------------------
options(ggplot2.discrete.fill= c("#59C3C3", "#52489C"))

## -----------------------------------------------------------------------------
plot_groupbar_h(item = w5_q11, grouper = w5_q2, dk_share = 12, labels = c("Male", "Female")) +
  scale_x_discrete(labels = c("Item One", "Item Two", "Item Three", "Item Four", "Item Five"))

