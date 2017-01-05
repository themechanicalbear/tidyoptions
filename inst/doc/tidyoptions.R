## ---- echo = FALSE, message = FALSE--------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
options(tibble.print_min = 4L, tibble.print_max = 4L)
library(dplyr)
library(data.table)

## ---- echo=FALSE, results='asis', warning=FALSE, message=FALSE, error=FALSE----
library(tidyoptions)
knitr::kable(raw_example[, 1:10])
knitr::kable(raw_example[, 11:22])

## ---- eval=FALSE---------------------------------------------------------
#  tidy_options("XLE", "data/raw_files/options", "data/volatility/vx.xle.daily.prices.RData")

## ---- echo=FALSE, results='asis', warning=FALSE, message=FALSE, error=FALSE----
knitr::kable(processed_example[, 1:7])
knitr::kable(processed_example[, 8:16])
knitr::kable(processed_example[, 17:23])

