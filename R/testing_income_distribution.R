library(fst)
library(data.table)
library(tidyverse)
library(dtplyr)
library(lubridate)
library(SPHSUgraphs)
library(patchwork)

theme_set(theme_sphsu_light())

apr14_mar21 <- c(
  "data/fst_files/apr14_mar15.fst",
  "data/fst_files/apr15_mar16.fst",
  "data/fst_files/apr16_mar17.fst",
  "data/fst_files/apr17_mar18.fst",
  "data/fst_files/apr18_mar19.fst",
  "data/fst_files/apr19_mar20.fst",
  "data/fst_files/apr20_mar21.fst"
) |> map(read_fst, c(
  "grsswk",
  "refdte"))


apr14_mar21 |> 
  reduce(bind_rows) |> 
  mutate(grsswk = if_else(grsswk < 0, 0, grsswk)) |> 
  ggplot(aes(grsswk)) +
  geom_histogram(aes(y = after_stat(count/sum(count))))
