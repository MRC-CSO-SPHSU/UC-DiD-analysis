library(fst)
library(tidyverse)
library(data.table)
library(survey)
library(SPHSUgraphs)


satis_income_df <-
  c(
    "data/fst_files/apr11_mar12.fst",
    "data/fst_files/apr12_mar13.fst",
    "data/fst_files/apr13_mar14.fst",
    "data/fst_files/apr14_mar15.fst",
    "data/fst_files/apr15_mar16.fst",
    "data/fst_files/apr16_mar17.fst",
    "data/fst_files/apr17_mar18.fst",
    "data/fst_files/apr18_mar19.fst",
    "data/fst_files/apr19_mar20.fst",
    "data/fst_files/apr20_mar21.fst"
  ) |> map(read_fst)

# df <- read_fst("data/fst_files/apr19_mar20.fst")

names(satis_income_df) <- 2011:2020


10:20 |> 
 (\(yr) paste0("pwta", yr))() |> 
  map_dfr(function(lab) {
    map_dfr(satis_income_df, has_name, lab) |> 
      mutate(lab = lab)
  }) |> 
  pivot_longer(-lab, names_to = "year", values_to = "pres") |> 
  ggplot(aes(year, lab, fill = pres)) +
  geom_tile() +
  scale_fill_sphsu()

satis_income_df$`2016` |> 
  lm(satis ~ gross99, data = _, weights = pwta18)
  

des1 <- survey::svydesign(id  = ~1, weights = ~pwta18, data = as_tibble(satis_income_df$`2016`))

svyglm(satis ~ gross99, design = des1, family = gaussi)