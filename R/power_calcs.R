library(fst)

aps_2021 <- read_fst("data/fst_files/apr20_mar21.fst")
library(tidyverse)

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
  ) |> map(read_fst, columns = c("refdte", "age", "satis"))

# df <- read_fst("data/fst_files/apr19_mar20.fst")

names(satis_income_df) <- 2011:2020

stats <- tibble(year = as.character(2011:2020), data = satis_income_df[year]) |> 
  unnest(data) |> 
  mutate(satis = na_if(satis, -9)) |> 
  filter(age < 20, !is.na(satis)) |> 
  summarise(mean = mean(satis), sd = sd(satis))

library(pwr)

pwr.t.test(
  n = 385000,
  d = 0.05 / stats$sd,
  sig.level = 0.01
)

stats$mean


pwr.t.test(d = 0.2, power = 0.8, sig.level = 0.05)

aps_2021 |> 
  as_tibble() |> 
  select(age, satis) |> 
  filter(age < 20, !is.na(satis)) |> 
  summarise(mean = mean(satis), sd = sd(satis))
