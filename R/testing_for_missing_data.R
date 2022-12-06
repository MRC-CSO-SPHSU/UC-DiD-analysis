library(fst)
library(tidyverse)
library(data.table)
library(survey)
library(SPHSUgraphs)


full_dataset <-
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
  ) |> map(read_fst,
           c("age",
             "refdte",
             "sex",
             "marsta",
             "country",
             "ilodefr",
             "gross99",
             "satis",
             "anxious",
             "happy",
             "worth",
             "grsswk",
             "grsswk2",
             "empmon",
             # "nolwm",
             "ioutcome",
             "caind",
             # "ethuk11",
             # "fdpch19",
             # "oycirc",
             # "ucredit",
             # "heal[20](01-17)",
             # "tpben09(01-09)",
             # "tpben13(01-10)",
             # "qualch11(01-06)",
             # "lnglst",
             # "limita",
             # "limitk",
             # "pwta18",
             # "pwta20",
             "benfts"),
           as.data.table = TRUE
          )


full_dataset |> 
  reduce(bind_rows) |>
  # count(ioutcome, satis) |> 
  # pivot_wider(names_from = satis, values_from = n)
  filter(ioutcome == 1, caind == 1) |> 
  summarise(across(.fns = ~sum(.x == -8|.x == -9)/n()))


full_dataset |> 
  reduce(bind_rows) 
