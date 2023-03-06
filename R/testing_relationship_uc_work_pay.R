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
  "age",
  "tpbn1301", 
  "tpbn1302", 
  "tpbn1303", 
  "tpbn1304", 
  "tpbn1305", 
  "tpbn1306", 
  "tpbn1307", 
  "tpbn1308", 
  "tpbn1309", 
  "tpbn1310", 
  "grsswk",
  "sumhrs",
  "refdte"))

wts1 <- c(
  "data/fst_files/apr14_mar15.fst",
  "data/fst_files/apr15_mar16.fst",
  "data/fst_files/apr16_mar17.fst",
  "data/fst_files/apr17_mar18.fst",
  "data/fst_files/apr18_mar19.fst"
  # "data/fst_files/apr19_mar20.fst"
  # "data/fst_files/apr20_mar21.fst"
) |> map(read_fst, "pwta18") |> 
  map(`colnames<-`, "weight")

wts2 <- c("data/fst_files/apr19_mar20.fst",
          "data/fst_files/apr20_mar21.fst") |>
  map(read_fst, "pwta20") |> 
  map(`colnames<-`, "weight")


# combined_dt <- apr14_mar21 |> 
#   map2(append(wts1, wts2), cbind) |> 
#   reduce(bind_rows) |> 
#   mutate(date = floor_date(dmy(refdte), "months")) |> 
#   as.data.table()

apr14_mar21_dt <- apr14_mar21 |> 
  map2(append(wts1, wts2), cbind) |> 
  reduce(bind_rows) |> 
  mutate(date = floor_date(dmy(refdte), "months")) |> 
  as.data.table()

apr14_mar21_dt <- apr14_mar21_dt[age > 17 & age < 66]

apr14_mar21_dt[,uc := rowSums(.SD == 1), .SD = 2:11]
apr14_mar21_dt[,tax_cr := rowSums(.SD == 3), .SD = 2:11]
apr14_mar21_dt[,housing := rowSums(.SD == 2), .SD = 2:11]
apr14_mar21_dt[,income_s := rowSums(.SD == 4), .SD = 2:11]
apr14_mar21_dt[,jsa := rowSums(.SD == 5), .SD = 2:11]
apr14_mar21_dt[,disab := rowSums(.SD == 6), .SD = 2:11]
apr14_mar21_dt[,other := as.numeric(rowSums(.SD)>0), .SD = tax_cr:jsa]

# linear model testing uc effect on earned income not mediated through hours worked
library(magrittr)

apr14_mar21_dt |> 
  filter((uc == 1 | other == 1) & sumhrs > 0) %$%
  lm(grsswk ~ uc + sumhrs) |> 
  summary()
