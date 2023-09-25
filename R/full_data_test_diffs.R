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

full_dat <- tibble(year = 2011:2020, data = satis_income_df) |> 
  mutate(data = map(data, ~select(.x, age, satis, worth, anxious, happy, statr, ilodefr, refdte, statr))) |> 
  unnest(data) |> 
  filter(age > 17 | age < 66) |> 
  mutate(across(satis:happy, ~ ifelse(.x == -9|.x == -8, NA, .x)),
         refdte = str_pad(trimws(refdte), 8, side = "left", pad =  "0"),
         Date = floor_date(dmy(refdte), "months"))

setDT(full_dat)

full_dat[,`:=`(empl = case_when(
  ilodefr == 1 ~ "Employed",
  ilodefr == 2 ~ "Unemployed"),
  empl_a = case_when(
    statr == 1 ~ "Employed",
    TRUE ~ "Unemployed"
  )
  )]


full_dat |> 
  filter(!is.na(empl)) |> 
  # pivot_longer(satis:happy, names_to = "metric", values_to = "score") |>
  melt(measure.vars = c("satis", "worth", "anxious", "happy"
  ), variable.name = "metric", value.name = "score", variable.factor = FALSE) |> 
  filter(!is.na(score)) |> 
  ggplot(aes(year, score, colour = empl)) +
  stat_summary(geom = "errorbar", fun.data = mean_cl_normal) +
  stat_summary(geom = "line", fun = mean) +
  facet_wrap(~metric, scales = "free_y")




full_dat |> 
  pivot_longer(satis:happy, names_to = "metric", values_to = "score") |> 
  ggplot(aes(year, score, colour = empl_a)) +
  stat_summary(geom = "line", fun = mean) +
  facet_wrap(~metric, scales = "free_y")


full_dat[, time := ifelse(year == 2019, 1, 0)]


library(magrittr)

full_dat[(year == 2013 | year == 2019) & !is.na(empl)] %$%
  lm(anxious ~ empl*time) |> 
  summary()

full_dat[(year == 2013 | year == 2019) & !is.na(empl)] %$%
  lm(satis ~ empl*time) |> 
  summary()

full_dat[(year == 2013 | year == 2019) & !is.na(empl)] %$%
  lm(happy ~ empl*time) |> 
  summary()

full_dat[(year == 2013 | year == 2019) & !is.na(empl)] %$%
  lm(worth ~ empl*time) |> 
  summary()


