library(tidyverse)
library(lubridate)
library(fst)


pre_post_covid <- c(
  "pre_c" = "data/fst_files/apr19_mar20.fst",
  "post_c" = "data/fst_files/apr20_mar21.fst"
) |> map(read_fst, c(
  "satis", 
  "anxious", 
  "happy", 
  "worth", 
  "pwta20",
  "refdte"),
  as.data.table = TRUE)

pre_post_dat <- pre_post_covid |> 
  imap(~mutate(.x, period = .y)) |> 
  reduce(bind_rows) |> 
  mutate(across(satis:worth, ~ ifelse(.x == -9|.x == -8, NA, .x)),
         Date = floor_date(dmy(refdte), "months"),
         Month = factor(month(Date)),
         Year = factor(year(Date)),
         period = fct_inorder(period)) 

pre_post_dat |> 
  pivot_longer(satis:worth, names_to = "metric", values_to = "Score") |> 
  filter(!is.na(Score)) |> 
  ggplot(aes(period, Score)) +
  stat_summary(geom = "pointrange", shape = 15,
               fun.data = mean_se) +
  facet_wrap(~metric, scale = "free_y")

library(fixest)

tibble(outcome = c("satis", "happy", "worth", "anxious")) |> 
  mutate(model = map(outcome, \(outcome) {
    
    feols(as.formula(paste(outcome, "~ period | Month")), data = pre_post_dat, weights = ~pwta20)
    
  })) |> 
  mutate(coefs = map(model, broom::tidy, conf.int = TRUE)) |> 
  unnest(coefs) |> 
  filter(term == "periodpost_c")


pre_post_dat |> 
  as_tibble() |> 
  pivot_longer(satis:worth, names_to = "outcome", values_to = "score") |> 
  ggplot(aes(Date, score)) +
  stat_summary(geom = "line", fun = mean) +
  stat_summary(fun.data = mean_cl_normal, aes(colour = Year)) +
  geom_vline(xintercept = ymd("20200401")) +
  facet_wrap(~outcome, scales = "free_y")

# March is start of exposure effects (anticipatory)

# longer lead-in ----------------------------------------------------------


apr11_mar21 <- c(
  `2013` = "data/fst_files/apr13_mar14.fst",
  `2014` = "data/fst_files/apr14_mar15.fst",
  `2015` = "data/fst_files/apr15_mar16.fst",
  `2016` = "data/fst_files/apr16_mar17.fst",
  `2017` = "data/fst_files/apr17_mar18.fst",
  `2018` = "data/fst_files/apr18_mar19.fst"
) |> map(read_fst, c(
  "satis", 
  "anxious", 
  "happy", 
  "worth", 
  "refdte",
  "pwta18"),
  as.data.table = TRUE) |> 
  c(
    c(
  `2019` = "data/fst_files/apr19_mar20.fst",
  `2020` = "data/fst_files/apr20_mar21.fst"
) |> map(read_fst, c(
  "satis", 
  "anxious", 
  "happy", 
  "worth", 
  "refdte",
  "pwta20"),
  as.data.table = TRUE) 
  )




longer_data <- apr11_mar21 |> 
  imap(~mutate(.x, period = .y)) |> 
  map(rename_with, \(x) str_replace(x, "pwta\\d{2}", "weight")) |> 
  reduce(bind_rows) |> 
  mutate(across(satis:worth, ~ ifelse(.x == -9|.x == -8, NA, .x)),
         Date = floor_date(dmy(refdte), "months"),
         Month = factor(month(Date)),
         Year = factor(year(Date)),
         period = fct_inorder(period) |> as.integer(),
         covid = as.integer(Date >= ymd("20200301"))) 

longer_data |> 
pivot_longer(satis:worth, names_to = "metric", values_to = "Score") |> 
  filter(!is.na(Score)) |> 
  ggplot(aes(Year, Score)) +
  stat_summary(geom = "pointrange", shape = 15,
               fun.data = mean_se) +
  facet_wrap(~metric, scale = "free_y")

tibble(outcome = c("satis", "happy", "worth", "anxious")) |> 
  mutate(model = map(outcome, \(outcome) {
    
    feols(as.formula(paste(outcome, "~ covid + Year | Month")), data = longer_data, weights = ~weight)
    
  })) |> 
  mutate(coefs = map(model, broom::tidy, conf.int = TRUE)) |> 
  unnest(coefs) |> 
  filter(term == "covid")

read_table("
UC	COVID
-0.66	-0.23
-0.41	-0.23
-0.73	-0.12
+0.79	+0.43
") |> 
  mutate(mult = UC/COVID)
