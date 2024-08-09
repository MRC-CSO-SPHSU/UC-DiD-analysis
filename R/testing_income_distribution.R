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
  geom_histogram(aes(y = after_stat(count / sum(count))))

apr14_mar21 |>
  reduce(bind_rows) |> 
  filter(grsswk == max(grsswk)) |> 
  mutate(income = grsswk * 52)

apr14_mar21 |>
  reduce(bind_rows) |>
  mutate(
    income = cut(
      grsswk,
      breaks = c(-9, -8, 0, seq(1, 170, length.out = 5), seq(231, max(grsswk), length.out = 15)),
      right = FALSE,
      include.lowest = TRUE
    ),
      included = factor(
        case_when(
          grsswk < 0 ~ "Missing (included by default)",
          grsswk > 230 ~ "High income",
          .default = "Low income"
        )
      ),
  ) |> 
  ggplot(aes(income, fill = included)) +
  stat_count(geom = "bar") +
  scale_fill_discrete("") +
  scale_x_discrete("Annual income in employment", labels = c("-9", "-8", ">£0", rep("", 4), ">£12,000", rep("", 12), "£41,000+"))
  
# all income --------------------------------------------------------------


top_100 <- read_fst("data/fst_files/apr14_mar15.fst", 
         to = 100)

top_100 |> 
  select(starts_with("band"), starts_with("gr"), starts_with("inc"), starts_with("net")) |> names() |> dput()


earnings_21 <- read_fst("data/fst_files/apr20_mar21.fst", 
                    columns = c("bandg", "bandg2", "bandn", "bandn2", "gross99", "grsexp", 
                             "grsprd", "grsswk", "grsswk2", "incnow", "incsup", "net99", "netprd", 
                             "netwk", "netwk2"))


earnings_nas <- earnings_21 |> 
  tibble() |> 
  mutate(across(.fns = as.numeric)) |> 
  mutate(across(.fns = ~ if_else(.x == -9, NA_real_, .x))) 

earnings_nas |> 
  summarise(across(.fns = ~sum(is.na(.x))/n()))

earnings_nas |> 
  select(gross99, grsswk, incnow, net99) |> 
  pivot_longer(
    everything(),
    names_to = "metric",
    values_to = "value"
  ) |> 
  replace_na(list(value = 0)) |> 
  ggplot(aes(value)) +
  geom_histogram() +
  facet_wrap(~metric)

ukmod_income <- fread(file = "data/ukmod_out/uk_2019_UCAon.txt")

ukmod_income |> 
  ggplot(aes(yem)) +
  geom_histogram()
