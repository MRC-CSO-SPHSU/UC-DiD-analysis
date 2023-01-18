library(fst)
library(tidyverse)
library(SPHSUgraphs)

make_perc <- function(p, dp = 1) {
  sprintf(glue::glue("%.{dp}f%%"), p * 100)
}

lfs_hh <- haven::read_sav("data/UKDA-9036-spss/spss/spss25/lfsh_js22_eul_phhwt22.sav")

lfs_hh |> 
  filter(PERSNO == 1) |> 
  select(CASENOP)

hh_by_region <- lfs_hh |> 
  group_by(HSERIALP) |> 
  slice_head(n = 1) |> 
  group_by(GOVTOF, HHTYPE6) |> 
  summarise(n = sum(PHHWT22), .groups = "drop_last") |>
  mutate(perc_in_area = make_perc(n/sum(n))) |> 
  ungroup() |> 
  mutate(across(where(haven::is.labelled), .fns = as_factor))

hh_by_region |> 
  ggplot(aes(n, GOVTOF, fill = HHTYPE6)) +
  geom_col(position = "fill") +
  scale_x_continuous(labels = scales::percent_format())+
  scale_fill_sphsu()


lfs_hh |> 
  group_by(HSERIALP) |> 
  slice_head(n = 1)
  count(THISWV)

# dat_2020_hh <- haven::read_sav("data/UKDA-8861-spss/spss/spss25/apsh_jd20_eul_phhwta22.sav")
dat_2020_hh <- haven::read_dta("data/UKDA-8861-stata/stata/stata13/apsh_jd20_eul_phhwta22.dta")

dat_2020_hh |> 
  filter(IOUTCOME == 1) |> 
  group_by(GOVTOF, HHTYPE6) |> 
  summarise(n = sum(PHHWTA22), .groups = "drop_last") |>
  mutate(perc_in_area = make_perc(n/sum(n))) |> 
  ungroup() |> 
  mutate(across(where(haven::is.labelled), .fns = as_factor))


  count(HSERIALP) |> 
  arrange(desc(n))
