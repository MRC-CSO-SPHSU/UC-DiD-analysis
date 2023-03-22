library(fst)
library(SPHSUgraphs)
library(tidyverse)
library(haven)

make_perc <- function(p, dp = 1) {
  sprintf(glue::glue("%.{dp}f%%"), p * 100)
}

library(tidyverse)

lfs_hh <- haven::read_sav("data/UKDA-7101-spss/spss/spss19/lfsh_aj11_eul.sav")

lfs_hh |> 
  select(starts_with("hhtype"),
         starts_with("hserial"),
         starts_with("hrp"))

lfs_hh |> 
  select(starts_with("HSERIAL"))

hh_by_region <- lfs_hh |> 
  rename_all(str_to_upper) |> 
  group_by(HSERIALP) |> 
  slice_head(n = 1) |> 
  group_by(GOVTOF, HHTYPE6) |> 
  summarise(n = sum(PHHWT14), .groups = "drop_last") |>
  mutate(perc_in_area = make_perc(n/sum(n))) |> 
  ungroup() |> 
  mutate(across(where(haven::is.labelled), .fns = as_factor))

lfs_hh |> 
  filter(PERSNO == 1) |> 
  select(HSERIALP, CASENOP, PERSNO)

lfs_hh |> 
  group_by(HSERIALP) |> 
  arrange(PERSNO) |> 
  slice_head(n = 1) |> 
  select(CASENOP, PERSNO) |> 
  ggplot(aes(as_factor(PERSNO))) + 
  stat_count()
  

hh_by_region |> 
  ggplot(aes(n, GOVTOF, fill = HHTYPE6)) +
  geom_col(position = "fill") +
  scale_x_continuous(labels = scales::percent_format())+
  scale_fill_sphsu()


lfs_hh |> 
  group_by(HSERIALP) |> 
  slice_head(n = 1)
  count(THISWV)



aps_2020_ind <- read_sav("data/UKDA-8789-spss/spss/spss25/apsp_jd20_eul_pwta22.sav")

aps_2020_ind |> select(contains("hrp"))

aps_2020_hh <- haven::read_sav("data/UKDA-8861-spss/spss/spss25/apsh_jd20_eul_phhwta22.sav")

aps_2020_hh |> 
  select(starts_with("hhtype"),
         starts_with("he"),
         starts_with("hserial"),
         starts_with("hrp"))

aps_2020_hh |> 
  filter(HRP == 1) |> 
  group_by(GOVTOF, HHTYPE6) |> 
  summarise(n = sum(PHHWTA22), .groups = "drop_last") |>
  mutate(perc_in_area = make_perc(n/sum(n))) |> 
  ungroup() |> 
  mutate(across(where(haven::is.labelled), .fns = as_factor))


  # count(HSERIALP) |> 
  # arrange(desc(n))
  # 

# Checking all files ------------------------------------------------------

hh_set_path <- "data/hh_set_test/sav_files/"

all_savs <- dir(hh_set_path) |> 
  paste0(hh_set_path, x = _) |> 
  map(haven::read_sav, col_select = starts_with("H"))

all_savs |> 
  map(select, starts_with("HS"))
