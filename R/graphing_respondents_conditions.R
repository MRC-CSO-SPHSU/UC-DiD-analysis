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
  "refdte"))



n_uc <- apr14_mar21 |> 
  reduce(bind_rows) |> 
  mutate(date = floor_date(dmy(refdte), "months"),
         across(tpbn1301:tpbn1310, ~.x == 1)) |> 
  rowwise() |> 
         mutate(uc = sum(tpbn1301:tpbn1310))

n_uc |> 
  group_by(date) |>
  select(date, uc) |> 
  summarise(prop_uc = sum(uc)/n()) |> 
  ggplot(aes(date, prop_uc)) +
  geom_col(fill = sphsu_cols("University Blue"), width = 31) +
  scale_x_date("Year") +
  scale_y_continuous("Percentage of respondents on UC", labels = scales::percent,
                     expand = expansion(mult = c(0, 0.05)))


apr14_mar21_dt <- apr14_mar21 |> 
  reduce(bind_rows) |> 
  mutate(date = floor_date(dmy(refdte), "months")) |> 
  data.table()

apr14_mar21_dt <- apr14_mar21_dt[age > 17 & age < 66]

apr14_mar21_dt[,uc := rowSums(.SD == 1), .SD = 2:11]
apr14_mar21_dt[,tax_cr := rowSums(.SD == 3), .SD = 2:11]
apr14_mar21_dt[,housing := rowSums(.SD == 2), .SD = 2:11]
apr14_mar21_dt[,income_s := rowSums(.SD == 4), .SD = 2:11]
apr14_mar21_dt[,jsa := rowSums(.SD == 5), .SD = 2:11]
apr14_mar21_dt[,disab := rowSums(.SD == 6), .SD = 2:11]
apr14_mar21_dt[,other := rowSums(.SD), .SD = tax_cr:jsa]

(perc_other <- apr14_mar21_dt |> 
  as_tibble() |> 
  group_by(date) |>
  select(date, other) |> 
  summarise(prop_other = sum(other)/n()) |> 
  ggplot(aes(date, prop_other)) +
  geom_col(fill = sphsu_cols("Pumpkin"), width = 31) +
  scale_x_date("Year") +
  scale_y_continuous("Respondents on\nLegacy Benefits", labels = scales::percent,
                     expand = expansion(mult = c(0, 0.05))))

(perc_all <- apr14_mar21_dt |> 
  as_tibble() |> 
  group_by(date) |>
  select(date, uc) |> 
  summarise(prop_uc = sum(uc)/n()) |> 
  ggplot(aes(date, prop_uc)) +
  geom_col(fill = sphsu_cols("University Blue"), width = 31) +
  scale_x_date("Year") +
  scale_y_continuous("Respondents on UC", labels = scales::percent,
                     expand = expansion(mult = c(0, 0.05))))

(perc_claim <- apr14_mar21_dt |> 
  group_by(date) |>
  select(date, uc, other) |> 
  filter(uc == 1 | other == 1) |> 
  mutate(benefits = case_when(
    uc == 1 & other == 1 ~ "Combination",
    uc == 1 ~ "Universal Credit",
    other == 1 ~ "Legacy Benefits"
  ),
  benefits = factor(benefits, levels = c("Legacy Benefits", "Combination", "Universal Credit"))) |> 
  as_tibble() |> 
  ggplot(aes(date, fill = benefits)) +
  stat_count(position = "fill", geom = "bar", aes(y = after_stat(count)), width = 31) +
  scale_fill_manual(name = "Benefits claimed", values = sphsu_cols("Pumpkin", "Leaf", "University Blue", names = FALSE)) +
  scale_x_date("Year") +
  theme(legend.position = "bottom") +
  scale_y_continuous("Claimant by benefit types", labels = scales::percent,
                     expand = expansion(mult = c(0, 0.05))))


patch <- (perc_other/perc_all/perc_claim) & theme(panel.grid.major.x = element_blank(),
                              panel.grid.minor.x = element_blank()) 
patch +
  plot_annotation(title = "Proportions of working-age adults claiming benefit types",
                  subtitle = "April 2014 - March 2021")


ggsave(filename = "graphs/proporrions_benefit_type.png", width = 20, height = 24,
       units = "cm", dpi = 400)


(n_claim <- apr14_mar21_dt |> 
    group_by(date) |>
    select(date, uc, other) |> 
    filter(uc == 1 | other == 1) |> 
    mutate(benefits = case_when(
      uc == 1 & other == 1 ~ "Combination",
      uc == 1 ~ "Universal Credit",
      other == 1 ~ "Legacy Benefits"
    ),
    benefits = factor(benefits, levels = c("Legacy Benefits", "Combination", "Universal Credit"))) |> 
    as_tibble() |> 
    ggplot(aes(date, fill = benefits)) +
    stat_count(position = "stack", geom = "bar", aes(y = after_stat(count)), width = 31) +
    scale_fill_manual(name = "Benefits claimed", values = sphsu_cols("Pumpkin", "Leaf", "University Blue", names = FALSE)) +
    scale_x_date("Year") +
    theme(legend.position = "bottom") +
    scale_y_continuous("Claimant by benefit types",
                       expand = expansion(mult = c(0, 0.05))))
