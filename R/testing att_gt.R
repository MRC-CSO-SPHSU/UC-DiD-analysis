library(tidyverse)
library(did)
set.seed(5565)

df <- tibble(id = rep(1:30, each = 100),
             t = rep(1:100, 30),
             Y = 3 + 2 * t) |>
  left_join(tibble(id = 1:30,
                   cohort = factor(1+floor((id-1)/6))),
                   # cohort = factor(sample(0:4, 30, replace = TRUE))),
            by = "id")

baseline <- tibble(
  id = 1:30,
  base = rnorm(30, 20, 5)
)

df <- tibble(
  cohort = factor(1:5),
  effect = sample(2:10, 5),
  timing = sample(25:50, 5)
) |> 
  left_join(df, ., by = "cohort") |> 
  left_join(baseline, by = "id") |> 
  mutate(Y = Y + base,
         timing = if_else(cohort == 1, 0L, timing))

df <- df |> 
  mutate(
    D = if_else(t >= timing & timing != 0, 1, 0),
    Y = if_else(D == 1, Y + (t - timing) * effect, Y)
  ) 

jitter <- tibble(
  id = 1:30,
  jitter = runif(30, -10, 10)
)


df <- df |> 
  mutate(Y = Y + rnorm(nrow(df), 5, 2)) 

df |> 
  left_join(jitter, by = "id") |> 
  mutate(Y = Y + jitter) |>
  ggplot(aes(t, Y, colour = cohort, group = id)) +
  geom_line()

did2 <- df |> 
att_gt(
  yname = "Y",
  gname = "timing",
  idname = "id",
  tname = "t",
  xformla = ~ 1,
  data = _,
  est_method = "reg",
  control_group = "nevertreated"
)

did2

ggdid(did2)

es <- aggte(did2, type = "dynamic")

summary(es)

ggdid(es)


group_eff <- aggte(did2, type = "group")

group_eff
