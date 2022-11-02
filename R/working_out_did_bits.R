library(tidyverse)
library(bacondecomp)
library(SPHSUgraphs)
bacon_coef <- function(bacon_df) (bacon_df$estimate %*% bacon_df$weight)[1,1]

df <- tibble(
  id = factor(rep(1:3, each = 10)),
  t = rep(1:10, 3),
  D = case_when(
    id == 3 & t >= 5 ~ 1,
    id == 2 & t >= 8 ~ 1,
    TRUE ~ 0    
  ),
  Y = 0
  # Y = t + as.numeric(id)
  ) |> 
  mutate(
  Y = case_when(
    id == 2 & t >= 8 ~ D * 4 + Y,
    id == 3 & t >= 5 ~ D * 2 + Y,
    TRUE ~ Y
  )
)

ggplot(df, aes(t, Y, colour = id)) +
  geom_point(shape = 18, size = 3) + 
  geom_line(size = 1) +
  geom_vline(xintercept = 4.5, linetype = "dashed", colour = "grey") +
  geom_vline(xintercept = 7.5, linetype = "dashed", colour = "grey") +
  scale_y_continuous("Outcome score") +
  scale_colour_sphsu(name = "LA", "mixed") +
  scale_x_continuous("Years on UC", breaks = seq(2, 10, 2))

expand_grid(tibble(a = 1:3), tibble(b = 1:3)) |> 
  filter(a != b, b!= 1) |> 
  rowwise() |> 
  mutate(data = list(df |> 
           filter(id == a | id == b))) |> 
  unnest(data) |> 
  mutate(comparison = paste(a, b, sep = "v")) |> 
  filter(!(comparison == "2v3" & t >= 8),
         !(comparison == "3v2" & t <= 4)) |> 
ggplot(aes(t, Y, colour = id)) +
  geom_point(shape = 18, size = 3) + 
  geom_line(size = 1) +
  geom_vline(xintercept = 4.5, size = 1, linetype = "dashed", colour = "grey") +
  geom_vline(xintercept = 7.5, size = 1, linetype = "dashed", colour = "grey") +
  scale_y_continuous("Outcome score") +
  scale_colour_sphsu(name = "LA", "mixed") +
  scale_x_continuous("Years on UC", breaks = seq(2, 10, 2)) +
  facet_wrap(~ comparison)
  

summary(lm(Y ~ D + factor(t) + id, data = df))


df_bacon <- bacon(Y ~ D, data = df,
      id_var = "id",
      time_var = "t", quietly = FALSE)

bacon_coef(df_bacon)

df_bacon |> 
  ggplot(aes(weight, estimate, shape = factor(type))) +
  geom_point() +
  geom_hline(yintercept = bacon_coef(df_bacon))



# new part - where it all goes wrong! -------------------------------------

set.seed(5565)

create_df <- function(seed = 9,
                      units = 30,
                      time_points = 100,
                      cohorts = 5,
                      nevertreated = FALSE)
{
  set.seed(seed)
  df <- tibble(
    id = rep(1:units, each = time_points),
    t = rep(1:time_points, units),
    Y = 3 + 2 * t
  ) |>
    left_join(tibble(id = 1:units,
                     cohort = factor(1 + floor((
                       id - 1
                     ) / 6))),
              # cohort = factor(sample(0:4, units, replace = TRUE))),
              by = "id")
  
  baseline <- tibble(id = 1:units,
                     base = rnorm(units, 20, 5))
  
  df <- tibble(
    cohort = factor(1:5),
    effect = sample(2:10, 5),
    timing = sample(10:90, 5)
  ) |>
    left_join(df, ., by = "cohort") |>
    left_join(baseline, by = "id") |>
    mutate(Y = Y + base,
           timing = if_else(cohort == 1 & nevertreated, 0L, timing))
  
  df <- df |>
    mutate(D = if_else(t >= timing & timing != 0, 1, 0),
           Y = if_else(D == 1, Y + (t - timing) * effect, Y))
  
  
  df <- df |>
    mutate(Y = Y + rnorm(nrow(df), 5, 2))
  
  df
}

df <- create_df() |> 
  rename(ind_id = id) |> 
  mutate(ind_id = sample(1000:9999, nrow(df)))

create_df() |> 
  ggplot(aes(t, Y, colour = cohort, group = id)) +
  geom_line()


  
bacon_df <- bacon(
  Y ~ D,
  data = df,
  id_var = "cohort",
  time_var = "t"
)

# coef_bacon <- sum(bacon_df$estimate * bacon_df$weight)


bacon_coef(bacon_df)

bacon_df |> 
ggplot(aes(weight, estimate, shape = factor(type))) +
  geom_point() +
  geom_hline(yintercept = bacon_coef(bacon_df))


# de Chaisemartin and D’Haultfoeuille -------------------------------------

# library(DIDmultiplegt)

# did1 <- did_multiplegt(df, "Y", "id", "t", "D", cluster = "id", brep = 20,
#                        placebo = 10, dynamic = 10, parallel = TRUE)

# very very slow!

# Callaway and Sant'Anna 2021 ---------------------------------------------
library(tidyverse)
library(did)
set.seed(5565)


create_df() |>
  ggplot(aes(t, Y, colour = cohort, group = id)) +
  geom_line()

did2 <- df |>
  mutate(cohort = as.numeric(cohort)) |> 
  att_gt(
    yname = "Y",
    gname = "timing",
    idname = "cohort",
    tname = "t",
    xformla = ~ 1,
    data = _,
    est_method = "reg",
    control_group = "notyettreated",
    allow_unbalanced_panel = TRUE
  )

did2

ggdid(did2)

es <- aggte(did2, type = "dynamic")

summary(es)

ggdid(es)


group_eff <- aggte(did2, type = "group")

group_eff


# did2s -------------------------------------------------------------------
# Following Gardner (2021)
library(did2s)

did3 <- did2s(
  df,
  "Y",
  first_stage = ~ 0 | cohort + t,
  second_stage =  ~ i(D, ref = FALSE),
  treatment = "D",
  cluster_var = "cohort"
)

did3

mod3 <- create_df() |>
  mutate(rel_year = t - timing) |>
  did2s(
    "Y",
    first_stage = ~ 0 | cohort + t,
    second_stage =  ~ i(rel_year, keep = -10:10),
    treatment = "D",
    cluster_var = "cohort"
  )

tibble(
  t = 1:length(mod3$coefficients),
  coef = mod3$coefficients,
  se = mod3$se,
  ui = coef + se * qnorm(0.975),
  li = coef + se * qnorm(0.025)
)  |>
  ggplot(aes(t, coef)) +
  geom_point() +
  geom_linerange(aes(ymax = ui, ymin = li))
