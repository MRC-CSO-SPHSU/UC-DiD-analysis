library(tidyverse)
library(did2s)

df_het |> 
  ggplot(aes(year, dep_var, colour= group)) +
   stat_summary(geom = "line") +
   stat_summary()
  
mod_df <- tibble(state = 1:40, 
       l = if_else(state %in% sample(40, 20), 1, 0)) |> 
  left_join(df_het, by = "state") |> 
  mutate(dep_var = if_else(l == 1 & treat, dep_var +0.5 , dep_var))

dids <- did2s(mod_df,
              yname = "dep_var",
              treatment = "treat",
              first_stage = ~ 0 | unit + year,
              second_stage = ~ i(rel_year, ref = c(-1, Inf)),
              cluster_var = "unit")

iplot(dids)

feols(dep_var ~ i(rel_year, ref = c(-1, Inf))| unit + year, fsplit = ~ l , data = mod_df)

lm(dep_var ~ i(rel_year, ref = c(-1, Inf)) * l + unit + year, data = mod_df)

dids |> 
  ggplot(aes(rel))

dd1 <- event_study(mod_df |> filter(l == 1), "dep_var", idname = "unit", 
            tname = "year", gname = "g", estimator = "did")

dd0 <- event_study(mod_df |> filter(l == 0), "dep_var", idname = "unit", 
                   tname = "year", gname = "g", estimator = "did")

library(patchwork)
library(bacon)

plot_event_study(dd1) + plot_event_study(dd0)
