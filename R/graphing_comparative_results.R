library(tidyverse)

tribble(
  ~ metric, ~text,
  "Life Satisfaction", "-0.066 point drop in life satisfaction (-0.087 to -0.044)",
  "Happiness", "-0.068 point drop in happiness (-0.094 to -0.042)",
  "Anxiety", "0.065 point increase in anxiety (0.031 to 0.098)",
  "Life Worthwhile", "-0.074 point drop in life being worthwhile (-0.095 to -0.053)",
  "Life Satisfaction", "-0.12 point drop in life satisfaction (-0.15 to -0.09)",
  "Happiness", "-0.11 point drop in happiness (-0.15 to -0.07)",
  "Anxiety", "0.12 point increase in anxiety (0.07 to 0.17)",
  "Life Worthwhile", "-0.12 point drop in life being worthwhile (-0.15 to -0.09)"
) |> 
  mutate(
    estimate = str_extract(text, "-?0\\.\\d*") |> as.numeric(),
    cl = str_extract(text, "(?<=\\()-?0\\.\\d*") |> as.numeric(),
    cu = str_extract(text, "(?<=to )-?0\\.\\d*") |> as.numeric(),
    time = c(rep("Within 1y", 4), rep(">20% threshold", 4))
  ) |> 
  ggplot(aes(estimate, metric, colour = time)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_linerange(aes(xmin = cl, xmax = cu), position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0) +
  SPHSUgraphs::theme_sphsu_light() +
  SPHSUgraphs::scale_colour_sphsu()
