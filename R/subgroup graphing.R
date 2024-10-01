library(tidyverse)
library(officer)
library(docxtractr)
library(SPHSUgraphs)
library(patchwork)

theme_set(theme_sphsu_light())

subgroup_doc <- read_docx("data/ate_subgroups_differencing_effects.docx")

tables <- map(1:24, ~docx_extract_tbl(subgroup_doc, tbl_number = .x))

end_table <- docx_extract_tbl(subgroup_doc, tbl_number = 25)

library(readxl)

end_table <- read_xlsx("data/concat_subgroups.xlsx", skip = 2, col_types = c(
  "text", 
  "text",
  "numeric",
  "text",
  "numeric",
  "text",
  "numeric",
  "text",
  "numeric",
  "text"
  ))

subgroup_interactions <- end_table |> 
  filter(!is.na(Comparison)) |> 
  pivot_longer(
    -c(Subgroup, Comparison), 
    names_to = c("Metric", ".value"),
    names_sep = "_"
    ) |> 
  separate_wider_delim(ends_with("95% CI"), " to ", names = c("ll", "ul")) |> 
  mutate(across(c(ll, ul), as.numeric))

subgroup_interactions |> 
  mutate(se = (ul - ll)/(2 * 1.96)) |> 
  rowwise() |> 
  mutate(p = 2 * pnorm(-abs(estimate), sd = se, lower.tail = TRUE),
         signif = p < 0.05) |> 
  ggplot(aes(Metric, Comparison, fill = signif)) +
  geom_tile()

subgroup_interactions |> 
  mutate(Metric = fct_inorder(Metric) |> fct_rev()) |> 
  nest(data = -Subgroup) |> 
  mutate(graph = map2(data, Subgroup, \(df, subg) {
    
    df |> 
      ggplot(aes(estimate, Metric, colour = Comparison)) +
      geom_point(position = position_dodge(width = 0.5)) +
      geom_errorbar(aes(xmin = ll, xmax = ul), width = 0.2, 
                    position = position_dodge(width = 0.5)) +
      geom_vline(xintercept = 0, linetype = "dotted") +
      scale_color_brewer(name = subg, palette = "Set1") +
        facet_wrap(~Comparison) +
      theme_minimal()
    
  })) |> 
  group_by(Subgroup) |> 
  group_walk(\(df, subg) {
    
    ggsave(plot = df$graph[[1]], filename = glue::glue("graphs/subg_{subg}.png"))
    
  })
