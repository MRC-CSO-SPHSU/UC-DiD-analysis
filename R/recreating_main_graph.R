library(tidyverse)
library(docxtractr)
library(patchwork)

main_doc <- read_docx("data/ate_paper_outputs.docx")

main_tables <- main_doc$tbls |> 
  imap(\(tbl, i) {
    
    docx_extract_tbl(main_doc, i) |> 
      janitor::clean_names() |> 
      filter(row_number()< n())
    
  })


life_sat <- main_tables[[7]]
happy <- main_tables[[13]]
life_worth <- main_tables[[19]]
anxiety <- main_tables[[25]]

plotting_data <- list(
  "Life Satisfaction" = life_sat,
  "Happiness" = happy,
  "Life Worthwhile" = life_worth,
  "Anxiety" = anxiety
) |> 
  imap(~mutate(.x, outcome = .y)) |> 
  reduce(bind_rows) |> 
  separate_wider_delim(x95_ci, delim = ", ", names = c("ll", "ul")) |> 
  mutate(across(estimate:ul, as.numeric), outcome = fct_inorder(outcome) |> fct_rev())

effect_labs <- plotting_data |>
  mutate(label = glue::glue(
    "{effect} ({lower}, {upper})",
    effect = sprintf("%.2f", estimate),
    lower = sprintf("%.2f", ll),
    upper = sprintf("%.2f", ul)
  )) |> 
  ggplot(aes(1, outcome, label = label)) +
  geom_text(position = position_dodge(width = 0.5), fontface ="bold") +
  theme_void() +
  theme(legend.position = "none")

main_plot <- plotting_data |> 
  ggplot(aes(estimate, outcome)) + 
  geom_point(position = position_dodge(width = 0.5)) + 
  geom_errorbar(aes(xmin = ll, xmax = ul), width = 0.3, position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_y_discrete("Well-being outcome") +
  theme_minimal() +
  scale_x_continuous("Effect estimate (95%CI)") +
  theme(legend.position = "bottom", text = element_text(size = 12))

main_plot + effect_labs + plot_layout(widths = c(5, 2)) +
  plot_annotation(caption = "Unweighted N = 245,658")

ggsave("graphs/main_outcome.svg", height = 10.7*1.2, width = 17.2*1.2, units = "cm")
ggsave("graphs/main_outcome.tiff", height = 10.7*1.2, width = 17.2*1.2, units = "cm", compression = "lzw", dpi = 600)



# event study plots -------------------------------------------------------


life_sat <- main_tables[[8]]
happy <- main_tables[[14]]
life_worth <- main_tables[[20]]
anxiety <- main_tables[[26]]

plotting_data <- list(
  "Life Satisfaction" = life_sat,
  "Happiness" = happy,
  "Life Worthwhile" = life_worth,
  "Anxiety" = anxiety
) |> 
  imap(~mutate(.x, outcome = .y)) |> 
  reduce(bind_rows) |> 
  separate_wider_delim(x95_ci, delim = ", ", names = c("ll", "ul")) |> 
  mutate(across(-outcome, as.numeric), outcome = fct_inorder(outcome) |> fct_rev())


plotting_data |> 
  ggplot(aes(quarter_relative_to_uc_rollout, estimate)) +
  geom_vline(xintercept = -0.5, linetype = "dashed", colour = "red") +
  geom_hline(yintercept = 0, colour = "grey") +
  geom_point() +
  geom_linerange(aes(ymin = ll, ymax = ul)) +
  facet_wrap(~outcome, scales = "free_y") +
  scale_x_continuous("Quarter relative to UC rollout") +
  theme_minimal() +
  scale_y_continuous("Effect estimate (95%CI)") +
  labs(caption = "Unweighted N = 245,658")

ggsave("graphs/event_study_plots.svg", height = 15*1.2, width = 19*1.2, units = "cm")
ggsave("graphs/event_study_plots.tiff", height = 15*1.2, width = 19*1.2, units = "cm", compression = "lzw", dpi = 600)
