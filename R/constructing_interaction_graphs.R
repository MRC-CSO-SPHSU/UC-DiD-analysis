library(docxtractr)
library(tidyverse)
library(patchwork)

subg_doc <- read_docx("data/ate_paper_subgroups.docx")

subg_tables <- subg_doc$tbls |> 
  imap(\(tbl, i) {
    
    docx_extract_tbl(subg_doc, i) |> 
      filter(row_number() < n())
    
  })

interaction_terms <- subg_tables |> 
  reduce(bind_rows) |> 
  janitor::clean_names() |> 
  filter(!is.na(interaction_term)) |> 
  separate(interaction_term, into = c("axis", "subgroup"), sep = ":") |> 
  select(outcome, axis, subgroup, effect_size, standard_error, x95_ci, subgroup_unweighted_n) |> 
  separate(x95_ci, into = c("li", "ul"), sep = ", ", convert = TRUE) |> 
  mutate(effect_size = as.numeric(effect_size),
         standard_error = as.numeric(standard_error),
         outcome = fct_inorder(outcome))


interaction_plots <- interaction_terms |>
  nest(results = -axis) |>
  mutate(gp = map2(results, axis, \(data, title) {
    
    effect_labs <- data |>
      mutate(label = glue::glue(
        "{effect} ({lower}, {upper})",
        effect = sprintf("%.2f", effect_size),
        lower = sprintf("%.2f", li),
        upper = sprintf("%.2f", ul)
      )) |> 
      ggplot(aes(1, fct_rev(outcome), label = label, colour = subgroup)) +
      geom_text(position = position_dodge(width = 0.5), fontface ="bold") +
      theme_void() +
      scale_colour_brewer(palette = "Set1") +
      theme(legend.position = "none")
    
    main_plot <- ggplot(data, aes(effect_size, fct_rev(outcome), colour = subgroup)) + 
      geom_point(position = position_dodge(width = 0.5)) + 
      geom_errorbar(aes(xmin = li, xmax = ul), width = 0.3, position = position_dodge(width = 0.5)) +
      geom_vline(xintercept = 0, linetype = "dashed") +
      scale_y_discrete("Well-being outcome") +
      scale_colour_brewer("", palette = "Set1", guide = guide_legend(reverse = TRUE)) +
      theme_minimal() +
      scale_x_continuous("Effect estimate (95%CI)") +
      theme(legend.position = "bottom", text = element_text(size = 12))
    
    main_plot + effect_labs + plot_layout(widths = c(5, 2)) +
      plot_annotation(title = glue::glue("Interaction effects by {title}"))
  }))

interaction_plots$gp[1]

interaction_plots$gp |> 
  iwalk(\(plot, number) ggsave(glue::glue("graphs/SPAplot{number}.png"), plot, width = 150, height = 120, units = "mm"))


ggsave(interaction_plots$gp[12][[1]], filename = ggsave("graphs/SPAplot12.png"), width = 150, height = 150, units = "mm")
