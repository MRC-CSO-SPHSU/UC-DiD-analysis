library(tidyverse)
library(tidymodels)
library(furrr)
library(data.table)

# importing ukmod data ---------------------------------------------------------
import_ukmod_data <- function() {
  input_parts <-
    tibble(
      file = file.path("data/ukmod_out", dir("data/ukmod_out")),
      year_policy = str_extract(file, "(?<=uk_)\\w*(?=\\.txt)")
    ) |>
    mutate(data = future_map(file, fread))
  
  
  input_parts |>
    select(-file) |>
    separate(year_policy,
             into = c("year", "policy"),
             sep = "_") |>
    mutate(
      data = map(data, summarise, across(starts_with("b") &
                                           ends_with("_s"), sum)),
      data = map(
        data,
        pivot_longer,
        everything(),
        names_to = "benefit",
        values_to = "val"
      )
    ) |>
    unnest(data) |>
    group_by(benefit, policy) |>
    summarise(val = sum(val), .groups = "drop_last") |>
    mutate(diff_val = diff(val)) |>
    filter(abs(diff_val) > 1000) |>
    arrange(abs(diff_val)) |>
    ggplot(aes(val, fct_inorder(benefit))) +
    geom_bar(stat = "identity") +
    facet_wrap(~ policy)
  
  full_pred_data <- input_parts |>
    select(-file) |>
    separate(year_policy,
             into = c("year", "policy"),
             sep = "_") |>
    pivot_wider(names_from = policy, values_from = data) |>
    mutate(
      UCAon = map(
        UCAon,
        transmute,
        uc_income = bho_s + bmu_s + boamt_s + boamtmm_s + boamtxp_s + bsauc_s - brduc_s,
        uc_receipt = as.numeric(bsauc_s - brduc_s > 0),
        idperson = idperson
      ),
      LBAon = map(
        LBAon,
        mutate,
        lba_income = bho_s + bmu_s + boamt_s + boamtmm_s + boamtxp_s + bfamt_s + bsadi_s + bsa_s + bwkmt_s - brd_s
      )
    ) |>
    mutate(comb_data = map2(LBAon, UCAon, left_join, by = "idperson"),
           .keep = "unused") |>
    unnest(comb_data) |>
    arrange(year, idperson)
  
  
  full_pred_data <- full_pred_data |>
    group_by(year, idhh) |>
    mutate(children = sum(dag < 16)) |>
    ungroup()
  
  ukmod_tidy <- full_pred_data |>
    mutate(
      age = dag,
      age_2 = age ^ 2,
      cit = if_else(dcz == 1, "UK", "Other"),
      # leave out as odd in UKMOD?
      disab = factor(
        ddi,
        levels = c("0", "1"),
        labels = c("Not disabled", "Disabled")
      ),
      employment = case_when(
        les %in% 1:3 ~ "Employed",
        les == 5 ~ "Unemployed",
        les %in% 6:7 ~ "Inactive",
        les == 4 ~ "Retired",
        les == 8 ~ "Sick or disabled",
        TRUE ~ "Other"),
      emp_len = case_when(
        les == 5 ~ "Unemployed",
        les %in% 6:7 ~ "Inactive",
        les == 4 ~ "Retired",
        les == 8 ~ "Sick or disabled",
        liwwh < 12 ~ "Less than 12 months",
        liwwh < 24 ~ "Between 1 and 2 years",
        liwwh < 60 ~ "Between 2 and 5 years",
        liwwh < 120 ~ "Between 5 and 10 years",
        liwwh < 240 ~ "Between 10 and 20 years",
        liwwh >= 240 ~ "20 years or more",
      ),
      student = if_else(les == 6, 1L, 0L),
      educ = case_when(
        deh == 4 ~ "Degree or College",
        deh == 3 ~ "Secondary",
        deh == 2 ~ "Lower Secondary",
        deh == 5 ~ "Tertiary",
        TRUE ~ "None"
      ),
      gender = factor(
        dgn,
        levels = c("0", "1"),
        labels = c("Female", "Male")
      ),
      marsta = factor(
        if_else(dms == 0, 1L, dms),
        levels = 1:5,
        labels = c("Single", "Married", "Separated", "Divorced", "Widowed")
      ),
      region = if_else(drgn1 < 3, LETTERS[drgn1 + 2], LETTERS[drgn1 + 1]),
      seeking = factor(
        lowas,
        levels = c("0", "1"),
        labels = c("No", "Yes")
      ),
      children = fct_other(factor(children), c("0", "1"), other_level = "2+"),
      income = if_else(yem > 3415, 3415, yem),
      i_0 = as.numeric(income == 0),
      i_m = as.numeric(income == 3415),
      i_l = income * (1 - i_m),
      # i.e. if income is max then turn off the continuous
      i_c = cut(
        income,
        c(0, 1, 500, 1000, 2000, 3000, 3500),
        right = FALSE,
        include.lowest = TRUE,
        labels = c("0",
                   "1-499",
                   "500-999",
                   "1000-1999",
                   "2000-2999",
                   "3000+")
      ),
      house_ten = fct_collapse(
        factor(amrtn),
        Mortgaged = "1",
        Outright = "2",
        Rented = c("3", "4", "5"),
        Free = "6",
        Other = "7"
      ),
      house_resp = factor(dhr, labels = c("No", "Yes")),
      caring = factor(
        full_pred_data$lcr01 == 0,
        levels = c(TRUE, FALSE),
        labels = c("No", "Yes")
      )
    ) |>
    select(
      year,
      idhh,
      uc_income,
      lba_income,
      uc_receipt,
      age,
      cit,
      disab,
      employment,
      educ,
      gender,
      marsta,
      region,
      emp_len,
      seeking,
      student,
      children,
      income,
      i_0,
      i_m,
      i_l,
      i_c,
      house_ten,
      house_resp,
      caring
    )
  
  ukmod_tidy
}




# importing aps data ------------------------------------------------------

import_aps_data <- function(aps_data) {
  aps_tidy <- aps_data |>
    mutate(
      age = AGE,
      age_2 = age ^ 2,
      cit = if_else(NTNLTY12 == 926, "UK", "Other"),
      # leave out as odd in UKMOD?
      disab = fct_collapse(
        as_factor(DISEA),
        `Disabled` = "Equality Act Disabled",
        `Not disabled` = c("Not Equality Act Disabled", "Does not apply", "No answer")
      ),
      employment = case_when(
        INECAC05 %in% 1:4 ~ "Employed",
        INECAC05 == 5 ~ "Unemployed",
        INECAC05 %in% 12:33 ~ "Inactive",
        INECAC05 %in% c(20, 31) ~ "Retired",
        INECAC05 %in% c(15:16, 26:27) ~ "Sick or disabled",
        TRUE ~ "Other"
      ),
      student = if_else(STUCUR == 1, 1L, 0L),
      educ = case_when(
        if_any(matches("QUAL_[1-9]$"), ~ .x == 1) ~ "Degree or College",
        if_any(matches("QUAL_1[0-7]$"), ~ .x == 1) |
          QUAL_23 == 1 | QUAL_29 == 1 | QUAL_30 == 1 ~ "Secondary",
        if_any(matches("QUAL_(18|19|2[0-4])$"), ~ .x == 1) ~ "Secondary",
        if_any(matches("QUAL_(2[5-9]|3[0-5])$"), ~ .x == 1) ~ "Tertiary",
        TRUE ~ "None"
      ),
      gender = as_factor(SEX),
      marsta = case_when(
        MARSTA == 1 ~ "Single",
        MARSTA %in% c(2, 6) ~ "Married",
        MARSTA %in% c(3, 7) ~ "Separated",
        MARSTA %in% c(4, 8) ~ "Divorced",
        MARSTA %in% c(5, 9)  ~ "Widowed",
        TRUE ~ "Single"
      ),
      emp_len = case_when(
        INECAC05 == 5 ~ "Unemployed",
        INECAC05 %in% 12:33 ~ "Inactive",
        INECAC05 %in% c(20, 31) ~ "Retired",
        INECAC05 %in% c(15:16, 26:27) ~ "Sick or disabled",
        EMPMON < 12 ~ "Less than 12 months",
        EMPMON < 24 ~ "Between 1 and 2 years",
        EMPMON < 60 ~ "Between 2 and 5 years",
        EMPMON < 120 ~ "Between 5 and 10 years",
        EMPMON < 240 ~ "Between 10 and 20 years",
        EMPMON >= 240 ~ "20 years or more",
      ),
      seeking = if_else(ILODEFR == 2, "Yes", "No"),
      GRSSWK = if_else(GRSSWK < 0, 0, GRSSWK),
      GRSSWK2 = if_else(GRSSWK2 < 0, 0, GRSSWK2),
      income = 52 * (GRSSWK + GRSSWK2) / 12,
      income = if_else(income > 3415, 3415, income),
      income = if_else(is.na(income), 0, income),
      i_0 = as.numeric(income == 0),
      i_m = as.numeric(income == 3415),
      i_l = income * (1 - i_m),
      # i.e. if income is max then turn off the continuous
      i_c = cut(
        income,
        c(0, 1, 500, 1000, 2000, 3000, 3500),
        right = FALSE,
        include.lowest = TRUE,
        labels = c("0",
                   "1-499",
                   "500-999",
                   "1000-1999",
                   "2000-2999",
                   "3000+")
      ),
      house_ten = fct_collapse(
        factor(TEN1),
        Mortgaged = "2",
        Outright = "1",
        Rented = c("3", "4"),
        Free = "5",
        Other = c("6", "-9", "-8")
      ),
      house_resp = fct_other(as_factor(HRPID), "Yes", other_level = "No"),
      # Missing variable outside safe lab
      # children = fct_other(factor(FDPCH16), c("0", "1"), other_level = "2+"),
      # region = str_extract(NUTS102, "(?<=^UK)\\w"),
      # Totally missing variable?
      # caring = factor(as.numeric(NOLWM == 3), labels = c("No", "Yes"))
    ) |>
    select(
      # uc_income, lba_income, uc_receipt,
      age,
      cit,
      disab,
      employment,
      educ,
      gender,
      marsta,
      emp_len,
      seeking,
      income,
      i_0,
      i_m,
      i_l,
      i_c,
      house_ten,
      house_resp
      # idhh, year
      # children, region, caring
    )
  
  aps_tidy
}

process_aps_data <- function(aps_data) {
  recipe_class_log <- recipe(
    uc_receipt ~ .,
    data = aps_data
  ) |> 
    step_interact(
      # ~ starts_with('gender_'):starts_with('children_') + starts_with('gender_'):starts_with('children_'):starts_with('emp_len_') + starts_with('children_'):starts_with('emp_len_') + student:starts_with('children_') + student:starts_with('caring_') + starts_with('n_hh_emp_'):starts_with('children_') + starts_with('n_hh_unemp_'):starts_with('children_') + starts_with('n_hh_inact_'):starts_with('children_') + starts_with('n_hh_emp_'):starts_with('caring_') + starts_with('n_hh_unemp_'):starts_with('caring_') + starts_with('n_hh_inact_'):starts_with('caring_') + starts_with('marsta_')*starts_with('gender_')*starts_with('children_')
      ~ starts_with('gender_'):starts_with('children_') + starts_with('gender_'):starts_with('children_'):starts_with('emp_len_') + starts_with('children_'):starts_with('emp_len_') + student:starts_with('children_') + student:starts_with('caring_') + starts_with('marsta_')*starts_with('gender_')*starts_with('children_')
    )
  
  recipe_class_log |> 
    prep() |> 
    bake(aps_data) 
}
