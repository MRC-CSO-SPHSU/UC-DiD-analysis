library(tidyverse)
library(tidymodels)
# library(SPHSUgraphs)
library(data.table)

# theme_set(theme_sphsu_light())


# practice data from APS --------------------------------------------------

# library(haven)
# aps18_19 <-
#   read_sav("data/UKDA-8510-spss/spss/spss24/apsp_a18m19_eul_pwta18.sav")
# aps18_19_dta <-
#   read_dta("data/UKDA-8197-stata/stata/stata11/apsp_a16m17_eul.dta")
# aps_16_hh_spss <-
#   read_sav("data/UKDA-8216-spss/spss/spss24/apsh_jd16_eul.sav")


# importing ukmod data ----------------------------------------------------------

input_parts <-
  tibble(
    file = file.path("data/ukmod_out", dir("data/ukmod_out")),
    year_policy = str_extract(file, "(?<=uk_)\\w*(?=\\.txt)")
  ) |>
  mutate(data = map(file, fread))


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

rm(input_parts)

full_pred_data <- full_pred_data |>
  group_by(year, idhh) |>
  mutate(children = sum(dag < 16)) |>
  ungroup()

# processing and matching data --------------------------------------------
# # code in progress for processing within Safe Lab
# aps_tidy <- aps18_19_dta |>
#   mutate(
#     age = AGE,
#     cit = if_else(NTNLTY12 == 926, "UK", "Other"),  # leave out as odd in UKMOD?
#     disab = fct_collapse(
#       as_factor(DISEA),
#       `Disabled` = "Equality Act Disabled",
#       `Not disabled` = c("Not Equality Act Disabled", "Does not apply", "No answer")
#     ),
#     employment = case_when(
#       INECAC05 %in% 1:4 ~ "Employed",
#       INECAC05 == 5 ~ "Unemployed",
#       INECAC05 %in% c(20, 31) ~ "Retired",
#       INECAC05 %in% c(15:16, 26:27) ~ "Sick or disabled",
#       INECAC05 %in% 12:33 ~ "Inactive",
#       TRUE ~ "Other"
#     ),
#     educ = case_when(
#       if_any(matches("QUAL_[1-9]$"), ~ .x == 1) ~ "Degree or College",
#       if_any(matches("QUAL_1[0-7]$"), ~ .x == 1) |
#         QUAL_23 == 1 | QUAL_29 == 1 | QUAL_30 == 1 ~ "Secondary",
#       if_any(matches("QUAL_(18|19|2[0-4])$"), ~ .x == 1) ~ "Secondary",
#       if_any(matches("QUAL_(2[5-9]|3[0-5])$"), ~ .x == 1) ~ "Tertiary",
#       TRUE ~ "None"
#     ),
#     gender = as_factor(SEX),
#     marsta = case_when(
#       MARSTA == 1 ~ "Single",
#       MARSTA %in% c(2, 6) ~ "Married",
#       MARSTA %in% c(3, 7) ~ "Separated",
#       MARSTA %in% c(4, 8) ~ "Divorced",
#       MARSTA %in% c(5, 9)  ~ "Widowed",
#       TRUE ~ "Single"
#     ),
#     emp_len = case_when(
#       INECAC05 > 4 ~ "Not in employment",
#       EMPMON < 12 ~ "Less than 12 months",
#       EMPMON < 24 ~ "Between 1 and 2 years",
#       EMPMON < 60 ~ "Between 2 and 5 years",
#       EMPMON < 120 ~ "Between 5 and 10 years",
#       EMPMON < 240 ~ "Between 10 and 20 years",
#       EMPMON >= 240 ~ "20 years or more",
#     ),
#     seeking = if_else(ILODEFR == 2, "Yes", "No"),
#     GRSSWK = if_else(GRSSWK < 0, 0, GRSSWK),
#     GRSSWK2 = if_else(GRSSWK2 < 0, 0, GRSSWK2),
#     income = 52 * (GRSSWK + GRSSWK2) / 12,
#     income = if_else(income > 3415, 3415, income),
#     income = if_else(is.na(income), 0, income),
#     i_0 = as.numeric(income == 0),
#     i_m = as.numeric(income == 3415),
#     i_l = income * (1 - i_m),
#     # i.e. if income is max then turn off the continuous
#     i_c = cut(
#       income,
#       c(0, 1, 500, 1000, 2000, 3000, 3500),
#       right = FALSE,
#       include.lowest = TRUE,
#       labels = c("0",
#                  "1-499",
#                  "500-999",
#                  "1000-1999",
#                  "2000-2999",
#                  "3000+")
#     ),
#     house_ten = fct_collapse(factor(TEN1), Mortgaged = "2", Outright = "1", Rented = c("3", "4"), Free = "5", Other = c("6", "-9", "-8")),
#     house_resp = fct_other(as_factor(HRPID), "Yes", other_level = "No"),
#     # Missing variable outside safe lab
#     # children = fct_other(factor(FDPCH16), c("0", "1"), other_level = "2+"),
#     # region = str_extract(NUTS102, "(?<=^UK)\\w"),
#     # Totally missing variable?
#     # caring = factor(as.numeric(NOLWM == 3), labels = c("No", "Yes"))
#   ) |> 
#   select(
#     # uc_income, lba_income, uc_receipt,
#     age, cit, disab, employment, educ, gender, marsta, emp_len, seeking, 
#     income, i_0, i_m, i_l, i_c, house_ten, house_resp
#     # idhh, year
#     # children, region, caring
#   )


ukmod_tidy <- full_pred_data |>
  mutate(
    age = dag,
    cit = if_else(dcz == 1, "UK", "Other"),  # leave out as odd in UKMOD?
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
      TRUE ~ "Other"
    ),
    educ = case_when(
      dec == 4 ~ "Degree or College",
      dec == 3 ~ "Secondary",
      dec == 2 ~ "Secondary",
      dec == 5 ~ "Tertiary",
      TRUE ~ "None"
    ),
    gender = factor(
      dgn,
      levels = c("0", "1"),
      labels = c("Female", "Male")
    ),
    marsta = factor(
      if_else(dms == 0, 1, dms),
      levels = 1:5,
      labels = c("Single", "Married", "Separated", "Divorced", "Widowed")
    ),
    region = if_else(drgn1 < 3, LETTERS[drgn1 + 2], LETTERS[drgn1 + 1]),
    emp_len = case_when(
      les %in% c(4:8) ~ "Not in employment",
      liwwh < 12 ~ "Less than 12 months",
      liwwh < 24 ~ "Between 1 and 2 years",
      liwwh < 60 ~ "Between 2 and 5 years",
      liwwh < 120 ~ "Between 5 and 10 years",
      liwwh < 240 ~ "Between 10 and 20 years",
      liwwh >= 240 ~ "20 years or more",
    ),
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
    house_ten = fct_collapse(factor(amrtn), Mortgaged = "1", Outright = "2", Rented = c("3", "4", "5"), Free = "6", Other = "7"),
    house_resp = factor(dhr, labels = c("No", "Yes")),
    caring = fct_collapse(factor(lcr01), Yes = c("1", "2","3"), No = 0)
  ) |> 
  select(
    year, idhh, uc_income, lba_income, uc_receipt,
    age, cit, disab, employment, educ, gender, marsta, region, emp_len, seeking, 
    children, income, i_0, i_m, i_l, i_c, house_ten, house_resp, caring
  )


# testing data ------------------------------------------------------------

# # testing for comparable income distributions
# 
# aps_inc |> select(i_c) |> mutate(source = "aps") |> 
#   bind_rows(ukmod_tidy |> select(i_c) |> mutate(source = "ukmod")) |> 
#   ggplot(aes(y = i_c)) +
#   geom_bar() +
#   facet_wrap(~source, scale = "free_x")
# 
# # testing for aps income dist
# 
# aps_inc |> 
#   ggplot(aes(GROSS99)) + geom_histogram()

# testing for linear effects of income on uc_income

# ukmod_tidy |> 
#   filter(i_l !=0) |> 
#   ggplot(aes(i_l, uc_income)) + geom_point() +
#   geom_smooth()
# 
# ukmod_tidy |> 
#   ggplot(aes(yem, uc_income)) + #geom_point()
#   geom_smooth()

# propose income be categorised:
# - 0-500
# - 500-1000
# - 1000-2000
# - 2000-3000
# - 3000+

# ukmod_tidy |> 
#   ggplot(aes(i_c)) +
#   geom_bar()
# 
# ukmod_tidy |> 
#   filter(i_l !=0) |> 
#   ggplot(aes(i_l)) +
#   geom_histogram()

# predict data from UKMOD -------------------------------------------------





model_data <- ukmod_tidy |> 
  group_by(year, idhh) |> 
  mutate(lba_income = sum(lba_income),
         uc_income = max(uc_income),
         uc_receipt = factor(max(uc_receipt)),
         n_hh_emp = sum(employment == "Employed"),
         n_hh_unemp = sum(employment == "Unemployed"),
         n_hh_inact = sum(employment == "Inactive")) |> 
  ungroup() |> 
  filter(age > 17 & age < 66) |> 
  mutate(
    p_hh_emp = if_else(employment == "Employed" & n_hh_emp > 0, 1, 0),
    n_hh_emp = fct_other(factor(n_hh_emp), c("0", "1"), other_level = "2+"),
    n_hh_unemp = fct_other(factor(n_hh_unemp), c("0", "1"), other_level = "2+"),
    n_hh_inact = fct_other(factor(n_hh_inact), c("0", "1"), other_level = "2+"))

data_split <- initial_split(model_data, prop = 0.8, strata = year)

train_data <- training(data_split)
test_data <- testing(data_split)

mc_train_set <- mc_cv(train_data)
cv_train_set <- vfold_cv(train_data, v = 10)

# linear regression - UC payment amount -----------------------------------

mod_lin_lm <- linear_reg() |>
  fit(
    uc_income ~ poly(age, 2) + 
      i_c +
      # poly(i_l, 4) + i_0 + i_m +
      region + disab + educ + gender + emp_len + seeking +
      house_ten + house_resp + caring + n_hh_emp + n_hh_unemp + n_hh_inact +
      children * employment * marsta,
      # employment * children * seeking * marsta,
      # employment * children * seeking * marsta,
      # employment * children * seeking * marsta,
    data = train_data
  )

mod_lin_lm |>
  extract_fit_engine() |>
  summary()

test_data |> 
  bind_cols(predict(mod_lin_lm, new_data = test_data)) |> 
  mutate(.pred = if_else(.pred < 0, 0, .pred)) |> 
  (\(x) {print(rmse(x, uc_income, .pred)); x})() |> 
  ggplot(aes(uc_income, .pred)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed")


mod_lin_xg <- boost_tree(min_n = 3) |> 
  set_mode("regression") |> 
  fit(
    uc_income ~ poly(age, 2) + 
      i_c +
      # poly(i_l, 4) + i_0 + i_m +
      region + disab + educ + gender + emp_len + seeking +
      house_ten + house_resp + caring + n_hh_emp + n_hh_unemp + n_hh_inact +
      children + employment + marsta,
    data = train_data
  )

test_data |> 
  bind_cols(predict(mod_lin_xg, new_data = test_data)) |> 
  mutate(.pred = if_else(.pred < 0, 0, .pred)) |> 
  (\(x) {print(rmse(x, uc_income, .pred)); x})() |> 
  ggplot(aes(uc_income, .pred)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed")


mod_lin_forest <- rand_forest(trees = 1000) |> 
  set_engine("ranger") |> 
  set_mode("regression") |> 
  fit(
    uc_income ~ age + 
      i_c +
      region + disab + educ + gender + emp_len + seeking +
      house_ten + house_resp + caring + n_hh_emp + n_hh_unemp + n_hh_inact +
      children + employment + marsta,
    data = train_data
  )
  
test_data |> 
  bind_cols(predict(mod_lin_forest, new_data = test_data)) |> 
  mutate(.pred = if_else(.pred < 0, 0, .pred)) |> 
  (\(x) {print(rmse(x, uc_income, .pred)); x})() |> 
  ggplot(aes(uc_income, .pred)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed")


# probability of UC receipt -----------------------------------------------


## XG boost ------------------------

mod_class_xg <- boost_tree(tree_depth = 4, trees = 1000) |>
  set_engine("xgboost") |>
  set_mode("classification")

recipie_class_xg <- workflow() |>
  add_model(mod_class_xg) |>
  add_formula(
    uc_receipt ~ age +
      i_c +
      region + disab + educ + gender + emp_len + seeking +
      house_ten + house_resp + caring + n_hh_emp + n_hh_unemp + n_hh_inact +
      children + employment + marsta
  )

fit_class_xg <- recipie_class_xg |>
  fit(data = train_data)

pred_class_xg <- test_data |> 
  bind_cols(predict(fit_class_xg, new_data = test_data, type = "prob")) |> 
  mutate(.pred_class = factor(as.numeric(.pred_1 > 0.5), levels = c("1", "0")))
  # mutate(predict(fit_class_xg, new_data = test_data))

pred_class_xg |> 
  (\(x) {print(roc_auc(x, uc_receipt, .pred_1)); x})() |> 
  roc_curve(uc_receipt, .pred_1) |> 
  autoplot()

  
pred_class_xg |> 
  (\(x) {print(conf_mat(x, uc_receipt, .pred_class)); x})() |> 
  ggplot(aes(uc_receipt, fill = .pred_class)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::label_percent())

pred_class_xg |> 
  select(.pred_1, uc_receipt) |> 
  crossing(prob = c(0.1, 0.25, 0.5, 0.75, 0.9)) |> 
  mutate(.pred_class = factor(.pred_1 > prob, labels = c("No UC", "UC")),
         uc_receipt = factor(uc_receipt, levels = c("1", "0"), labels = c("UC", "No UC"))) |> 
  ggplot(aes(uc_receipt, fill = .pred_class)) +
  geom_bar(position = "fill") +
  scale_y_continuous("Perc predicted", labels = scales::label_percent()) +
  facet_wrap(~ prob, nrow = 1)

### Tuning grid? -----------------------

tune_class_xg <- boost_tree(trees = tune(), min_n = tune(), tree_depth = tune()) |>
  set_engine("xgboost") |>
  set_mode("classification")

recipie_tune_class_xg <- workflow() |> 
  add_model(tune_class_xg) |>
  add_formula(uc_receipt ~ age +
    i_c +
    region + disab + educ + gender + emp_len + seeking +
    house_ten + house_resp + caring + n_hh_emp + n_hh_unemp + n_hh_inact +
    children + employment + marsta)


xg_tune_grid <- tune_class_xg |> 
  extract_parameter_set_dials() |> 
  grid_regular(levels = 2)

tune_out_class_xg <- tune::tune_grid(recipie_tune_class_xg, resamples = cv_train_set, grid = xg_tune_grid)

fit_resamples(recipie_tune_class_xg, cv_train_set)
### with MC resampling CV ---------------------------------------------


cv_class_xg <- fit_resamples(recipie_class_xg, mc_train_set, control = control_resamples(save_pred = TRUE))

collect_metrics(cv_class_xg)

collect_predictions(cv_class_xg) |> 
  select(.pred_1, uc_receipt) |> 
  crossing(prob = c(0.1, 0.25, 0.5, 0.75, 0.9)) |> 
  mutate(.pred_class = factor(.pred_1 > prob, labels = c("No UC", "UC"))) |> 
  ggplot(aes(uc_receipt, fill = .pred_class)) +
  geom_bar(position = "fill") +
  scale_y_continuous("Perc predicted", labels = scales::label_percent()) +
  facet_wrap(~ prob, nrow = 1)


collect_predictions(cv_class_xg) |> 
  select(.pred_1, uc_receipt) |> 
  crossing(prob = seq(0.05, 0.95, 0.05)) |> 
  mutate(.pred_class = factor(.pred_1 > prob, labels = c("No UC", "UC")),
         uc_receipt = factor(uc_receipt, levels = c("0", "1"), labels = c("No UC", "UC"))) |> 
  group_by(uc_receipt, prob) |> 
  summarise(Pred_receive = sum(.pred_class == "UC")/n()) |> 
  ggplot(aes(prob, Pred_receive, colour = uc_receipt)) +
  geom_line()

## logistic classification ----------------------------------

mod_class_log <- logistic_reg() |> 
  set_engine("glm") |> 
  set_mode("classification")

recipie_class_log <- workflow() |>
  add_model(mod_class_log) |>
  add_formula(
    uc_receipt ~ poly(age, 2) +
      i_c +
      # poly(i_l, 4) + i_0 + i_m +
      region + disab + educ + gender + emp_len + seeking +
      house_ten + house_resp + caring + n_hh_emp + n_hh_unemp + n_hh_inact +
      children * employment * marsta
  )

fit_class_log <- 
  fit(
    recipie_class_log,
    data = train_data
  )

fit_class_log

pred_class_log <- test_data |> 
  bind_cols(predict(fit_class_log, new_data = test_data, type = "prob")) |> 
  mutate(.pred_class = factor(as.numeric(.pred_1 > 0.5), levels = c("1", "0")))

pred_class_log |> 
  (\(x) {print(roc_auc(x, uc_receipt, .pred_1)); x})() |> 
  roc_curve(uc_receipt, .pred_1) |> 
  autoplot()

pred_class_log |> 
  (\(x) {print(conf_mat(x, uc_receipt, .pred_class)); x})() |> 
  ggplot(aes(uc_receipt, fill = .pred_class)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::label_percent())

pred_class_log |> 
  select(.pred_1, uc_receipt) |> 
  crossing(prob = c(0.1, 0.25, 0.5, 0.75, 0.9)) |> 
  mutate(.pred_class = factor(.pred_1 > prob, labels = c("No UC", "UC")),
         uc_receipt = factor(uc_receipt, levels = c("0", "1"), labels = c("No UC", "UC"))) |> 
  ggplot(aes(uc_receipt, fill = .pred_class)) +
  geom_bar(position = "fill") +
  scale_y_continuous("Perc predicted", labels = scales::label_percent()) +
  facet_wrap(~ prob, nrow = 1)

### with Monte Carlo resampling for cross-validation ----------------------

cv_class_log <- recipie_class_log |> fit_resamples(mc_train_set, control = control_resamples(save_pred = TRUE))

collect_metrics(cv_class_log)

collect_predictions(cv_class_log) |> 
  select(.pred_1, uc_receipt) |> 
  crossing(prob = c(0.1, 0.25, 0.5, 0.75, 0.9)) |> 
  mutate(.pred_class = factor(.pred_1 > prob, labels = c("No UC", "UC"))) |> 
  ggplot(aes(uc_receipt, fill = .pred_class)) +
  geom_bar(position = "fill") +
  scale_y_continuous("Perc predicted", labels = scales::label_percent()) +
  facet_wrap(~ prob, nrow = 1)


collect_predictions(cv_class_log) |> 
  select(.pred_1, uc_receipt) |> 
  crossing(prob = seq(0.05, 0.95, 0.05)) |> 
  mutate(.pred_class = factor(.pred_1 > prob, labels = c("No UC", "UC")),
         uc_receipt = factor(uc_receipt, levels = c("0", "1"), labels = c("No UC", "UC"))) |> 
  group_by(uc_receipt, prob) |> 
  summarise(Pred_receive = sum(.pred_class == "UC")/n()) |> 
  ggplot(aes(prob, Pred_receive, colour = uc_receipt)) +
  geom_line()  
