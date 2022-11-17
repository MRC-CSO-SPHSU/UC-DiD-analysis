# importing and preparing dataset ----------------------------------------------

source("R/prediction_data_import.R")

ukmod_tidy <- import_ukmod_data()


# pre-processing manually -------------------------------------------------

tidy_stage1 <- ukmod_tidy |> 
  group_by(year, idhh) |> 
  mutate(lba_income = sum(lba_income),
         uc_income = max(uc_income),
         uc_receipt = max(uc_receipt),
         n_hh_emp = sum(employment == "Employed"),
         n_hh_unemp = sum(employment == "Unemployed"),
         n_hh_inact = sum(employment == "Inactive")) |> 
  ungroup()

model_data <- tidy_stage1 |> 
  filter(age > 17 & age < 66) |> 
  select(-idhh, -i_0, -i_m, -i_l, - income, -uc_income, -lba_income) |> 
  mutate(
    year = as.integer(year),
    p_hh_emp = if_else(employment == "Employed" & n_hh_emp > 0, 1, 0),
    n_hh_emp = fct_other(factor(n_hh_emp), c("0", "1"), other_level = "2+"),
    n_hh_unemp = fct_other(factor(n_hh_unemp), c("0", "1"), other_level = "2+"),
    n_hh_inact = fct_other(factor(n_hh_inact), c("0", "1"), other_level = "2+"),
    age_2 = age ^ 2) |> 
  dummy_cols(remove_first_dummy = TRUE, remove_selected_columns = TRUE) |> 
  janitor::clean_names() |> 
  mutate(uc_receipt = factor(uc_receipt, levels = c("Yes" = 1, "No" = 0)))
  

# predict data from UKMOD -------------------------------------------------

data_split <- initial_split(model_data, prop = 0.8, strata = year)

train_data <- training(data_split) |> select(-year)
test_data <- testing(data_split) |> select(-year)

mc_train_set <- mc_cv(train_data)
cv_train_set <- vfold_cv(train_data, v = 10)

# Running logistic model -------------------------------------------------------

mod_class_log <- logistic_reg() |> 
  set_engine("glm") |> 
  set_mode("classification")


recipe_class_log <- recipe(
  uc_receipt ~ .,
  data = train_data
) #|> 
  # step_interact(
  #   ~ starts_with('gender_'):starts_with('children_') + starts_with('gender_'):starts_with('children_'):starts_with('employment_') + starts_with('children_'):starts_with('employment_') + student:starts_with('children_') + student:starts_with('caring_') + starts_with('marsta_'):starts_with('employment_') + starts_with('n_hh_emp_'):starts_with('children_') + starts_with('n_hh_unemp_'):starts_with('children_') + starts_with('n_hh_inact_'):starts_with('children_') + starts_with('n_hh_emp_'):starts_with('caring_') + starts_with('n_hh_unemp_'):starts_with('caring_') + starts_with('n_hh_inact_'):starts_with('caring_') + starts_with('marsta_')*starts_with('gender_')*starts_with('children_')
  # )


wf_class_log <- workflow() |>
  add_model(mod_class_log) |>
  add_recipe(recipe_class_log)

fit_class_log <- 
  fit(
    wf_class_log,
    data = train_data
  )

pred_class_log <- test_data |> 
  bind_cols(predict(fit_class_log, new_data = test_data, type = "prob")) |> 
  mutate(.pred_class = factor(as.numeric(.pred_1 > 0.5), levels = c("1", "0")))

pred_class_log |> 
  (\(x) {print(roc_auc(x, uc_receipt, .pred_1)); x})() |> 
  roc_curve(uc_receipt, .pred_1) |> 
  autoplot()

tidy(fit_class_log) |> 
  filter(is.na(estimate))

intercept <- tidy(fit_class_log) |>  head(1) |> pull(estimate)
# library(tidypredict)

fit_class_log |> extract_fit_engine()

# testing calculation -----------------------------------------------------

pred_class_log |> 
  mutate(id = row_number()) |> 
  select(- uc_receipt, -starts_with(".pred")) |> 
  pivot_longer(- id, names_to = "term", values_to = "val") |> 
  left_join(tidy(fit_class_log), by = "term") |>
  mutate(pred_val = val * estimate) |> 
  group_by(id) |> 
  summarise(pred_val = sum(pred_val, na.rm = TRUE) + intercept) |> 
  mutate(prob = 1 - (exp(pred_val))/(1 + exp(pred_val))) |> 
  bind_cols(.pred_1 = pred_class_log$.pred_1) |> 
  filter(.pred_1 != prob)
