library(future)

# importing and preparing dataset ----------------------------------------------
plan(multisession, workers = 4)
source("R/prediction_data_import.R")

ukmod_tidy <- import_ukmod_data()

plan(sequential)



# linear regression on benefit receipt difference -------------------------

tidy_stage1 <- ukmod_tidy |> 
  group_by(year, idhh) |> 
  mutate(lba_income = sum(lba_income),
         uc_income = max(uc_income),
         uc_receipt = max(uc_receipt),
         benefit_change = uc_income - lba_income,
         n_hh_emp = sum(employment == "Employed"),
         n_hh_unemp = sum(employment == "Unemployed"),
         n_hh_inact = sum(employment == "Inactive")) |> 
  ungroup()

data_ben_diff <- tidy_stage1 |> 
  filter(age > 17 & age < 66) |> 
  select(-idhh, -i_0, -i_m, -i_l, - income, -uc_income, -lba_income, -employment) |> 
  mutate(
    year = as.integer(year),
    # p_hh_emp = if_else(employment == "Employed" & n_hh_emp > 0, 1, 0),
    n_hh_emp = fct_other(factor(n_hh_emp), c("0", "1"), other_level = "2+"),
    n_hh_unemp = fct_other(factor(n_hh_unemp), c("0", "1"), other_level = "2+"),
    n_hh_inact = fct_other(factor(n_hh_inact), c("0", "1"), other_level = "2+")
  ) |> 
  fastDummies::dummy_cols(remove_first_dummy = TRUE, remove_selected_columns = TRUE) |> 
  janitor::clean_names() |> 
  # select(-starts_with("n_hh")) |>
  mutate(uc_receipt = factor(uc_receipt, levels = 1:0, labels = c("Yes", "No")))


data_split <- initial_split(data_ben_diff, prop = 0.8, strata = year)

train_data <- training(data_split) |> select(-year)
test_data <- testing(data_split) |> select(-year)

mc_train_set <- mc_cv(train_data)
cv_train_set <- vfold_cv(train_data, v = 10)


rec_bd <- recipe(benefit_change ~ .,
                 data = train_data) |>
  step_interact(
    ~ starts_with('gender_'):starts_with('children_') + 
      starts_with('gender_'):starts_with('children_'):starts_with('emp_len_') + 
      starts_with('children_'):starts_with('emp_len_') + 
      student:starts_with('children_') + student:starts_with('caring_') + 
      starts_with('marsta_') * starts_with('gender_') * starts_with('children_')
  )

mod_diff_knn <- nearest_neighbor() |>
  set_mode("regression")

wf_diff_knn <- workflow() |>
  add_recipe(rec_bd) |>
  add_model(mod_diff_knn)

fit_diff_knn <- fit(wf_diff_knn,
                    data = train_data)

nearest_neighbor("regression") |> 
  fit(
    benefit_change ~ .,
    data = train_data
  )

pred_out <- test_data |> 
  bind_cols(predict(fit_diff_knn, new_data = test_data))

pred_out |> 
  (\(x) {print(rmse(x, benefit_change, .pred)); x})() |> 
  ggplot(aes(benefit_change, .pred)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed")

# linear mod --------------------------------------------------------------

mod_diff_ln <- l
