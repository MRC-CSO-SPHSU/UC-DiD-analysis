library(tidyverse)
library(tidymodels)
# library(SPHSUgraphs)
library(furrr)
library(data.table)

source("R/prediction_data_import.R")

ukmod_tidy <- import_ukmod_data()

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

# probability of UC receipt -----------------------------------------------


## XG boost ------------------------

mod_class_xg <- boost_tree(tree_depth = 10, trees = 1000, min_n = 40,
                           learn_rate = 0.0178, loss_reduction = 0.0000562) |>
  set_engine("xgboost") |>
  set_mode("classification")

recipe_class_xg <- workflow() |>
  add_model(mod_class_xg) |>
  add_formula(
    uc_receipt ~ age +
      i_c +
      region + disab + educ + gender + emp_len + seeking + student +
      house_ten + house_resp + caring + n_hh_emp + n_hh_unemp + n_hh_inact +
      children + employment + marsta
  )

fit_class_xg <- recipe_class_xg |>
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


### with MC resampling CV ---------------------------------------------


cv_class_xg <- fit_resamples(recipe_class_xg, mc_train_set, control = control_resamples(save_pred = TRUE))

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
         uc_receipt = factor(uc_receipt, levels = c("1", "0"), labels = c("UC", "No UC"))) |> 
  group_by(uc_receipt, prob) |> 
  summarise(Pred_receive = sum(.pred_class == "UC")/n()) |> 
  ggplot(aes(prob, Pred_receive, colour = uc_receipt)) +
  geom_line()

## logistic classification ----------------------------------

mod_class_log <- logistic_reg() |> 
  set_engine("glm") |> 
  set_mode("classification")


recipe_class_log <- recipe(
  uc_receipt ~ age + i_c +
    region + disab + educ + emp_len + 
    house_ten + house_resp + seeking + student +
    n_hh_emp  +  n_hh_unemp  +  n_hh_inact + 
    caring  +  employment +  gender + marsta + children,
  data = train_data
) |> 
  step_dummy(all_nominal_predictors()) |> 
  step_interact(
    ~ starts_with('gender_'):starts_with('children_') + starts_with('gender_'):starts_with('children_'):starts_with('employment_') + starts_with('children_'):starts_with('employment_') + student:starts_with('children_') + student:starts_with('caring_') + starts_with('marsta_'):starts_with('employment_') + starts_with('n_hh_emp_'):starts_with('children_') + starts_with('n_hh_unemp_'):starts_with('children_') + starts_with('n_hh_inact_'):starts_with('children_') + starts_with('n_hh_emp_'):starts_with('caring_') + starts_with('n_hh_unemp_'):starts_with('caring_') + starts_with('n_hh_inact_'):starts_with('caring_') + starts_with('marsta_')*starts_with('gender_')*starts_with('children_')
  )


wf_class_log <- workflow() |>
  add_model(mod_class_log) |>
  add_recipe(recipe_class_log)
# add_formula(
#   uc_receipt ~ poly(age, 2) + i_c +
#     # # poly(i_l, 4) + i_0 + i_m +
#     region + disab + educ + emp_len + 
#     house_ten + house_resp + seeking + student +
#     n_hh_emp * n_hh_unemp * n_hh_inact + 
#     caring * employment + gender:children + gender:children:employment +
#     children:employment + student:children + student:caring + marsta:employment +
#     n_hh_emp:children + n_hh_unemp:children + n_hh_inact:children +
#     n_hh_emp:caring + n_hh_unemp:caring + n_hh_inact:caring +
#     gender*marsta*children
# )

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

pred_class_log |> 
  (\(x) {print(conf_mat(x, uc_receipt, .pred_class)); x})() |> 
  ggplot(aes(uc_receipt, fill = .pred_class)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::label_percent())

pred_class_log |> 
  select(.pred_1, uc_receipt) |> 
  crossing(prob = c(0.1, 0.25, 0.5, 0.75, 0.9)) |> 
  mutate(.pred_class = factor(.pred_1 > prob, labels = c("No UC", "UC")),
         uc_receipt = factor(uc_receipt, levels = c("1", "0"), labels = c("UC", "No UC"))) |> 
  ggplot(aes(uc_receipt, fill = .pred_class)) +
  geom_bar(position = "fill") +
  scale_y_continuous("Perc predicted", labels = scales::label_percent()) +
  facet_wrap(~ prob, nrow = 1)


## practice exports ----------------------------------------------

library(butcher)
library(tidypredict)

log_mod_exp <- fit_class_log |> extract_fit_engine() 

object.size(log_mod_exp)

log_mod_sm <- axe_data(log_mod_exp)

object.size(log_mod_sm)

log_mod_tiny <- butcher(log_mod_exp)

weigh(log_mod_tiny)
parse_model(log_mod_exp)
parse_model(log_mod_tiny)

fit_class_log |> extract_preprocessor()

## mixed effects classification ----------------------------------

library(multilevelmod)

mod_class_me <- logistic_reg() |> 
  set_engine("glmer") |> 
  set_mode("classification")

recipe_class_me <-   workflow() |>
  add_variables(
    outcomes = uc_receipt,
    predictors = c(
      age, i_c, disab, educ, gender, emp_len,
      seeking, student, house_ten, house_resp, n_hh_emp,
      n_hh_unemp, n_hh_inact, children, employment, marsta, caring,
      region
    )
  ) |>
  step_poly(age, 2) |>
  step_dummy(all_nominal_predictors() - region) |>
  step_interact(
    ~ starts_with("children_"):starts_with("employment_"):starts_with("marsta_"):starts_with("caring_") +
      starts_with("employment"):student
  ) |>
  add_model(
    mod_class_me,
    formula = uc_receipt ~ age + i_c +
      disab + educ + gender + emp_len + seeking + student +
      house_ten + house_resp + n_hh_emp + n_hh_unemp + n_hh_inact +
      children + employment + marsta + caring + (1 | region)
  )


recipe_class_me <- recipe(uc_receipt ~ age + i_c +
                            disab + educ + gender + emp_len + seeking + student +
                            house_ten + house_resp + n_hh_emp + n_hh_unemp + n_hh_inact +
                            children + employment + marsta + caring + region,
                          data = train_data) |> 
  add_role(region, new_role = "exp_unit") |>
  step_interact(
    ~ starts_with("children_"):starts_with("employment_"):starts_with("marsta_"):starts_with("caring_") +
      starts_with("employment"):student
  ) |>
  step_poly(age, 2) |>
  step_dummy(all_nominal_predictors() - region)



# workflow() |>
#   add_model(mod_class_me) |> 
#   add_recipe(recipe_class_me) |> 
#   fit(train_data)

fit_class_me <-
  fit(recipe_class_me,
      data = train_data)


pred_class_me <- test_data |> 
  bind_cols(predict(fit_class_me, new_data = test_data, type = "prob")) |> 
  mutate(.pred_class = factor(as.numeric(.pred_1 > 0.5), levels = c("1", "0")))

pred_class_me |> 
  (\(x) {print(roc_auc(x, uc_receipt, .pred_1)); x})() |> 
  roc_curve(uc_receipt, .pred_1) |> 
  autoplot()

pred_class_me |> 
  (\(x) {print(conf_mat(x, uc_receipt, .pred_class)); x})() |> 
  ggplot(aes(uc_receipt, fill = .pred_class)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::label_percent())

pred_class_me |> 
  select(.pred_1, uc_receipt) |> 
  crossing(prob = c(0.1, 0.25, 0.5, 0.75, 0.9)) |> 
  mutate(.pred_class = factor(.pred_1 > prob, labels = c("No UC", "UC")),
         uc_receipt = factor(uc_receipt, levels = c("1", "0"), labels = c("UC", "No UC"))) |> 
  ggplot(aes(uc_receipt, fill = .pred_class)) +
  geom_bar(position = "fill") +
  scale_y_continuous("Perc predicted", labels = scales::label_percent()) +
  facet_wrap(~ prob, nrow = 1)


## knn ---------------------------------------------------------------------

mod_class_knn <- nearest_neighbor(mode = "classification", neighbors = 10)

recipe_class_knn <- recipe(uc_receipt ~ .,
                           data = train_data) 

fit_class_knn <- fit(mod_class_knn, recipe_class_knn, data = train_data)

## with Monte Carlo resampling for cross-validation ----------------------

library(doParallel)

cores <- parallel::detectCores()
cl <- parallel::makePSOCKcluster(floor(0.98*cores))

registerDoParallel(cl)

cv_class_log <- recipe_class_log |> fit_resamples(mc_train_set, control = control_resamples(save_pred = TRUE))

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
         uc_receipt = factor(uc_receipt, levels = c("1", "0"), labels = c("UC", "No UC"))) |> 
  group_by(uc_receipt, prob) |> 
  summarise(Pred_receive = sum(.pred_class == "UC")/n()) |> 
  ggplot(aes(prob, Pred_receive, colour = uc_receipt)) +
  geom_line()  



stopCluster(cl)

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

