library(future)

# importing and preparing dataset ----------------------------------------------
plan(multisession, workers = 4)
source("R/prediction_data_import.R")

ukmod_tidy <- import_ukmod_data()

plan(sequential)


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


# predict data from UKMOD -------------------------------------------------

data_split <- initial_split(model_data, prop = 0.8, strata = year)

train_data <- training(data_split) |> select(-year)
test_data <- testing(data_split) |> select(-year)

mc_train_set <- mc_cv(train_data)
cv_train_set <- vfold_cv(train_data, v = 10)



# tuning grid -------------------------------------------------------------

library(doParallel)

cores <- parallel::detectCores()
cl <- parallel::makePSOCKcluster(floor(0.95*cores))

registerDoParallel(cl)



# random forest -----------------------------------------------------------
# Not working - huuuuuuuge RAM increases!
# tune_class_forest <-
#   rand_forest(
#     engine = "ranger",
#     mode = "classification",
#     trees = tune(),
#     min_n = tune()
#   )
# 
# recipe_tune_class_forest <- workflow() |>
#   add_model(tune_class_forest) |>
#   add_formula(
#     uc_receipt ~ .
#   )
# 
# 
# 
# forest_tune_grid <- tune_class_forest |> 
#   extract_parameter_set_dials() |> 
#   grid_regular(levels = 4)
# 
# tune_out_class_forest <- 
#   tune_grid(
#     recipe_tune_class_forest,
#     grid = forest_tune_grid,
#     resamples = cv_train_set,
#     metrics = metric_set(sens, spec, ppv, npv, roc_auc),
#     control = control_grid(parallel_over = "everything",
#                            verbose = TRUE)
#   )
# 
# 
# saveRDS(tune_out_class_forest, "output/tune_out_class_forest.rds")

# ## xgboost ---------------------------------------------------------------
# 
# 
# 
tune_class_xg <- boost_tree(trees = 1000, tree_depth = tune(), min_n = tune(), learn_rate = tune(), loss_reduction = tune()) |>
  set_engine("xgboost") |>
  set_mode("classification")

recipie_tune_class_xg <- workflow() |>
  add_model(tune_class_xg) |>
  add_formula(uc_receipt ~ .)

xg_tune_grid <- tune_class_xg |>
  extract_parameter_set_dials() |>
  grid_regular(levels = 3)

xg_tune_grid$min_n <- rep(c(40, 45, 50), 27)
xg_tune_grid$tree_depth <- rep(rep(c(10, 15, 20), each = 3), 9)


start <- Sys.time()
tune_out_class_xg <-
  tune_grid(
    recipie_tune_class_xg,
    grid = xg_tune_grid,
    resamples = mc_train_set,
    metrics = metric_set(roc_auc),
    control = control_grid(parallel_over = "everything",
                           verbose = TRUE)
  )
Sys.time() - start


saveRDS(tune_out_class_xg, "output/tune_out_class_xg_up.rds")

stopCluster(cl)
