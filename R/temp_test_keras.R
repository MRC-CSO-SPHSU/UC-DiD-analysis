# tune mlp model ---------------------------------------------------------------

tune_class_mlp <- mlp(hidden_units = 7, epochs = 20, activation = tune()) |> 
  set_engine("keras", trace = 0) |> 
  set_mode("classification")

tune_simple_mlp <- mlp(engine = "keras", mode = "classification")

recipie_tune_class_mlp <- workflow() |>
  add_model(tune_class_mlp) |>
  add_formula(
    uc_receipt ~ age +
      i_c +
      region + disab + educ + gender + emp_len + seeking + student +
      house_ten + house_resp + caring + n_hh_emp + n_hh_unemp + n_hh_inact +
      children + employment + marsta
  )

mlp_tune_grid <- tune_class_mlp |>
  extract_parameter_set_dials() |>
  grid_regular(levels = 4)

workflow() |> 
  add_model(tune_simple_mlp) |> 
  add_formula(
    uc_receipt ~ age +
      i_c +
      region + disab + educ + gender + emp_len + seeking +
      house_ten + house_resp + caring + n_hh_emp + n_hh_unemp + n_hh_inact +
      children + employment + marsta
  ) |> 
  fit(train_data)

start <- Sys.time()
tune_out_class_mlp <-
  tune_grid(
    recipie_tune_class_mlp,
    grid = mlp_tune_grid,
    resamples = cv_train_set,
    metrics = metric_set(roc_auc),
    control = control_grid(parallel_over = "everything",
                           verbose = TRUE)
  )
Sys.time() - start


saveRDS(tune_out_class_mlp, "output/tune_out_class_keras.rds")



autoplot(tune_out_class_mlp)

show_best(tune_out_class_mlp)
