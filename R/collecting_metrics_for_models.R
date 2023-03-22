library(doParallel)

cores <- parallel::detectCores()
cl <- parallel::makePSOCKcluster(50)

registerDoParallel(cl)


cv_class_log <- fit_resamples(wf_class_log, mc_train_set, control = control_resamples(save_pred = TRUE,
                                                                                      parallel_over = "everything",
                                                                                      verbose = TRUE),
                              metrics = metric_set(sens, spec, ppv, npv, roc_auc))


cv_class_xg <- fit_resamples(recipe_class_xg, mc_train_set, control = control_resamples(save_pred = TRUE,
                                                                                        parallel_over = "everything",
                                                                                        verbose = TRUE),
                             metrics = metric_set(sens, spec, ppv, npv, roc_auc))

stopCluster(cl)


cv_class_log |>  
  collect_metrics()

cv_class_xg |> 
  collect_metrics()

saveRDS(cv_class_log, "output/cv_class_log_no_hh.rds")
saveRDS(cv_class_xg, "output/cv_class_xg_no_hh.rds")
