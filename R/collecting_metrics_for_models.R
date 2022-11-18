library(doParallel)

cores <- parallel::detectCores()
cl <- parallel::makePSOCKcluster(floor(0.98*cores))

registerDoParallel(cl)


cv_class_xg <- fit_resamples(recipe_class_xg, mc_train_set, control = control_resamples(save_pred = TRUE,
                                                                                        parallel_over = "everything",
                                                                                        verbose = TRUE))

cv_class_log <- fit_resamples(wf_class_log, mc_train_set, control = control_resamples(save_pred = TRUE,
                                                                                      parallel_over = "everything",
                                                                                      verbose = TRUE))