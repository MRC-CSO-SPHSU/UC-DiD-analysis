library(fst)
library(tidyverse)
library(data.table)

cols_present <- read_fst("data/fst_files/apr17_mar18.fst", to  = 10)

cols_present$npwt18

wb_df <-
  c(
    # "data/fst_files/apr11_mar12.fst",
    # "data/fst_files/apr12_mar13.fst",
    # "data/fst_files/apr13_mar14.fst",
    "data/fst_files/apr14_mar15.fst",
    "data/fst_files/apr15_mar16.fst",
    "data/fst_files/apr16_mar17.fst",
    "data/fst_files/apr17_mar18.fst",
    "data/fst_files/apr18_mar19.fst"
    # "data/fst_files/apr19_mar20.fst"
    # "data/fst_files/apr20_mar21.fst"
  ) |> map(\(file_name) {
    read_fst(file_name,
             columns = c("age", 
                         "satis", 
                         "worth", 
                         "anxious", 
                         "happy", 
                         "statr", 
                         "ilodefr", 
                         "refdte",
                         "npwt18",
                         "gor9d"))
  })

# df <- read_fst("data/fst_files/apr19_mar20.fst")

names(wb_df) <- 2014:2018

tidy_df <- enframe(wb_df, "datayear", "dat") |> 
  unnest(dat) |> 
  mutate(
    Date = floor_date(dmy(refdte), "months"),
    across(satis:happy, ~na_if(.x, -9)),
    month = month(Date) + (year(Date) - 2014) * 12,
    q = quarter(Date) + (year(Date) - 2014) * 4
    )

library(did)
library(magrittr)
library(plm)

pseudo_rollouts <- tidy_df |>
  filter(year(Date) > 2014) |> 
  select(gor9d, rollout = month, r_q = q) |> 
  slice_sample(n = 1, by = gor9d)

tidy_df <- tidy_df |> 
  left_join(pseudo_rollouts, by = join_by(gor9d)) |> 
  mutate(lag_rollout = factor(month - rollout),
         lag_q = factor(q - r_q),
         id = as.numeric(as.factor(gor9d)))


# twfe model --------------------------------------------------------------

model_twfe <- tidy_df |> 
  mutate(uc = (q <= r_q) * 1) %$%
  lm(anxious ~ uc + poly(age, 3) + ilodefr + datayear + gor9d, weights = npwt18)

broom::tidy(model_twfe)


# fixest model ------------------------------------------------------------

library(fixest)

model_feols <- tidy_df |> 
  mutate(uc = (q <= r_q) * 1) |> 
  feols(anxious ~ uc + poly(age, 3) + ilodefr + datayear | r_q + gor9d,
        data = _,
        weights = ~npwt18)

summary(model_feols)

# panel_model -------------------------------------------------------------


panel_model <-  plm(satis ~ lag_q, data = tidy_df, model = "within", effect = "twoways", index = c("gor9d", "q"))


summary(panel_model)


# some housekeeping for making the plot
# add 0 at event time -1
coefs <- coef(panel_model)
ses <- sqrt(diag(summary(panel_model)$vcov))
# coefs <- c(coefs1[idx.pre], 0, coefs1[idx.post])
# ses <- c(ses1[idx.pre], 0, ses1[idx.post])
exposure <- -24:23

cmat <- data.frame(coefs=coefs, ses=ses, exposure=exposure)

library(ggplot2)

ggplot(data = cmat, mapping = aes(y = coefs, x = exposure)) +
  geom_line(linetype = "dashed") +
  geom_point() + 
  geom_errorbar(aes(ymin = (coefs-1.96*ses), ymax = (coefs+1.96*ses)), width = 0.2) +
  ylim(c(-2, 5)) +
  theme_bw()

# Using did methods

did_att_gt <- att_gt(yname = "anxious",
                     tname = "q",
                     idname = "id",
                     gname = "rollout",
                     xformla = ~ ilodefr,
                     data = tidy_df,
                     control_group = "notyettreated",
                     clustervars = "gor9d",
                     weightsname = "npwt18",
                     bstrap = TRUE,
                     cband = FALSE,
                     panel = FALSE)


summary(did_att_gt)

# aggregate them into event study plot
did_es <- aggte(did_att_gt, type = "dynamic")

# plot the event study
ggdid(did_es)

aggte(did_att_gt, type = "simple")

conditional_did_pretest(
  yname = "anxious",
  tname = "month",
  gname = "rollout",
  idname = "id",
  xformla = ~ilodefr,
  data = tidy_df |> filter(month < max(rollout)),
  panel = FALSE,
  bstrap = FALSE
)
