library(tidyverse)
library(lubridate)
library(data.table)
library(fst)

apr11_mar21 <- c(
  "data/fst_files/apr11_mar12.fst",
  "data/fst_files/apr12_mar13.fst",
  "data/fst_files/apr13_mar14.fst",
  "data/fst_files/apr14_mar15.fst",
  "data/fst_files/apr15_mar16.fst",
  "data/fst_files/apr16_mar17.fst",
  "data/fst_files/apr17_mar18.fst",
  "data/fst_files/apr18_mar19.fst",
  "data/fst_files/apr19_mar20.fst",
  "data/fst_files/apr20_mar21.fst"
) |> map(read_fst, c(
  "satis", 
  "anxious", 
  "happy", 
  "worth", 
  "refdte"),
  as.data.table = TRUE)

apr11_mar21 |> 
  reduce(bind_rows) |> 
  mutate(across(satis:worth, ~ ifelse(.x == -9|.x == -8, NA, .x)),
         Date = floor_date(dmy(refdte), "months"),
         Year = year(Date)) |> 
  filter(Year < 2020) |> 
  pivot_longer(satis:worth, names_to = "metric", values_to = "Score") |> 
  filter(!is.na(Score)) |> 
  ggplot(aes(Year, Score)) +
  stat_summary(geom = "pointrange", shape = 15,
               fun.data = mean_se) +
  facet_wrap(~metric, scale = "free_y")


# pre-covid ---------------------------------------------------------------

pre_cov <- apr11_mar21 |> 
  reduce(bind_rows) |> 
  mutate(across(satis:worth, ~ ifelse(.x == -9|.x == -8, NA, .x)),
         Date = floor_date(dmy(refdte), "months")) |> 
  pivot_longer(satis:worth, names_to = "metric", values_to = "Score") |> 
  as.data.table() |> 
  filter(!is.na(Score), Date < dmy("01-01-2020"), metric == "anxious") |> 
  mutate(time = as.numeric(as.factor(Date)),
         exp = if_else(Date < ymd("2019-01-01"), 0, 1),
         month = month(Date)) |> 
  rowwise() |> 
  mutate(t_exp = max(0, time - 93)) |> 
  ungroup()


mod <- lm(Score ~ time + exp + t_exp + factor(month), data = pre_cov)

summary(mod)

pre_cov |> 
  mutate(line = predict(mod)) |> 
  ggplot(aes(time, line)) +
  geom_line() +
  geom_vline(xintercept = 93.5, linetype = "dashed") +
  stat_summary(aes(y = Score), geom = "pointrange", shape = 15,
               fun.data = mean_se)

library(forecast)
library(zoo)
library(astsa)

bare <- pre_cov |> 
  group_by(time) |> 
  summarise(score = mean(Score),
            exp = max(exp),
            t_exp = max(t_exp)) |> 
  mutate(diff = score - lag(score)) 

plot(diff(diff(bare$score, 12)), type = "l")

acf2(diff(diff(bare$score, 12)))

mod1 <- auto.arima(bare$score, 
           xreg = as.matrix(bare[c("exp", "t_exp")]),
           approximation = FALSE, allowdrift = FALSE,
           seasonal = TRUE, trace = TRUE)

summary(mod1)
confint(mod1)

# mod2 <- Arima(ts(bare$score[1:93]), order = c(0,1,3))

summary(mod2)


fc <- forecast(mod1, h = 100)



as_tibble(fc) |> 
  transmute(time = 81:180,
            score = `Point Forecast`,
            obs = "pred") |> 
  bind_rows(mutate(bare, obs = "obs") |> filter(time < 94)) |> 
  ggplot(aes(time, score, colour = obs, linetype = obs)) +
  geom_line() +
  geom_point()

autoplot(fc)

scores <- ts(bare$score, start = c(2011, 4), frequency = 12)

autoplot(scores)

mod3 <- auto.arima(window(scores, end = c(2018, 12)),
           seasonal = TRUE,
           # as.matrix(bare[c("exp", "t_exp")]),
           trace = TRUE,
           approximation = FALSE,
           allowdrift = FALSE)

fc <- forecast(mod3, h = 12)

summary(mod3)

autoplot(fc)

data.frame(fc) |> 
  rownames_to_column("date") |> 
  as_tibble() |> 
  mutate(time = 94:105,
         score = Point.Forecast,
         obs = "pred") |> 
  bind_rows(mutate(bare, obs = "obs")) |> 
  ggplot(aes(time, score, colour = obs, linetype = obs)) +
  geom_line() +
  geom_point()
  
mod4 <- auto.arima(scores,
                   seasonal = TRUE,
                   xreg = as.matrix(bare[c("t_exp")]),
                   trace = TRUE,
                   approximation = FALSE,
                   allowdrift = FALSE)

summary(mod4)
confint(mod4)

fore4 <- forecast(mod4, h = 24, xreg = as.matrix(data.frame(t_exp = 13:36)))

autoplot(fore4)



# doing whole lot again with ts's -----------------------------------------


monthly_obs <- apr11_mar21 |> 
  reduce(bind_rows) |> 
  mutate(across(satis:worth, ~ ifelse(.x == -9|.x == -8, NA, .x)),
         Date = floor_date(dmy(refdte), "months")) |> 
  filter(!is.na(Date)) |> 
  group_by(Date) |> 
  summarise(across(satis:worth, mean, na.rm = TRUE))



wb_ts <- monthly_obs |> 
  select(-Date) |> 
  map(ts, start = c(2011, 4), frequency = 12)

wb_mods <- wb_ts |> 
  map(window, end = c(2020, 1)) |> 
  map(auto.arima,
      seasonal = TRUE,
      trace = TRUE,
      approximation = FALSE,
      allowdrift = FALSE)


forcast_to_df <- function(fc) {
  data.frame(fc) |> 
    rownames_to_column("Date") |> 
    as_tibble() |> 
    transmute(Date = dmy(paste("1", Date)),
           score = Point.Forecast,
           upper_ci = Hi.95,
           lower_ci = Lo.95,
           obs = "pred")
}


predictions <- wb_mods |> 
  map(forecast, h = 15) |> 
  map(forcast_to_df) |> 
  imap(~ mutate(.x, metric = .y)) |> 
  reduce(bind_rows)


monthly_obs |> 
  pivot_longer(satis:worth, names_to = "metric", values_to = "score") |> 
  mutate(obs = "observed") |> 
  bind_rows(predictions) |> 
  ggplot(aes(Date, score, colour = obs, fill = obs)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
              colour = NA, alpha = 0.2) +
  facet_wrap(~metric, scale = "free_y")
