library(patchwork)
library(goft)
library(SPHSUgraphs)
source("R/prediction_data_import.R")

ukmod_tidy <- import_ukmod_data()

ukmod_tidy |> 
  select(uc_income, lba_income) |> 
  pivot_longer(uc_income:lba_income, names_to = "benefit", values_to = "income", names_pattern = "(.*)_income") |> 
  filter(income < quantile(income, 0.99)) |> 
  ggplot(aes(income, y = after_stat(count/sum(count)), fill = benefit)) +
  geom_histogram(bins = 30) +
  scale_fill_sphsu(name = "Benefit", labels = c("Legacy Benefits", "Universal Credit")) +
  scale_x_continuous("Benefit income (/yr)", labels = label_dollar(prefix = "£")) +
  scale_y_continuous("", expand = expansion(mult = c(0, 0.01)), labels = label_percent()) +
  facet_wrap(~benefit) +
  theme(strip.text = element_blank())

gamma_params <- test_df |> 
  select(uc_income, lba_income, dwt) |> 
  pivot_longer(uc_income:lba_income, names_to = "benefit", values_to = "income", names_pattern = "(.*)_income") |> 
  filter(income !=0) |> 
  group_by(benefit) |> 
  summarise(gamma_fit = list(as_tibble(t(gamma_fit(income))))) |> 
  unnest(gamma_fit)

p1 <- test_df |> 
  select(uc_income, lba_income, dwt) |> 
  pivot_longer(uc_income:lba_income, names_to = "benefit", values_to = "income", names_pattern = "(.*)_income") |> 
  mutate(benefit = factor(benefit, levels = c("lba", "uc"))) |> 
  filter(income < quantile(income, 0.999)) |> 
  ggplot(aes(income, y = after_stat(count/sum(count)), weight = dwt, fill = benefit)) +
  geom_histogram(bins = 30) +
  scale_fill_manual(name = "Benefit", labels = c("Legacy Benefits", "Universal Credit"),
                    values = sphsu_cols("Pumpkin", "University Blue", names = FALSE)) +
  scale_x_continuous("Benefit income (/yr)", labels = label_dollar(prefix = "£")) +
  scale_y_continuous("Percent", expand = expansion(mult = c(0, 0.01)), labels = label_percent()) +
  facet_wrap(~benefit) +
  theme(strip.text = element_blank())

p2 <- test_df |> 
  select(uc_income, lba_income, dwt) |> 
  pivot_longer(uc_income:lba_income, names_to = "benefit", values_to = "income", names_pattern = "(.*)_income") |> 
  mutate(benefit = factor(benefit, levels = c("lba", "uc"))) |> 
  filter(income !=0, income < quantile(income, 0.999)) |> 
  ggplot(aes(income, y = after_stat(density), weight = dwt, fill = benefit)) +
  geom_histogram(bins = 30) +
  geom_function(data = gamma_params |> filter(benefit == "lba"),
                fun = dgamma, args = list(shape = gamma_params$shape[1], scale = gamma_params$scale[1]),
                inherit.aes = FALSE, linewidth = 0.75, linetype = "longdash") +
  geom_function(data = gamma_params |> filter(benefit == "uc"),
                fun = dgamma, args = list(shape = gamma_params$shape[2], scale = gamma_params$scale[2]),
                inherit.aes = FALSE, linewidth = 0.75, linetype = "longdash") +
  scale_fill_manual(name = "Benefit", labels = c("Legacy Benefits", "Universal Credit"),
                    values = sphsu_cols("Pumpkin", "University Blue", names = FALSE)) +
  scale_x_continuous("Benefit income (/yr; excluding £0)", labels = label_dollar(prefix = "£")) +
  scale_y_continuous("Density", expand = expansion(mult = c(0, 0.01))) +
  facet_wrap(~benefit) +
  theme(strip.text = element_blank())


p1/p2 + plot_layout(guides = "collect")

ukmod_tidy |> 
  ggplot(aes(uc_income, y = after_stat(count))) +
  geom_histogram(bins = 4)

ukmod_uc <- ukmod_tidy |> 
  mutate(benefit = uc_income) |> 
  filter(benefit != 0)

alpha_param <- mean(ukmod_uc$benefit)^2/var(ukmod_uc$benefit)
beta_param <- var(ukmod_uc$benefit)/mean(ukmod_uc$benefit)




gamma_test(ukmod_uc$benefit)
exp_test(ukmod_uc$benefit)
sn_test(log(sample(ukmod_uc$benefit, 5000)))
normal_test(log(sample(ukmod_uc$benefit, 400)))
weibull_test(sample(ukmod_uc$benefit, 5000), N = 100)
summary(ukmod_uc$benefit)

gamma_ben <- gamma_fit(ukmod_uc$benefit)

cov(ukmod_uc$benefit, log(ukmod_uc$benefit))


ukmod_uc |> 
  ggplot(aes(x = benefit)) +
  geom_histogram(aes(y = after_stat(density)), bins = 100) +
  geom_function(fun = dgamma, args = list(shape = gamma_ben[1], scale = gamma_ben[2]), colour = "red") +
  geom_function(fun = dgamma, args = list(shape = alpha_param, scale = beta_param), colour = "green")

gamma_test(ukmod_uc$benefit)


hist(sqrt(ukmod_uc$benefit))

x <- ukmod_uc$benefit

n <- length(x)
x.bar <- mean(x)
s2.x <- var(x)
a.check <- x.bar^2/s2.x
b.check <- s2.x/x.bar
# b.check <- cov(x, z)
# a.check <- x.bar/b.check
v <- sqrt(n * a.check) * (s2.x/(x.bar * b.check) - 1)
p.value <- 2 * pnorm(abs(v), mean = 0, sd = sqrt(2), lower.tail = FALSE)
results <- list(statistic = c(V = v), p.value = p.value,
                method = "Test of fit for the Gamma distribution")
class(results) = "htest"

results


# trying iptw -------------------------------------------------------------

library(magrittr)
compute_v <- function(x, alt = FALSE) {
  n <- length(x)
  x.bar <- mean(x)
  s2.x <- var(x)
  if (alt) {
    b.check <- cov(x, log(x))
    a.check <- x.bar/b.check
  } else {
    b.check <- s2.x/x.bar
    a.check <- x.bar^2/s2.x
  }
  
  vn <- s2.x/(x.bar * b.check)
  vstar <- sqrt(n * a.check) * (vn - 1)
  return(vstar)
}

mod1 <- ukmod_uc %$% glm(benefit ~ 1, family = Gamma(link = "log"))


s1 <- summary(mod1)

# These are exactly the same as calcs in Coffman and Zhong, 2012
shape <- 1/s1$dispersion
scale <- unname(exp(coef(mod1)) / shape)


ukmod_uc |> 
  ggplot(aes(x = benefit)) +
  geom_histogram(aes(y = after_stat(density)), bins = 100) +
  geom_function(fun = dgamma, args = list(shape = shape, scale = scale), colour = "green")

compute_v(ukmod_uc$benefit)
compute_v(ukmod_uc$benefit, TRUE)



mod2 <- ukmod_uc %$%
  glm(benefit ~ age + gender + marsta + emp_len, family = Gamma(link = "log"))

mod2
summary(mod2)

ukmod_uc |> 
  mutate(mean_bens = exp(predict(mod1)),
         pred_bens = exp(predict(mod2))) |> 
  select(benefit, age, gender, marsta, emp_len, mean_bens, pred_bens) |> 
  ggplot(aes(benefit)) +
  geom_histogram(bins = 100) +
  stat_bin(aes(x = pred_bens), geom = "line", bins = 20)


var(log(x))
var(x)
cov(x, log(x))
b.check
