library(dplyr)
library(ggplot2); theme_set(theme_bw(base_family = "Times"))
library(egg)
source("param.R")
load("exponential_simulation.rda")
load("fit_exponential_gamma.rda")
load("fit_exponential_gamma_uncertainty.rda")

exponential_simulation_truncate <- exponential_simulation %>%
  filter(t_symptom < tmax)

exponential_simulation_truncate %>%
  group_by(r) %>%
  summarize(
    total=n()
  )

exponential_simulation_truncate_summ <- exponential_simulation_truncate %>%
  group_by(r) %>%
  summarize(
    mean=mean(t_symptom-t_infected),
    lwr=t.test(t_symptom-t_infected)[[4]][1],
    upr=t.test(t_symptom-t_infected)[[4]][2]
  )

g1 <- ggplot(exponential_simulation_truncate_summ) +
  geom_hline(yintercept=true_inc_mean, lty=2) +
  geom_point(aes(r, mean)) +
  geom_errorbar(aes(r, ymin=lwr, ymax=upr), width=0) +
  scale_x_continuous("Growth rate (1/days)") +
  scale_y_continuous("Truncated mean incubation period") +
  theme(
    panel.grid = element_blank()
  )

ggsave("figure_sensitivity_growth_truncation1.pdf", g1, width=6, height=4)

g2 <- ggplot(fit_exponential_gamma) +
  geom_hline(yintercept=true_inc_mean, lty=2) +
  geom_point(aes(r, est, col=method, shape=method), position=position_dodge(width=0.02), size=2) +
  geom_errorbar(aes(r, ymin=lwr, ymax=upr, col=method), width=0, position=position_dodge(width=0.02)) +
  scale_x_continuous("Growth rate (1/days)") +
  scale_y_continuous("Estimated mean incubation period (days)") +
  scale_color_viridis_d(end = 0.9) +
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank(),
    legend.position = c(0.3, 0.8)
  )

ggsave("figure_sensitivity_growth_truncation2.pdf", g2, width=6, height=4)

g3 <- ggplot(fit_exponential_gamma) +
  geom_hline(yintercept=true_inc_mean, lty=2) +
  geom_point(aes(r, est, col=method, shape=method), position=position_dodge(width=0.02), size=2) +
  geom_errorbar(aes(r, ymin=lwr, ymax=upr, col=method), width=0, position=position_dodge(width=0.02)) +
  scale_x_continuous("Growth rate (1/days)") +
  scale_y_continuous("Estimated mean incubation period (days)", limits=c(4.5, 5.5)) +
  scale_color_viridis_d(end = 0.9) +
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank(),
    legend.position = c(0.3, 0.8)
  )

ggsave("figure_sensitivity_growth_truncation3.pdf", g3, width=6, height=4)

g4 <- ggplot(fit_exponential_gamma_uncertainty) +
  geom_hline(yintercept=true_inc_mean, lty=2) +
  geom_point(aes(r, est, col=method, shape=method), position=position_dodge(width=0.02), size=2) +
  geom_errorbar(aes(r, ymin=lwr, ymax=upr, col=method), width=0, position=position_dodge(width=0.02)) +
  scale_x_continuous("Growth rate (1/days)") +
  scale_y_continuous("Estimated mean incubation period (days)") +
  scale_color_viridis_d(end = 0.9) +
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank(),
    legend.position = c(0.3, 0.2)
  )

ggsave("figure_sensitivity_growth_truncation4.pdf", g4, width=6, height=4)
