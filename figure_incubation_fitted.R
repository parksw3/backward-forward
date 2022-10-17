library(dplyr)
library(ggplot2); theme_set(theme_bw())
library(egg)
source("param.R")

load("fit_sepir_gamma.rda")
load("fit_sepir_gamma_uncertainty_1day.rda")
load("fit_sepir_gamma_uncertainty_1day_both.rda")
load("fit_sepir_gamma_uncertainty_2day.rda")
load("fit_sepir_gamma_uncertainty_2day_both.rda")

fit_incubation_truncate2 <- fit_incubation_truncate %>%
  bind_rows %>%
  mutate(
    type="Right truncation adjustment"
  )

fit_incubation_exponential2 <- fit_incubation_exponential %>%
  bind_rows %>%
  mutate(
    type="Exponential growth adjustment"
  )

fit_incubation_known <- bind_rows(
  fit_incubation_truncate2,
  fit_incubation_exponential2
)

fit_incubation_known_summ <- bind_rows(
  fit_incubation_truncate2,
  fit_incubation_exponential2
) %>%
  group_by(type, tmeasure) %>%
  summarize(
    coverage=mean(lwr < true_inc_mean & true_inc_mean < upr),
    coverage_lwr=binom.test(sum(lwr < true_inc_mean & true_inc_mean < upr), 100)[[4]][[1]],
    coverage_upr=binom.test(sum(lwr < true_inc_mean & true_inc_mean < upr), 100)[[4]][[2]],
    bias=mean((cmean - true_inc_mean)/true_inc_mean),
    bias_lwr=t.test((cmean - true_inc_mean)/true_inc_mean)[[4]][1],
    bias_upr=t.test((cmean - true_inc_mean)/true_inc_mean)[[4]][2]
  )

g1 <- ggplot(fit_incubation_known) +
  geom_boxplot(aes(tmeasure, cmean, group=interaction(tmeasure, type), fill=type), alpha=0.7) +
  geom_hline(yintercept=true_inc_mean, lty=2) +
  scale_x_continuous("Truncation time (days)") +
  scale_y_sqrt("Estimated mean incubation period (days)") +
  scale_fill_manual(values=c("black", "orange")) +
  ggtitle("A") +
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank(),
    legend.position = c(0.33, 0.85),
    legend.background = element_rect(fill=NA)
  )

g2 <- ggplot(fit_incubation_known) +
  geom_boxplot(aes(tmeasure, upr-lwr, group=interaction(tmeasure, type), fill=type), alpha=0.7) +
  scale_x_continuous("Truncation time (days)") +
  scale_y_sqrt("Confidence interval widths (days)", breaks=c(1, 2, 5, 10, 20, 40, 80)) +
  scale_fill_manual(values=c("black", "orange")) +
  ggtitle("B") +
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank(),
    legend.position = "none"
  )

g3 <- ggplot(fit_incubation_known_summ) +
  geom_point(aes(tmeasure, bias, col=type, shape=type), position=position_dodge(width=1)) +
  geom_errorbar(aes(tmeasure, ymin=bias_lwr, ymax=bias_upr, col=type), width=0, position=position_dodge(width=1)) +
  geom_hline(yintercept=0, lty=2) +
  scale_x_continuous("Truncation time (days)") +
  scale_y_continuous("Relative bias") +
  scale_color_manual(values=c("black", "orange")) +
  ggtitle("C") +
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank(),
    legend.position = "none"
  )

g4 <- ggplot(fit_incubation_known_summ) +
  geom_point(aes(tmeasure, coverage, col=type, shape=type), position=position_dodge(width=1)) +
  geom_errorbar(aes(tmeasure, ymin=coverage_lwr, ymax=coverage_upr, col=type), width=0, position=position_dodge(width=1)) +
  geom_hline(yintercept=0.95, lty=2) +
  scale_x_continuous("Truncation time (days)") +
  scale_y_continuous("Coverage") +
  scale_color_manual(values=c("black", "orange")) +
  ggtitle("D") +
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank(),
    legend.position = "none"
  )

gfinal <- ggarrange(g1, g2, g3, g4, nrow=2, draw=FALSE)

ggsave("figure_incubation_fitted.pdf", gfinal, width=8, height=6)

fit_incubation_truncate_uncertainty_1day2 <- fit_incubation_truncate_uncertainty_1day %>%
  bind_rows %>%
  mutate(
    type="1 day uncertainty"
  )

fit_incubation_truncate_exponential_uncertainty_1day2 <- fit_incubation_truncate_exponential_uncertainty_1day %>%
  bind_rows %>%
  mutate(
    type="1 day uncertainty (exponential adjustment)"
  )

fit_incubation_truncate_uncertainty_2day2 <- fit_incubation_truncate_uncertainty_2day %>%
  bind_rows %>%
  mutate(
    type="2 day uncertainty"
  )

fit_incubation_truncate_exponential_uncertainty_2day2 <- fit_incubation_truncate_exponential_uncertainty_2day %>%
  bind_rows %>%
  mutate(
    type="2 day uncertainty (exponential adjustment)"
  )

fitall <- bind_rows(
  fit_incubation_truncate_uncertainty_1day2,
  fit_incubation_truncate_exponential_uncertainty_1day2,
  fit_incubation_truncate_uncertainty_2day2,
  fit_incubation_truncate_exponential_uncertainty_2day2
)
fitall_summ <- fitall %>%
  group_by(type, tmeasure) %>%
  summarize(
    coverage=mean(lwr < true_inc_mean & true_inc_mean < upr),
    coverage_lwr=binom.test(sum(lwr < true_inc_mean & true_inc_mean < upr), 100)[[4]][[1]],
    coverage_upr=binom.test(sum(lwr < true_inc_mean & true_inc_mean < upr), 100)[[4]][[2]],
    bias=mean((cmean - true_inc_mean)/true_inc_mean),
    bias_lwr=t.test((cmean - true_inc_mean)/true_inc_mean)[[4]][1],
    bias_upr=t.test((cmean - true_inc_mean)/true_inc_mean)[[4]][2]
  )

g5 <- ggplot(fitall) +
  geom_boxplot(aes(tmeasure, cmean, group=interaction(tmeasure, type), fill=type), alpha=0.7) +
  geom_hline(yintercept=true_inc_mean, lty=2) +
  scale_x_continuous("Truncation time (days)") +
  scale_y_sqrt("Estimated mean incubation period (days)") +
  scale_fill_viridis_d() +
  ggtitle("A") +
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank(),
    legend.position = c(0.73, 0.85),
    legend.background = element_rect(fill=NA)
  )

g6 <- ggplot(fitall) +
  geom_boxplot(aes(tmeasure, upr-lwr, group=interaction(tmeasure, type), fill=type), alpha=0.7) +
  scale_x_continuous("Truncation time (days)") +
  scale_y_sqrt("Confidence interval widths (days)", breaks=c(1, 2, 5, 10, 20, 40, 80)) +
  scale_fill_viridis_d() +
  ggtitle("B") +
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank(),
    legend.position = "none"
  )

g7 <- ggplot(fitall_summ) +
  geom_point(aes(tmeasure, bias, col=type, shape=type), position=position_dodge(width=4)) +
  geom_errorbar(aes(tmeasure, ymin=bias_lwr, ymax=bias_upr, col=type), width=0, position=position_dodge(width=4)) +
  geom_hline(yintercept=0, lty=2) +
  scale_x_continuous("Truncation time (days)") +
  scale_y_continuous("Relative bias") +
  scale_color_viridis_d() +
  ggtitle("C") +
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank(),
    legend.position = "none"
  )

g8 <- ggplot(fitall_summ) +
  geom_point(aes(tmeasure, coverage, col=type, shape=type), position=position_dodge(width=4)) +
  geom_errorbar(aes(tmeasure, ymin=coverage_lwr, ymax=coverage_upr, col=type), width=0, position=position_dodge(width=4)) +
  geom_hline(yintercept=0.95, lty=2) +
  scale_x_continuous("Truncation time (days)") +
  scale_y_continuous("Coverage") +
  scale_color_viridis_d() +
  ggtitle("D") +
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank(),
    legend.position = "none"
  )

gfinal2 <- ggarrange(g5, g6, g7, g8, nrow=2, draw=FALSE)

ggsave("figure_incubation_fitted2.pdf", gfinal2, width=12, height=10)

