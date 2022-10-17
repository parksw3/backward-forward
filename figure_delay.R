library(dplyr)
library(ggplot2); theme_set(theme_bw(base_family = "Times"))
library(egg)
source("generation.R")
source("param.R")
load("sepir_simulation.rda")

dodge <- 0.5

generationdata <- genfun(sepir_simulation)

daily_incidence <- sepir_simulation %>%
  mutate(
    t_infected=floor(t_infected/1) * 1 + 1
  ) %>%
  group_by(t_infected) %>%
  filter(!is.na(t_infected)) %>%
  summarize(
    n=length(t_infected)
  )

daily_incidence2 <- sepir_simulation %>%
  mutate(
    t_symptom=floor(t_symptom/1) * 1 + 1
  ) %>%
  group_by(t_symptom) %>%
  filter(!is.na(t_symptom)) %>%
  summarize(
    n=length(t_symptom)
  )

incubationdata <- sepir_simulation %>%
  mutate(
    incubation=t_symptom-t_infected,
    t_infected=floor(t_infected/5) * 5 + 5
  ) %>%
  group_by(t_infected) %>%
  filter(!is.na(t_infected)) %>%
  summarize(
    mean=mean(incubation),
    lwr=ifelse(length(incubation) > 1, t.test(incubation)[[4]][1], NA),
    upr=ifelse(length(incubation) > 1, t.test(incubation)[[4]][2], NA)
  )

generationdata_infection <- generationdata %>%
  mutate(
    tcohort=floor(tinfector_infection/5) * 5 + 5
  ) %>%
  group_by(tcohort) %>%
  filter(!is.na(tinfector_infection)) %>%
  summarize(
    mean=mean(generation),
    lwr=ifelse(length(generation) > 1, t.test(generation)[[4]][1], NA),
    upr=ifelse(length(generation) > 1, t.test(generation)[[4]][2], NA)
  )

generationdata_symptom <- generationdata %>%
  mutate(
    tcohort=floor(tinfector_symptom/5) * 5 + 5
  ) %>%
  group_by(tcohort) %>%
  filter(!is.na(tinfector_infection)) %>%
  summarize(
    mean=mean(generation),
    lwr=ifelse(length(generation) > 1, t.test(generation)[[4]][1], NA),
    upr=ifelse(length(generation) > 1, t.test(generation)[[4]][2], NA)
  )

serialdata_infection <- generationdata %>%
  mutate(
    tcohort=floor(tinfector_infection/5) * 5 + 5
  ) %>%
  group_by(tcohort) %>%
  filter(!is.na(tinfector_symptom)) %>%
  summarize(
    mean=mean(serial),
    lwr=ifelse(length(serial) > 1, t.test(serial)[[4]][1], NA),
    upr=ifelse(length(serial) > 1, t.test(serial)[[4]][2], NA)
  )

serialdata_symptom <- generationdata %>%
  mutate(
    tcohort=floor(tinfector_symptom/5) * 5 + 5
  ) %>%
  group_by(tcohort) %>%
  filter(!is.na(tinfector_symptom)) %>%
  summarize(
    mean=mean(serial),
    lwr=ifelse(length(serial) > 1, t.test(serial)[[4]][1], NA),
    upr=ifelse(length(serial) > 1, t.test(serial)[[4]][2], NA)
  )

incdiffdata_infection <- generationdata %>%
  mutate(
    tcohort=floor(tinfector_infection/5) * 5 + 5,
    incdiff=(tinfectee_symptom-tinfectee_infection)-(tinfector_symptom-tinfector_infection)
  ) %>%
  group_by(tcohort) %>%
  filter(!is.na(incdiff)) %>%
  summarize(
    mean=mean(incdiff),
    lwr=ifelse(length(incdiff) > 1, t.test(incdiff)[[4]][1], NA),
    upr=ifelse(length(incdiff) > 1, t.test(incdiff)[[4]][2], NA)
  )

incdiffdata_symptom <- generationdata %>%
  mutate(
    tcohort=floor(tinfector_symptom/5) * 5 + 5,
    incdiff=(tinfectee_symptom-tinfectee_infection)-(tinfector_symptom-tinfector_infection)
  ) %>%
  group_by(tcohort) %>%
  filter(!is.na(incdiff)) %>%
  summarize(
    mean=mean(incdiff),
    lwr=ifelse(length(incdiff) > 1, t.test(incdiff)[[4]][1], NA),
    upr=ifelse(length(incdiff) > 1, t.test(incdiff)[[4]][2], NA)
  )

sergendiff_infection <- generationdata %>%
  mutate(
    tcohort=floor(tinfector_infection/5) * 5 + 5,
    sergendiff=serial-generation
  ) %>%
  group_by(tcohort) %>%
  filter(!is.na(sergendiff)) %>%
  summarize(
    mean=mean(sergendiff),
    lwr=ifelse(length(sergendiff) > 1, t.test(sergendiff)[[4]][1], NA),
    upr=ifelse(length(sergendiff) > 1, t.test(sergendiff)[[4]][2], NA)
  )

sergendiff_symptom <- generationdata %>%
  mutate(
    tcohort=floor(tinfector_symptom/5) * 5 + 5,
    sergendiff=serial-generation
  ) %>%
  group_by(tcohort) %>%
  filter(!is.na(sergendiff)) %>%
  summarize(
    mean=mean(sergendiff),
    lwr=ifelse(length(sergendiff) > 1, t.test(sergendiff)[[4]][1], NA),
    upr=ifelse(length(sergendiff) > 1, t.test(sergendiff)[[4]][2], NA)
  )

daily_incidence_filter <- daily_incidence %>%
  filter(t_infected >= 10, t_infected <= 30)

lm(log(n)~t_infected, data=daily_incidence_filter)

g1 <- ggplot(daily_incidence) +
  geom_point(aes(t_infected, n)) +
  # geom_line(aes(t_infected, n)) +
  geom_point(data=daily_incidence2, aes(t_symptom, n), col=2) +
  # geom_line(data=daily_incidence2, aes(t_symptom, n), col=2) +
  geom_smooth(data=daily_incidence_filter, aes(t_infected, n), 
              method="lm", se=FALSE, lty=2, col="black", fullrange=TRUE, lwd=0.7) +
  scale_x_continuous("Time (day)", limits=c(0, 82), expand=c(0, 0)) +
  scale_y_log10("Incidence of new infections", limits=c(1, 2000), expand=c(0, 0),
                     sec.axis = sec_axis(~., "Incidence of new symptomatic individuals")) +
  ggtitle("A") +
  theme(
    panel.grid = element_blank(),
    axis.line.y.right = element_line(color=2),
    axis.ticks.y.right = element_line(color=2),
    axis.text.y.right = element_text(color=2),
    axis.title.y.right = element_text(color=2)
  )

g2 <- ggplot(incubationdata) +
  geom_point(aes(t_infected, mean)) +
  geom_errorbar(aes(t_infected, ymin=lwr, ymax=upr), width=0) +
  geom_hline(yintercept=true_inc_mean, lty=2) +
  scale_x_continuous("Time of infection (day)", limits=c(0, 82), expand=c(0, 0)) +
  scale_y_continuous("Mean forward incubation period (days)", limits=c(0, 10), expand=c(0, 0)) +
  ggtitle("B") +
  theme(
    panel.grid = element_blank()
  )

g3 <- ggplot(generationdata_infection) +
  geom_point(aes(tcohort-dodge, mean)) +
  geom_errorbar(aes(tcohort-dodge, ymin=lwr, ymax=upr), width=0) +
  geom_hline(yintercept=true_gen_mean, lty=2) +
  geom_point(data=serialdata_infection, aes(tcohort+dodge, mean), col=2) +
  geom_errorbar(data=serialdata_infection, aes(tcohort+dodge, ymin=lwr, ymax=upr), width=0, col=2) +
  scale_x_continuous("Infector's time of infection (day)", limits=c(0, 82), expand=c(0, 0)) +
  scale_y_continuous("Mean forward generation interval (days)", limits=c(0, 10), expand=c(0, 0),
                     sec.axis = sec_axis(~., "Mean forward serial interval (days)")) +
  ggtitle("C") +
  theme(
    panel.grid = element_blank(),
    axis.line.y.right = element_line(color=2),
    axis.ticks.y.right = element_line(color=2),
    axis.text.y.right = element_text(color=2),
    axis.title.y.right = element_text(color=2)
  )

g4 <- ggplot(generationdata_symptom) +
  geom_point(aes(tcohort-dodge, mean)) +
  geom_errorbar(aes(tcohort-dodge, ymin=lwr, ymax=upr), width=0) +
  geom_hline(yintercept=true_gen_mean, lty=2) +
  geom_point(data=serialdata_symptom, aes(tcohort+dodge, mean), col=2) +
  geom_errorbar(data=serialdata_symptom, aes(tcohort+dodge, ymin=lwr, ymax=upr), width=0, col=2) +
  scale_x_continuous("Infector's time of symptom onset (day)", limits=c(0, 82), expand=c(0, 0)) +
  scale_y_continuous("Mean forward generation interval (days)", limits=c(0, 10), expand=c(0, 0),
                     sec.axis = sec_axis(~., "Mean forward serial interval (days)")) +
  ggtitle("D") +
  theme(
    panel.grid = element_blank(),
    axis.line.y.right = element_line(color=2),
    axis.ticks.y.right = element_line(color=2),
    axis.text.y.right = element_text(color=2),
    axis.title.y.right = element_text(color=2)
  )

sergendiff_infection <- data.frame(
  tcohort=generationdata_infection$tcohort,
  diff=serialdata_infection$mean-generationdata_infection$mean
)

sergendiff_symptom <- data.frame(
  tcohort=generationdata_symptom$tcohort,
  diff=serialdata_symptom$mean-generationdata_symptom$mean
)

g5 <- ggplot(incdiffdata_infection) +
  geom_point(aes(tcohort-dodge, mean)) +
  geom_errorbar(aes(tcohort-dodge, ymin=lwr, ymax=upr), width=0) +
  geom_point(data=sergendiff_infection, aes(tcohort+dodge, mean), col=2) +
  geom_errorbar(data=sergendiff_infection, aes(tcohort+dodge, ymin=lwr, ymax=upr), width=0, col=2) +
  geom_hline(yintercept=0, lty=2) +
  scale_x_continuous("Infector's time of infection (day)", limits=c(0, 82), expand=c(0, 0)) +
  scale_y_continuous("Mean difference in incubation periods (days)", limits=c(-4, 2), expand=c(0, 0),
                     sec.axis = sec_axis(~., "Mean difference in\nserial and generation interval (days)")) +
  ggtitle("E") +
  theme(
    panel.grid = element_blank(),
    axis.line.y.right = element_line(color=2),
    axis.ticks.y.right = element_line(color=2),
    axis.text.y.right = element_text(color=2),
    axis.title.y.right = element_text(color=2)
  )

g6 <- ggplot(incdiffdata_symptom) +
  geom_point(aes(tcohort-dodge, mean)) +
  geom_errorbar(aes(tcohort-dodge, ymin=lwr, ymax=upr), width=0) +
  geom_point(data=sergendiff_symptom, aes(tcohort+dodge, mean), col=2) +
  geom_errorbar(data=sergendiff_symptom, aes(tcohort+dodge, ymin=lwr, ymax=upr), width=0, col=2) +
  geom_hline(yintercept=0, lty=2) +
  scale_x_continuous("Infector's time of infection (day)", limits=c(0, 82), expand=c(0, 0)) +
  scale_y_continuous("Mean difference in incubation periods (days)", limits=c(-8, 6), expand=c(0, 0),
                     sec.axis = sec_axis(~., "Mean difference in\nserial and generation interval (days)")) +
  ggtitle("F") +
  theme(
    panel.grid = element_blank(),
    axis.line.y.right = element_line(color=2),
    axis.ticks.y.right = element_line(color=2),
    axis.text.y.right = element_text(color=2),
    axis.title.y.right = element_text(color=2)
  )

gfinal <- ggarrange(g1, g2, g3, g4, g5, g6, nrow=2, byrow=FALSE, draw=FALSE)

ggsave("figure_delay.pdf", gfinal, width=12, height=6)
