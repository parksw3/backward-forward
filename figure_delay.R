library(dplyr)
library(ggplot2); theme_set(theme_bw(base_family = "Times"))
library(egg)
source("generation.R")
load("sepir_simulation.rda")

generationdata <- genfun(sepir_simulation)
serialdata <- serfun(sepir_simulation)

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


generationdata2 <- generationdata %>%
  mutate(
    tinfector2=floor(tinfector/5) * 5 + 5
  ) %>%
  group_by(tinfector2) %>%
  filter(!is.na(tinfector)) %>%
  summarize(
    mean=mean(generation),
    lwr=ifelse(length(generation) > 1, t.test(generation)[[4]][1], NA),
    upr=ifelse(length(generation) > 1, t.test(generation)[[4]][2], NA)
  )

serialdata2 <- serialdata %>%
  mutate(
    tinfector2=floor(tinfector/5) * 5 + 5
  ) %>%
  group_by(tinfector2) %>%
  filter(!is.na(tinfector)) %>%
  summarize(
    mean=mean(serial),
    lwr=ifelse(length(serial) > 1, t.test(serial)[[4]][1], NA),
    upr=ifelse(length(serial) > 1, t.test(serial)[[4]][2], NA)
  )

g1 <- ggplot(daily_incidence) +
  geom_point(aes(t_infected, n)) +
  geom_line(aes(t_infected, n)) +
  geom_point(data=daily_incidence2, aes(t_symptom, n), col=2) +
  geom_line(data=daily_incidence2, aes(t_symptom, n), col=2) +
  scale_x_continuous("Time (day)", limits=c(0, 82), expand=c(0, 0)) +
  scale_y_continuous("Daily number of new infections", limits=c(0, 1270), expand=c(0, 0),
                     sec.axis = sec_axis(~., "Daily number of new symptomatic individuals")) +
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
  scale_x_continuous("Time of infection (day)", limits=c(0, 82), expand=c(0, 0)) +
  scale_y_continuous("Mean forward incubation period (days)", limits=c(0, 10), expand=c(0, 0)) +
  ggtitle("B") +
  theme(
    panel.grid = element_blank()
  )

g3 <- ggplot(generationdata2) +
  geom_point(aes(tinfector2, mean)) +
  geom_errorbar(aes(tinfector2, ymin=lwr, ymax=upr), width=0) +
  scale_x_continuous("Infector's time of infection (day)", limits=c(0, 82), expand=c(0, 0)) +
  scale_y_continuous("Mean forward generation interval (days)", limits=c(0, 10), expand=c(0, 0)) +
  ggtitle("C") +
  theme(
    panel.grid = element_blank()
  )

g4 <- ggplot(serialdata2) +
  geom_point(aes(tinfector2, mean))  +
  geom_errorbar(aes(tinfector2, ymin=lwr, ymax=upr), width=0) +
  scale_x_continuous("Infector's time of symptom onset (day)", limits=c(0, 82), expand=c(0, 0)) +
  scale_y_continuous("Mean forward serial interval (days)", limits=c(0, 10), expand=c(0, 0)) +
  ggtitle("D") +
  theme(
    panel.grid = element_blank()
  )
 
gfinal <- ggarrange(g1, g2, g3, g4, nrow=2)

ggsave("figure_delay.pdf", gfinal, width=8, height=6)
