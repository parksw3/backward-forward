library(dplyr)
library(ggplot2); theme_set(theme_bw(base_family = "Times"))
library(egg)
source("generation.R")
load("sepir_simulation.rda")

generationdata <- genfun(sepir_simulation)
serialdata <- serfun(sepir_simulation)

truncatetime <- 1:8 * 10

inclist <- genlist <- serlist <- vector("list", length(truncatetime))

incubationdata_truncated <- lapply(truncatetime, function(x) {
  sepir_simulation %>%
    filter(t_symptom < x) %>%
    filter(!is.na(t_infected)) %>%
    mutate(
      incubation=t_symptom-t_infected
    ) %>%
    summarize(
      mean=mean(incubation),
      lwr=t.test(incubation)[[4]][1],
      upr=t.test(incubation)[[4]][2],
      tmeasure=x
    )
}) %>%
  bind_rows

generationdata_truncated <- generationdata %>%
  filter(!is.na(tinfector)) %>%
  arrange(tinfectee) %>%
  mutate(
    cm=cumsum(generation)/1:n()
  )

serialdata_truncated <- serialdata %>%
  filter(!is.na(tinfector)) %>%
  mutate(
    ttrunc=pmax(tinfectee, tinfector)
  ) %>%
  arrange(ttrunc) %>%
  mutate(
    cm=cumsum(serial)/1:n()
  )

ggplot(incubationdata_truncated) +
  geom_line(aes(t_symptom, cm)) +
  scale_x_continuous()

ggplot(generationdata_truncated) +
  geom_line(aes(tinfectee, cm))

ggplot(serialdata_truncated) +
  geom_line(aes(ttrunc, cm))

