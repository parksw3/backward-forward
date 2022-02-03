genfun <- function(data) {
  data.frame(
    tinfector=data$t_infected[data$infected_by],
    tinfectee=data$t_infected,
    generation=data$t_infected - data$t_infected[data$infected_by]
  )
}

serfun <- function(data) {
  data.frame(
    tinfector=data$t_symptom[data$infected_by],
    tinfectee=data$t_symptom,
    serial=data$t_symptom - data$t_symptom[data$infected_by]
  )
}
