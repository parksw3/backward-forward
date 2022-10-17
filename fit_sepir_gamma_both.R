library(dplyr)
library(bbmle)
source("param.R")
source("fitfun.R")
source("generation.R")
load("sepir_simulation.rda")

incubationdata <- sepir_simulation %>%
  mutate(
    tmeasure=t_symptom,
    tdiff=t_symptom-t_infected
  ) %>%
  select(tmeasure, tdiff) %>%
  filter(!is.na(tdiff))

tvec <- 2:8*10
nsim <- 100
nsamp <- 100

fit_incubation_truncate_exponential <- vector('list', length(tvec))

for (i in 1:length(tvec)) {
  incubationdata2 <- incubationdata %>%
    filter(tmeasure < tvec[i])
  
  tmplist_truncate_exponential <- vector('list', nsim)
  
  for (j in 1:nsim) {
    incubationdata3 <- incubationdata2[sample(1:nrow(incubationdata2), nsamp),]
    
    print(paste0(i, ", ", j))
    
    ff1 <- try(fitfun_truncate_exponential(incubationdata3, tmax=tvec[i], logmean=log(true_inc_mean), logshape=0,
                                           r=0.1773))
    
    
    tmplist_truncate_exponential[[j]] <- ff1
  }
  
  fit_incubation_truncate_exponential[[i]] <- tmplist_truncate_exponential %>%
    bind_rows(.id="sim")
  
}

save("fit_incubation_truncate_exponential",
     file="fit_sepir_gamma_both.rda")
