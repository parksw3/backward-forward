library(dplyr)
library(bbmle)
source("param.R")
source("fitfun.R")
source("generation.R")
load("sepir_simulation.rda")

tuncertainy <- 1

incubationdata <- sepir_simulation %>%
  mutate(
    tmeasure=t_symptom,
    tstart_lwr=t_infected - tuncertainy,
    tstart_upr=pmin(t_infected + tuncertainy, t_symptom)
  ) %>%
  select(tmeasure, tstart_lwr, tstart_upr) %>%
  filter(!is.na(tmeasure))

tvec <- 2:8*10
nsim <- 100
nsamp <- 100

fit_incubation_truncate_exponential_uncertainty_1day <- vector('list', length(tvec))

for (i in 1:length(tvec)) {
  incubationdata2 <- incubationdata %>%
    filter(tmeasure < tvec[i])
  
  tmplist_truncate <- vector('list', nsim)
  
  for (j in 1:nsim) {
    incubationdata3 <- incubationdata2[sample(1:nrow(incubationdata2), nsamp),]
    
    print(paste0(i, ", ", j))
    
    ff1 <- try(fitfun_truncate2_exponential(incubationdata3, tmax=tvec[i], logmean=log(true_inc_mean), logshape=0,
                                            r=0.1773))
    
    tmplist_truncate[[j]] <- ff1
  }
  
  fit_incubation_truncate_exponential_uncertainty_1day[[i]] <- tmplist_truncate %>%
    bind_rows(.id="sim")
}

save("fit_incubation_truncate_exponential_uncertainty_1day", 
     file="fit_sepir_gamma_uncertainty_1day_both.rda")
