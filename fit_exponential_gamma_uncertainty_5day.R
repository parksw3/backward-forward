library(dplyr)
library(bbmle)
source("param.R")
source("fitfun.R")

load("exponential_simulation.rda")

tuncertainty <- 1

tmax <- unique(exponential_simulation$tmax)

rvec <- unique(exponential_simulation$r)

reslist <- vector('list', length(rvec))

for (i in 1:length(rvec)) {
  print(i)
  fitdata <- exponential_simulation %>%
    filter(r==rvec[i], t_symptom < tmax) %>%
    mutate(
      tstart_lwr=t_infected - tuncertainty,
      tstart_upr=pmin(t_infected + tuncertainty, t_symptom)
    ) %>%
    rename(tmeasure=t_symptom) %>%
    select(tstart_lwr, tstart_upr, tmeasure)
  
  ff1 <- fitfun_truncate2(fitdata, tmax=tmax, logmean=log(true_inc_mean), logshape=log(shape))
  
  ff2 <- fitfun_truncate2_exponential(fitdata, tmax=tmax, logmean=log(true_inc_mean), logshape=log(shape), r=rvec[i])
  
  reslist[[i]] <- data.frame(
    method=c("right truncation", "right truncation + exponential growth"),
    est=c(ff1$cmean, ff2$cmean),
    lwr=c(ff1$lwr, ff2$lwr),
    upr=c(ff1$upr, ff2$upr),
    r=rvec[i]
  )
}

fit_exponential_gamma_uncertainty <- reslist %>%
  bind_rows

save("fit_exponential_gamma_uncertainty", file="fit_exponential_gamma_uncertainty.rda")
