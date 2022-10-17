library(dplyr)
library(bbmle)
source("param.R")
source("fitfun.R")

load("exponential_simulation.rda")

tmax <- unique(exponential_simulation$tmax)

rvec <- unique(exponential_simulation$r)

reslist <- vector('list', length(rvec))

for (i in 1:length(rvec)) {
  fitdata <- exponential_simulation %>%
    filter(r==rvec[i], t_symptom < tmax) %>%
    mutate(
      tdiff = t_symptom - t_infected
    ) %>%
    rename(tmeasure=t_symptom) %>%
    select(tdiff, tmeasure)
  
  ff1 <- fitfun_truncate(fitdata, tmax=tmax, logmean=log(true_inc_mean), logshape=log(shape))
  
  ff2 <- fitfun_truncate_exponential(fitdata, tmax=tmax, logmean=log(true_inc_mean), logshape=log(shape), r=rvec[i])
  
  ff3 <- fitfun_exponential(fitdata, tmax=tmax, logmean=log(true_inc_mean), logshape=log(shape), r=rvec[i])
  
  ff4 <- fitfun_exponential_ltruncate(fitdata, logmean=log(true_inc_mean), logshape=log(shape), r=rvec[i])
  
  reslist[[i]] <- data.frame(
    method=c("right truncation", "right truncation + exponential growth", "exponential growth", "exponential growth + left truncation"),
    est=c(ff1$cmean, ff2$cmean, ff3$cmean, ff4$cmean),
    lwr=c(ff1$lwr, ff2$lwr, ff3$lwr, ff4$lwr),
    upr=c(ff1$upr, ff2$upr, ff3$upr, ff4$upr),
    r=rvec[i]
  )
  
}

fit_exponential_gamma <- reslist %>%
  bind_rows

save("fit_exponential_gamma", file="fit_exponential_gamma.rda")
