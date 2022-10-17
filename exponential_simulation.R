source("param.R")

nsamp <- 20000
rvec <- seq(-0.2, 0.2, length.out=11)
tmax <- 40

tmplist <- vector('list', length(rvec))

set.seed(101)
for (i in 1:length(rvec)) {
  quant <- runif(nsamp, 0, 1)
  
  if (rvec[i]==0) {
    inftime <- quant * tmax
  } else {
    inftime <- log(1 + quant * (exp(rvec[i] * tmax) - 1))/rvec[i]
  }
  
  incperiod <- rgamma(nsamp, shape=shape, rate=shape/true_inc_mean)
  
  tmplist[[i]] <- data.frame(
    t_infected=inftime,
    t_symptom=inftime+incperiod,
    tmax=tmax,
    r=rvec[i]
  )
  
}

exponential_simulation <- do.call("rbind", tmplist)

save("exponential_simulation", file="exponential_simulation.rda")
