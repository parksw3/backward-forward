source("sepir.R")
source("param.R")

sepir_simulation <- sepir(N, beta_p, beta_s, sigma, gamma_p, gamma_s, I0, seed=101)

save("sepir_simulation", file="sepir_simulation.rda")
