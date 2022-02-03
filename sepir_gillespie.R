sample2 <- function(x, size) {
  if(length(x)==1) {
    rep(x, size)
  } else{
    sample(x, size, replace=TRUE)
  }
}

sepir_gillespie <- function(size=1000,
                            beta_p=0.5,
                            beta_s=0.5,
                            sigma=1/3,
                            gamma_p=1/3,
                            gamma_s=1/3,
                            I0=10,
                            seed = NULL){
  
  if (!is.null(seed)) set.seed(seed)
  
  if (missing(I0)) {
    if (missing(I0)) stop("specify the initial conditions")
  }
  
  state <- rep("S", size)
  state[1:I0] <- "E"
  
  t_infected <- t_infectious <- t_symptom <- t_recovered <- rep(NA, size)
  t_infected[1:I0] <- 0
  
  t_gillespie <- NULL
  
  infected_by <- rep(NA, size)
  
  stop <- FALSE
  
  t <- 0
  
  while (!stop) {
    t_gillespie <- c(t_gillespie, t)
    
    S <- sum(state=="S")
    E <- sum(state=="E")
    Ip <- sum(state=="Ip")
    Is <- sum(state=="Is")
    R <- sum(state=="R")
    
    rate_total <- beta_p * S * Ip/size + beta_s * S * Is/size + E * sigma + Ip * gamma_p + Is * gamma_s
    
    tnext <- rexp(1, rate_total)
    
    whichnext <- which(c(rmultinom(1, 1, c(
      beta_p * S * Ip/size,
      beta_s * S * Is/size,
      E * sigma,
      Ip * gamma_p,
      Is * gamma_s
    )/rate_total))==1)
    
    if (whichnext==1) {
      ww <- sample2(which(state=="S"), 1)
      state[ww] <- "E"
      t_infected[ww] <- t
      
      ww2 <- sample2(which(state=="Ip"), 1)
      infected_by[ww] <- ww2
      
    } else if (whichnext==2) {
      ww <- sample2(which(state=="S"), 1)
      state[ww] <- "E"
      t_infected[ww] <- t
      
      ww2 <- sample2(which(state=="Is"), 1)
      infected_by[ww] <- ww2
      
    } else if (whichnext==3) {
      ww <- sample2(which(state=="E"), 1)
      state[ww] <- "Ip"
      t_infectious[ww] <- t
      
      
    } else if (whichnext==4) {
      ww <- sample2(which(state=="Ip"), 1)
      state[ww] <- "Is"
      t_symptom[ww] <- t
      
      
    } else if (whichnext==5) {
      ww <- sample2(which(state=="Is"), 1)
      state[ww] <- "R"
      t_recovered[ww] <- t
    }
    
    t <- t + tnext
    
    stop <- all(state %in% c("S", "R"))
  }
  
  data.frame(
    t_infected=t_infected,
    t_infectious=t_infectious,
    t_recovered=t_recovered,
    infected_by=infected_by
  )
}
