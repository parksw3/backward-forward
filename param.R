beta_p <- 0.5
beta_s <- 0.5
duration <- 2.5
sigma <- 1/duration
gamma_s <- 1/duration
gamma_p <- 1/duration
shape <- 2

p <- 1-(beta_s/gamma_s)/(beta_p/gamma_p + beta_s/gamma_s)

true_inc_mean <- 1/sigma + 1/gamma_s
true_gen_mean <- 1/sigma+1/gamma_s+1/gamma_p * (1-p)

gendist <- function(x, p=p, duration=duration) {
  p * (- pgamma(x, 4, rate=2/duration) + pgamma(x, 2, rate=2/duration))/duration +
    (1-p) * (- pgamma(x, 6, rate=2/duration) + pgamma(x, 4, rate=2/duration))/duration
}




N <- 30000
I0 <- 10
seed <- 101