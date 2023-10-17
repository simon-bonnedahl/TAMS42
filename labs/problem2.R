rm(list = ls()) # this is to clear all previous data
n_params <- list(20, 50, 100, 1000)
p <- 0.3 # values of paramters


bino_cdf <- 0 # the cdf of a Binomial X

# commands below give values of the cdf of a Binomial X at 0 to n

for (n in n_params) {
  for (i in 1:(n + 1))
  {
    bino_cdf[i] <- pbinom(i - 1, size = n, prob = p)
  }


  norm_cdf <- 0 # the corresponding normal cdf
  for (i in 1:(n + 1))
  {
    norm_cdf[i] <- pnorm(i - 1, n * p, sqrt(n * p * (1 - p)))
  }



  print(paste("The values of n are", n))
  print(max(abs(bino_cdf - norm_cdf)))
}


# (ii) Repeat the whole process described in (i) using n = 50, n = 100 and n = 1000.
