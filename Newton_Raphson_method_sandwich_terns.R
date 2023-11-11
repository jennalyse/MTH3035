ringlik_sandwich <- function(sbmatrix, phi, phi1, lambda) {
  lik <- 0
  nprob <- rep(1, nrow(sbmatrix))
  
  for (i in 1:nrow(sbmatrix)) {
    prob <- (1 - phi1[i]) * lambda[i]
    
    nprob[i] <- nprob[i] - prob
    lik <- lik + sbmatrix[i] * log(prob)
    
    lik <- lik + sbmatrix[i] * log(nprob[i])
  }
  
  lik <- -lik
  return(lik)
}

pars_fun_sandwich <- function(pars_in, sbmatrix) {
  phi <- matrix(c(0, expit(pars_in[2:19])), n1, n2, byrow = TRUE)
  phi1 <- matrix(expit(pars_in[20]), n1, n2, byrow = TRUE)
  lambda <- matrix(expit(pars_in[21]), n1, n2, byrow = TRUE)
  
  ringlik_sandwich(sbmatrix, phi, phi1, lambda)
}
initialize_parameters <- function(num_params) {
  c(0, rep(0.1, num_params - 2), 0.5, 0.5)
}
# Newton-Raphson optimization function for sandwich bird data
newton_raphson_sandwich <- function(initial_pars, sbmatrix) {
  result <- optim(par = initial_pars, fn = pars_fun_sandwich, sbmatrix = sbmatrix,
                  method = "BFGS", control = list(maxit = 1000))
  
  if (result$convergence == 0) {
    cat("Converged after", result$iterations, "iterations\n")
    return(result$par)
  } else {
    cat("Optimization did not converge\n")
    return(NULL)
  }
}

initial_parameters_sandwich <- initialize_parameters(21)
result_sbp <- newton_raphson_sandwich(initial_parameters_sandwich, sbpmatrix)
cat("Optimal parameters for sbpmatrix found at:\n")
print(result_sbp)
result_sbj <- newton_raphson_sandwich(initial_parameters_sandwich, sbjmatrix)
cat("Optimal parameters for sbjmatrix found at:\n")
print(result_sbj)
result_sba <- newton_raphson_sandwich(initial_parameters_sandwich, sbamatrix)
cat("Optimal parameters for sbamatrix found at:\n")
print(result_sba)

