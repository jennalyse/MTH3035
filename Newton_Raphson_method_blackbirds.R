# Newton Raphson method for blackbirds
# Define likelihood function computing matrices ('phia', 'phi1', 'lambda1', 'lambda2') 
# using logistic function ('expit')
likelihood_function_matrix <- function(pars_in, Datamatrix) {
  phia <- matrix(c(0, expit(pars_in[2:20])), n1, n2, byrow = TRUE)
  phi1 <- matrix(expit(pars_in[21]), n1, n2, byrow = TRUE)
  lambda1 <- matrix(expit(pars_in[22:41]), n1, n2, byrow = TRUE)
  lambda2 <- matrix(expit(pars_in[22:41]), n1, n2, byrow = TRUE)
  return(ringlik(Datamatrix, phia, phi1, lambda1, lambda2))
}

# Optimise for the different life stages 
optimise_likelihood <- function(likelihood_function, Datamatrix) {
  result <- optim(par = initial_pars, fn = likelihood_function, method = "BFGS", Datamatrix = Datamatrix)
  cat("Estimated Parameters:", result$par, "\n")
  return(result)
}
# For pullis
result_bbpmatrix <- optimize_likelihood(likelihood_function_matrix, Datamatrix)
# For juveniles
result_bbjmatrix <- optimize_likelihood(likelihood_function_matrix, Datamatrixj)
# For adults
result_bbamatrix <- optimize_likelihood(likelihood_function_matrix, Datamatrixa)

# Print results
cat("Estimated Parameters for bbpmatrix:", result_bbpmatrix$par, "\n")
cat("Estimated Parameters for bbjmatrix:", result_bbjmatrix$par, "\n")
cat("Estimated Parameters for bbamatrix:", result_bbamatrix$par, "\n")

