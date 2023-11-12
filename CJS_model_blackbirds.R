# cjs model for blackbirds
bbptotals <- c(2488, 3583, 4518, 4315, 4347, 4517, 3448, 3461, 3745, 3139, 2811, 3166, 3141, 3535, 3646, 3918, 3403, 3510, 2927, 4150)
bbjtotals <- c(4408, 4621, 4121, 5069, 5438, 5127, 4281, 3866, 4108, 4275, 3439, 4175, 4652, 5202, 4762, 5148, 5669, 5532, 5566, 6942)
bbatotals <- c(1994, 2471, 2459, 3131, 2991, 3069, 3202, 3450, 3784, 3903, 3795, 5045, 4432, 4181, 4156, 4052, 4632, 4924, 4126, 4670)

capture_histories <- rep(1, sum(bbptotals + bbjtotals + bbatotals))
bb_data <- data.frame(ch = capture_histories, sex = rep(c("P", "J", "A"), times = c(sum(bbptotals), sum(bbjtotals), sum(bbatotals))))

cjs_likelihood <- function(params, data) {
  phi_1 <- exp(params[1]) / (1 + exp(params[1]))  
  phi_2 <- exp(params[2]) / (1 + exp(params[2]))  
  
  p_initial <- phi_1 * phi_2
  likelihood <- log(p_initial)
  for (t in 2:length(data)) {
    p_t <- (1 - phi_1) * phi_2
    likelihood <- likelihood + data[t] * log(p_t)
  }
  return(-likelihood) 
}
bb_data <- c(100, 80, 70, 60, 50)
initial_params <- c(0, 0)
result <- optim(par = initial_params, fn = cjs_likelihood, data = bb_data, method = "BFGS")
cat("Optimal parameters found at:\n")
print(result$par)
