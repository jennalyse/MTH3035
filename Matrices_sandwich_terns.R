# Sandwich tern data 1970-1990

# Sandwich terns ringed as pulli (standard model)
sbpmatrix <- matrix(c(
  23, 17, 1, 1, 4, 7, 0, 1, 1, 2, 1, 2, 3, 0, 1, 1, 1, 2, 0, 2, 2,
  NA, 17, 7, 7, 5, 6, 4, 3, 1, 2, 2, 4, 1, 0, 0, 0, 1, 1, 2, 3, 4,
  NA, NA, 24, 10, 4, 1, 2, 1,  0, 0, 1, 0, 2, 2, 0, 1, 0, 1, 0, 1, 2,
  NA, NA, NA, 18, 15, 1, 4, 4, 2, 3, 4, 4, 1, 2, 0, 1, 3, 0, 0, 2, 1,
  NA, NA, NA, NA, 17, 6, 2, 1, 3, 4, 3, 4, 3, 0, 2, 1, 0, 0, 2, 2, 5,
  NA, NA, NA, NA, NA, 24, 10, 5, 3, 4, 0, 3, 2, 1, 0, 2, 2, 2, 2, 1, 4,
  NA, NA, NA, NA, NA, NA, 15, 10, 2, 4, 2, 0, 1, 0, 1, 1, 0, 1, 1, 0, 0,
  NA, NA, NA, NA, NA, NA, NA, 18, 3, 7, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 1,
  NA, NA, NA, NA, NA, NA, NA, NA, 10, 12, 4, 4, 2, 5, 2, 2, 0, 2, 2, 1, 4,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, 14, 2, 0, 1, 1, 0, 1, 1, 1, 1, 0, 2,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 18, 6, 4, 7, 6, 5, 3, 2, 0, 3, 6,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 17, 13, 4, 3, 3, 2, 3, 0, 2, 2,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 17, 7, 6, 2, 1, 2, 1, 3, 2,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 19, 10, 2, 2, 3, 2, 1, 4,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 8, 7, 3, 5, 2, 3, 6,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 4, 6, 4, 1, 1, 6,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 6, 7, 3, 3, 6,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 5, 5, 4, 4,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 7, 4, 3,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 4, 5,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 12
), nrow = 21, ncol = 21, byrow = TRUE)


# Sandwich terns ringed as juveniles (historical model)
sbjmatrix <- matrix(c(
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  NA, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
  NA, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  NA, NA, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  NA, NA, NA, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  NA, NA, NA, NA, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  NA, NA, NA, NA, NA, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
  NA, NA, NA, NA, NA, NA, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  NA, NA, NA, NA, NA, NA, NA, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
  NA, NA, NA, NA, NA, NA, NA, NA, NA, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0, 0, 0, 0, 0, 0, 0, 0, 1,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0, 0, 0, 0, 0, 0, 0, 0, 
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0, 2, 0, 0, 0, 0, 0,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0, 0, 0, 0, 0, 0,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 2, 2, 0, 0, 1,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0, 1, 0, 0,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0, 0, 0,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0, 0,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 1
), nrow = 21, ncol = 21, byrow = TRUE)

n1 <- nrow(sbjmatrix) # number of years of ringing
n2 <- ncol(sbjmatrix) # number of years of recovery

# Sandwich terns ringed as adults (historical model)
sbamatrix <- matrix(c(
  1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  NA, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  NA, NA, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  NA, NA, NA, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  NA, NA, NA, NA, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  NA, NA, NA, NA, NA, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  NA, NA, NA, NA, NA, NA, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  NA, NA, NA, NA, NA, NA, NA, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0, 0, 0, 0, 1, 0, 2, 0, 0,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0, 1, 0, 1, 0, 0, 0, 0,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 1, 0, 0, 0, 0, 0, 0,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0, 0, 0, 0, 0, 0,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 8, 1, 0, 0, 1,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0, 2, 0, 0,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0, 0, 0,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0, 0,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 1
), nrow = 21, ncol = 21, byrow = TRUE)
