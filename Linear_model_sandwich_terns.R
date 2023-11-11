# Sandwich data code for linear models
# sbpmatrix, sbjmatrix, and sbamatrix have already been defined (see Sandwich_tern_matrices)
sb_data_list <- lapply(list(sbpmatrix, sbjmatrix, sbamatrix), function(matrix) {
  data.frame(years = rep(1970:1990, each = 21), response = as.vector(matrix))
})

# Linear models
sb_lm_list <- lapply(sb_data_list, function(data) lm(response ~ years, data = data))

# Summarize results
lapply(sb_lm_list, summary)
