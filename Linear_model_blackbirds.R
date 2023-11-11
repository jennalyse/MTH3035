# Blackbird linear model

# Blackbird data
bb_data <- data.frame(
  years = 1964:1983,
  bbptotals = c(61, 230, 349, 492, 493, 573, 471, 440, 351, 345, 217, 194, 109, 74, 151, 59, 43, 59, 17, 13),
  bbjtotals = c(609, 2054, 3210, 2676, 3669, 3824, 4822, 3555, 3675, 3400, 2292, 1715, 1521, 1029, 1798, 1031, 933, 903, 679, 634),
  bbatotals = c(1320, 2340, 4391, 6467, 4925, 5738, 7414, 6488, 6531, 4233, 4032, 3390, 2912, 1910, 2724, 2212, 1522, 1236, 1378, 732)
)
# Linear models
bb_lm_list <- lapply(bb_data[, -1], function(response) lm(response ~ years, data = bb_data))
# Summarise results
lapply(bb_lm_list, summary)
