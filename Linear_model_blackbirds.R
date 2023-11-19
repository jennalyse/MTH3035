# Blackbird linear model
# Blackbird data
bb_data <- data.frame(
	years = 1964:1983,  
	bbptotals = c(2488, 3583, 4518, 4315, 4347, 4517, 3448, 3461, 3745, 3139, 2811, 3166, 3141, 3535, 3646, 3918, 3403, 3510, 2927, 4150),  
	bbjtotals = c(4408, 4621, 4121, 5069, 5438, 5127, 4281, 3866, 4108, 4275, 3429, 4175, 4652, 5202, 4762, 5148, 5669, 5532, 5566, 6942),  
	bbatotals = c(1994, 2471, 2459, 3131, 2991, 3069, 3202, 3450, 3784, 3903, 3795, 5045, 4432, 4181, 4156, 4052, 4632, 4924, 4126, 4670)
)
# Linear models
bb_lm_list <- lapply(bb_data[, -1], function(response) lm(response ~ years, data = bb_data))
# Summarise results
lapply(bb_lm_list, summary)
