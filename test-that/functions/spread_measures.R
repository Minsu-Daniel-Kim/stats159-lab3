
source("../functions/range-value.R")
# range_value <- function(x, na.rm = FALSE) {
#   max(x, na.rm = na.rm) - min(x, na.rm = na.rm)
# }

spread_measures <- function(x) {
  
  
  # Range, IQR, Std Dev
  return(c(range_value(x), IQR(x), round(sd(x), 1)))
}