
source("../functions/range-value.R")
source("../functions/center_measures.R")

descriptive_stats <- function(x) {
  
  return (c(center_measures(x), spread_measures(x), sum(is.na(x))))
  
}

