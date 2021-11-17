geometricMean <- function(x){

# The geometric mean returns the "average" daily rate of compounding returns
# For example, a principal of $100 that loses 10% on day one ($90) and then gains 10% on day 2 ($99) should not have an average daily return of 0% ((-10 + 10) / 2)
# x must be a vector of rates (0.04 instead of 4(%))
  
  if(!is.numeric(x)){
    
    stop("Vector must be numeric")
    
  }
  
  x <- na.omit(x)
  
  calcFunc <- compose(.dir = "forward",
          ~ map_dbl(.x, ~ . + 1),
          ~ reduce(.x, `*`),
          ~ .x ^ (1/length(x)),
          ~ .x - 1)
  
  geomOutput <- calcFunc(x)
  
  return(geomOutput)
  
}
