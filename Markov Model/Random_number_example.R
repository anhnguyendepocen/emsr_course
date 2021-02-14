## Generating random number for PSA

n = 10

getRN <- data.frame("normal" =   rnorm(n = n, mean = 1, sd = 0.2),
           "uniform" =  runif(n = n, min = 0, max = 1))

getRN

getRN <- function(n = 10){
  
  data.frame("normal" =   rnorm(n = n, mean = 1, sd = 0.2),
             "uniform" =  runif(n = n, min = 0, max = 1))
}

getRN()

