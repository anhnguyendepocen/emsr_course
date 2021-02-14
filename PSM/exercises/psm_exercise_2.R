# PSM Exercise 2

# 1. Build your own function to apply a discount rate to costs and qalys
  # disocunting formula:
  # discounted_x = x / (1 + disc_rate) ^ time
  # Set a default for the disc_rate argument of 0.035
# 2. Then use the apply(...) function to apply myDiscounter to the following data matrix: 

c_mat <- read.csv("./PSM/data/c_mat.csv")
time_points = 1:10

# HINT 1: in this case, you don't need a for loop within in the function
# HINT 2: to apply a function to each column of the matrix, set MARGIN = 2

myDiscounter <- function(x, time, disc_rate){
  
}


apply(X = c_mat, MARGIN =  , FUN =  )

