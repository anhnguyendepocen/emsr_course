#==========================#
# Making Health Economic Evaluation Shiny: A tutorial
# Robert Smith, Paul Schneider & Sarah Bates
# Dark Peak Analytics
# Feb 2021
#==========================#

# FOR LOOPS

# simplest example

# for
for(x in 1:10){
  
  print(x)
  
}


# while
x = 1
while(x <= 10){
  
  print(x)
  
  x = x + 1
  
}


# apply
nums <- matrix(data = 1:10,nrow = 10)

apply(X =  nums, 
      MARGIN = 1,   
      FUN = print)

lapply(X = 1:10,
       FUN = function(x){x+1}
       )




# FOCUSING ON FOR LOOPS #=====================

rm(list = ls())


# for loop with increasing x
for(x in 1:10){
  
  print(x)
  
  x <- x + 1
  
  Sys.sleep(time = 2)
  
}


# EXERCISES

# 1. create a loop that prints the time in 5 second intervals (you will want Sys.time function from base R).










