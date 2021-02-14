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


# BASIC LIVE/DIE SIMULATION =====================

n_iter = 10 # number of years to iterate through
n_pop = 12  # number of people
pD = 0.1    # probability of death in any period

# trace matrix
m_T = matrix(data = NA, 
             nrow = n_iter, 
             ncol = n_pop, 
             dimnames = list(paste("year",1:n_iter),
                             paste("person",1:n_pop)))

m_T[1,] = 1 # everyone is alive at start of period 1, whole row = 1


# for period 2 to the end, keep track of who is alive or not.
for(i in 2:n_iter){
  
  # with probability 0.1 the person dies
  dontDie <- runif(n = n_pop, min = 0, max = 1) >= pD
  
  # move to the next period
  m_T[i, ] = m_T[i-1, ] * dontDie
  
  
}

colSums(m_T)



# EXERCISES

# 1. create a loop that prints the time in 5 second intervals (you will want Sys.time function from base R).

# 2. create a loop that prints the 3rd to 100th values of the Fibonacci sequence.

# 3. An investor makes an investment of £1M and wants to estimate the value after 40 years given that:
#    a) the investment has a mean annual return which is sampled from a normal distribution with mean = 7% & SD of 8%.
#    b) she will withdraw  £50,000 per year.

# 4. Run the simulation 10000 times, what is the probability that she will run out of money?













