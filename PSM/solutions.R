#==========================#
# Making Health Economic Evaluation Shiny: A tutorial
# Robert Smith, Paul Schneider & Sarah Bates
# Dark Peak Analytics
# Feb 2021
#==========================#

# A SCRIPT FOR EXTRA MATERIAL & SOLUTIONS TO EXERCISES.

# random number function

getRN <- function(n = 1000){
  
  data.frame("normal" =   rnorm(n = n, mean = 1, sd = 0.2),
             "uniform" =  runif(n = n, min = 0, max = 1))
}

getRN()




# fibinnaci

n1 = 0
n2 = 1
count = 2

print(n1)
print(n2)

for(x in 1:100){
  nth = n1 + n2
  print(nth)
  # update values
  n1 = n2
  n2 = nth
}



# 3. An investor makes an investment of £1M and wants to estimate the value after 40 years given that:
#    a) the investment has a mean annual return which is sampled from a normal distribution with mean = 7% & SD of 8%.
#    b) she will withdraw  £50,000 per year.

rm(list = ls())
capital_invest = 1e+6
mean_return = 0.07
sd_return = 0.08
n_t = 40
withdrawal = 5e+4


v_capital = matrix(data = NA, nrow = n_t, ncol = 1)
v_capital[1,1] <- capital_invest

for(t in 2:n_t){
  
  start = v_capital[t-1,1] - withdrawal
  
  end = start * (1 + rnorm(n = 1, mean = mean_return, sd = sd_return))
  
  v_capital[t, 1] = end
}



# 4. Run the simulation 10000 times, what is the probability that she will run out of money?


runSim <- function(capital_invest = 1e+6,
                   mean_return = 0.07,
                   sd_return = 0.08,
                   n_t = 40,
                   withdrawal = 5e+4){
  
  v_capital = matrix(data = NA, nrow = n_t, ncol = 1)
  v_capital[1,1] <- capital_invest

for(t in 2:n_t){
  
  start = v_capital[t-1,1] - withdrawal
  
  end = start * (1 + rnorm(n = 1, mean = mean_return, sd = sd_return))
  
  v_capital[t, 1] = end
}
  
  return(min(v_capital)<=0)
  
}

# initilise with zero
bankrupt = 0

# run 10,000 iterations 
n_sim = 1e4

# loop through and add up each time the investor runs out of money.
for(x in 1:n_sim){
  
  bankrupt = bankrupt + runSim()
  
}

paste0("The investor run out of money: ", (bankrupt/n_sim)*100,"% of the time")



## STOP HERE FOR PRESENTATION



rm(list = ls())



# NESTED CUSTOM FUNCTIONS #=============================

# we can nest a function inside of another function
# here we use the R function 'mean'

# nested function, to be used within function below.
# this function does step 1 and step 2
getMean_sqrt = function(argOne, 
                        argTwo){
  
  # step 1
  argsMean = mean(x = c(argOne, argTwo) ) # take the mean of the two arguments.
  
  # step 2
  sqrtMean = sqrt(argsMean) # take the square root of the mean
  
  return(sqrtMean)
  
}

# outer function... does step 3
fourthFunc <- function(argOne = 1,  # argument 1 has a default value of 1
                       argTwo,      # argument 2 has no default value, it must be specified
                       funcOne = getMean_sqrt
){
  
  # step 3
  out = funcOne(argOne, argTwo) * 10   # do the first two steps (in the nested function)
  
  return(out) # return an object from the function
  
}

# test this
fourthFunc(argOne = 1, argTwo = 10)  # the default function is getMean_sqrt

# why define the function as an argument?
# because now we could simply change the inner function 
getMax_sqrt = function(argOne, 
                       argTwo){
  
  # step 1
  argsMax = max(x = c(argOne, argTwo) ) # take the maximum of the two arguments.
  
  # step 2
  sqrtMax = sqrt(argsMax) # take the square root of the maximum
  
  return(sqrtMax)
  
}

# this function uses the maximum of the two arguments
fourthFunc(argOne = 1,
           argTwo = 10,
           funcOne = getMax_sqrt)

# this function is the same but uses the mean of the two arguments
fourthFunc(argOne = 1,
           argTwo = 10,
           funcOne = getMean_sqrt)

# this would allow us to assess differences in STRUCTURAL CHANGES TO MODELS.



# RECAP #=========================================

# what we have done is very simple... we have a function that does either this...
sqrt( mean(x = c(1, 2)) ) * 10

fourthFunc(argOne = 1,
           argTwo = 2,
           funcOne = getMean_sqrt)



# or this:
sqrt( max(x = c(1, 2)) ) * 10

fourthFunc(argOne = 1,
           argTwo = 2,
           funcOne = getMax_sqrt)

# HOWEVER:
# the principle is the same for more complex models...

# we may have:
# runPSA as the outer function and runMarkovModel as an inner function.

# there may be tens of 'utility functions' that perform different steps of the model





# TROUBLE-SHOOTING #========================
# we may wish to see what is going on inside of a function, in order to troubleshoot.
# for this we can use 'print'

fifthFunc = function(argOne, 
                     argTwo, 
                     n_sim){
  
  # initialize an empty matrix with one column
  v_argOne_max =  matrix(data = NA, nrow = n_sim, ncol = 1)
  
  # for n_sum number of model runs
  for(i in 1:n_sim){
    
    # sample a random number between 0 and argOne
    underOne = runif(n = 1, min = 0, max = argOne)
    
    # sample a random number between 0 and argTwo
    underTwo = runif(n = 1, min = 0, max = argTwo)
    
    # print the two values
    print(paste("Value 1 = ", round(underOne,2),"...", "Value 2 = ", round(underTwo,2)))
    
    # Is the first random number bigger than the second?
    v_argOne_max[i,1] =    underOne > underTwo
    
    # print this
    print(paste("Val 1 > Val 2: ", v_argOne_max[i,1]))
    
  }
  
  return(mean(v_argOne_max[,1]))
  
}

# run the function to check it is working as we anticipate...

fifthFunc(argOne = 5,
          argTwo = 7,
          n_sim = 5)





# EXERCISES ==========================



# 1) create a function which compliments the user





# 2) create a function which returns the Nth largest of X random numbers, where N and X are arguments to the function





# 3) create a function which calculates the discounted QALYs from a vector of QALYs and a discount rate,
#    both of which are arguments to the function ... ignore half cycle correction (for now).




# 4) create a function that returns a data-frame with two columns and N (user defined) rows where the values in column 1 are selected from a 
#    a truncated normal distribution, and the values in column 2 are selected from a uniform distribution as below.
#   A) Truncated Normal, Mean = 0.6, Min = 0, Max = 1, SD = 0.2
#   B) Uniform, Min = 1000, Max = 5000

# extension - allow the user of the function to change column one to a beta distribution from outside the function. 
#   C) beta, alpha (shape 1) = 60, beta (shape 2) = 40

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























