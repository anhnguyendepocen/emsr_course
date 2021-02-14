#==========================#
# Making Health Economic Evaluation Shiny: A tutorial
# Robert Smith, Paul Schneider & Sarah Bates
# Dark Peak Analytics
# Feb 2021
#==========================#

# CUSTOM FUNCTIONS

rm(list = ls()) # clear workspace

# we are going to work from the simplest function towards more complicated functions.
# For more details see: https://r4ds.had.co.nz/functions.html


# SIMPLEST FUNCTION - ONE ARGUMENT #=============================

# basic structure of a function
firstFunc <- function(argOne = 1 ){  # argument 1 has a default value of 1
  
  # < SOME CODE TO DO SOMETHING USEFUL > #
  
  # step 1
  sqrtArg = sqrt(argOne) # take the square root of the argument
  
  # step 2
  out = sqrtArg * 10   # take the square root and multiply by 10.
  
  return(out) # return an object from the function
  
}

# we can print the code for our function:
firstFunc # the last line shows us the environment


# now we can run our function
firstFunc() # the output from our function is 10 if we don't define argOne since the default is 10
firstFunc(argOne = 10) # however we can set argOne to whatever we want (within reason).
firstFunc(argOne = -1) # here we tried to take the square root of a negative number.

# Note: we cannot access the objects created within the function environment from our global environment
sqrtArg


# TWO ARGUMENTS #=============================

secondFunc <- function(argOne = 1,  # argument 1 has a default value of 1
                       argTwo       # argument 2 has no default value, it must be specified
                       ){
  
  # < SOME CODE TO DO SOMETHING USEFUL > #
  
  # step 1
  sumArgs = argOne + argTwo # add the two numbers
  
  # step 2
  sqrt_sumArgs = sqrt(sumArgs) 
  
  # step 3
  out = sqrt_sumArgs * 10   # take the square root and multiply by 10.
  
  return(out) # return an object from the function
  
}


secondFunc() # note that we cannot simply run this without defining the second argument.
secondFunc(argOne = 1, argTwo = 2) # but we can run this without defining argument one (default = 1).


# Note: we can still break the function:
secondFunc(argOne = 1, argTwo = -5)


# NESTED FUNCTIONS #=============================
# we can nest a function inside of another function
# here we use the base function mean: https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/mean

thirdFunc <- function(argOne = 1,  # argument 1 has a default value of 1
                      argTwo       # argument 2 has no default value, it must be specified
                      ){
  
  # < SOME CODE TO DO SOMETHING USEFUL > #
  
  # step 1
  argsMean = mean(x = c(argOne, argTwo) ) # take the mean of the two arguments.
  
  # step 2
  sqrtMean = sqrt(argsMean) # take the square root of the mean
  
  # step 3
  out = sqrtMean * 10   # take the square root of the mean and multiply by 10.
  
  return(out) # return an object from the function
  
}


thirdFunc(argTwo = 2) # but we can run this without defining argument one (default = 1).


# IMPORTANT
# don't name your function the same name as a baseR function unless you intend to overwrite...
# for example, don't create a function called 'mean' which insults the user.

mean = function(){
  "You are a sad strange little man, and you have my pity. (Toy story)"
}

# QUESTION: WHY DOES THIS BREAK????
thirdFunc(argTwo = 2) 

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



















