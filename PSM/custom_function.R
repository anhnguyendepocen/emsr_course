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

# EXERCISES ==========================

# 1) create a function which compliments the user


