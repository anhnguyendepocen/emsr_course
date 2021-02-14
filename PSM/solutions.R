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

