# ==============
# Making Health Economic Modelling Shiny
# Robert Smith, Paul Schneider & Sarah Bates
# University of Sheffield
# contact: info@darkpeakanalytics.com
# ==============

# Matrix multiplication exercise solutions

rm(list = ls())

# define parameters
state_names = c("Healthy", "Sick", "Dead")
n_states = length(state_names)   # the length function is fairly intuitive
n_t = 10

# create transition matrix
m_P <- matrix(data = NA,
              nrow = n_states,
              ncol = n_states,
              dimnames = list(state_names,state_names)
              )

# fill transition matrix with values.
m_P[1, ] <- c(0.8, 0.15, 0.05)  
m_P[2, ] <- c(0, 0.7, 0.3)
m_P[3, ] <- c(0, 0, 1)

# check matrix
m_P

# create Markov Trace matrix.
m_T <- matrix(data = NA,
              nrow = n_t,
              ncol = n_states,
              dimnames = list(paste0("cycle_",1:n_t), state_names))

# check the Markov Trace
m_T


# Initialize the first period of the Markov Trace. (100% of the population starts Healthy).
m_T[1, ] <- c(1, 0, 0)



#   Use matrix multiplication to multiply the first row of the Markov Trace (m_T) by the Transition Matrix (m_P).
#   and store the resulting values in the second row of the Markov Trace.
m_T[2, ] <- m_T[1, ] %*% m_P

# HINT: Since we start with 1 - 0 - 0  we should end up with 0.8 - 0.15 - 0.05 in the second period. 
# if you don't have this something has gone wrong :)


# Use the for-loop function to repeat this process until the matrix is fully populated.
for(t in 2:n_t){
  
  m_T[t, ] <- m_T[t-1, ] %*% m_P
  
}

# check this looks correct
m_T

# more robust, look at summary
summary(rowSums(m_T))

# Wrap this whole process into a function that takes as arguments:
# 1) an initialized Markov Trace.
# 2) transition Matrix 
# arguments.

# create a new markov trace with the same initial values
m_T2 <- m_T * NA
m_T2[1, ] <- c(1, 0, 0)


runMarkov <- function(m_P = NULL, 
                      m_T = NULL){
  
  for(t in 2:nrow(m_T)){
    
    m_T[t, ] <- m_T[t-1, ] %*% m_P
    
  }
  
  return(m_T)
  
  
}

# run this and assign to a new markov trace
m_T2 <- runMarkov(m_P = m_P,  m_T = m_T2)

# check these give the same results
m_T == m_T2

######## Exercise 2 #########

#  1. Create vectors for the costs and utility of each treatment group, assume treatment cost
#  and utility is applied in the sick state of the model

c_H     <- 100            # cost of remaining one cycle in the healthy state
c_S     <- 200            # cost of remaining one cycle in the sick state
c_Trt   <- 50             # cost of treatment in sick state
c_D     <- 0              # cost of being in the death state
u_H     <- 1              # utility when healthy
u_S    <- 0.5             # utility when sick
u_Trt  <- 0.75            # utility when being treated (S1)
u_D     <- 0              # utility when dead

#  2. Estimate mean costs and QALYs for each year (hint: need to use matrix multiplication)


#  3. If you didn't in the the previous step, apply discount weights (hint: need to use transpose function t() and matrix multiplication)
d_r            <- 0.035                    # discount rate  
v_dwe <- v_dwc <- 1 / (1 + d_r) ^ (0:n_t)  # discount weight (equal discounting is assumed for costs and effects)

#  4. Create results table 


