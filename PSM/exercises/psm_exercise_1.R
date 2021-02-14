# PSM Exercise 1

# Build your own function to compute the Area Under the Curve (AUC)
# use the Trapezoid method:
# auc = ((y2 + y1) / 2) * (x2 - x1)

# Apply your function on the following data:
time_points <- 0:5                      # time points
cum_surv <- c(1,0.58,0.21,0.05,0.01,0)  # cumulative survival


# HINT 1: use a for-loop to go through the vectors time_points and cum_surv
# HINT 2: there are 6 time points, but only 5 intervals, so you have
#         to apply the auc method 'length(x)-1' times


myAUC <- function(x, y){
  # ...
}

myAUC(x = time_points, y = cum_surv)

