#Set your own working directory
setwd("C:/Users/diego/Documents/R/Projects/GitHub_Projects/Optimization/Capacitated Lot Sizing Problem")

# Import lpSolve package
library(lpSolve)

#Import required packages
library(dplyr)
library(ROI)
library(ROI.plugin.symphony)
library(ompr)
library(ompr.roi)

#Set demands
d <- c(60,100,140,200,120,80)

#Set fixed cost
K <- c(180,140,160,160,170,190)

#Set unit production cost
c <- c(7,7,8,7,6,10)

#Set unit inventory cost
h <- c(1,1,2,2,2,2)

#Set capacities
C <- rep(10000,6)

#Set number of periods (time instant t=0 is included)
T <- length(d)+1

#Set initial inventory Level
Io <- 0

#Set final inventory Level
In <- 0

#Build Model
Model <- MIPModel() %>%
  add_variable(q[t], t = 1:T , type = "continuous", lb=0) %>%       #define variables
  add_variable(I[t], t = 1:T , type = "continuous", lb=0) %>% 
  add_variable(y[t], t = 1:T , type = "binary") %>% 
  set_objective(expr = sum_expr(K[t-1]*y[t] + c[t-1]*q[t] + h[t-1]*I[t],t = 2:T), sense = "min") %>% #define objective
  add_constraint(I[t] == Io, t=1) %>%        #define constraints
  add_constraint(I[t] == In, t=T) %>%
  add_constraint(q[t] + I[t-1] == d[t-1] + I[t], t = 2:T) %>% 
  add_constraint(q[t] <= C[t-1]*y[t], t = 2:T) %>% 
  solve_model(with_ROI(solver = "symphony", verbosity = 1))


#Model summary
##Status
print(paste("Model status is:", Model$status))

##Objective Function
print(paste("Objective value:", objective_value(Model)))

## Variables
for(r in 2:T) {
  print(paste("Time istant t=", r - 1))
  
  tmp_y <- get_solution(Model, y[t]) %>%
    filter(variable == "y", t == r) %>%
    select(value)
  
  
  print(paste("--->y[", r - 1, "] =", tmp_y))
  
  
  tmp_I <- get_solution(Model, I[t]) %>%
    filter(variable == "I", t == r) %>%
    select(value)
  
  
  print(paste("--->I[", (r - 1), "] =", tmp_I))
  
  
  tmp_q <- get_solution(Model, q[t]) %>%
    filter(variable == "q", t == r) %>%
    select(value)
  
  
  print(paste("--->q[", (r - 1), "] =", tmp_q))
  
  
}
  


