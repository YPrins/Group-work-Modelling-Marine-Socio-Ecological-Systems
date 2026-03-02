#load packages needed
#install if necessary
library(data.table)
library(deSolve)
library(ggplot2)
library(rootSolve)
library(tidyr)
library(dplyr)
library(grid)
library(httr)
library(jsonlite)
library(tidyverse)


#### write a R function for the RK4 method (Numerical solver for non-linear models)
rk4_solver <- function(f, x0, times) {
  x <- numeric(length(times))
  x[1] <- x0
  
  for (i in 2:length(times)) {
    dt <- times[i] - times[i - 1]
    t <- times[i - 1]
    xi <- x[i - 1]
    
    k1 <- f(xi, t)
    k2 <- f(xi + 0.5 * dt * k1, t + 0.5 * dt)
    k3 <- f(xi + 0.5 * dt * k2, t + 0.5 * dt)
    k4 <- f(xi + dt * k3, t + dt)
    
    x[i] <- xi + (dt / 6) * (k1 + 2*k2 + 2*k3 + k4)
  }
  
  return(x)
}

#### use deSolve for non-linear models
# define a differential equation with a that depends on state
# this is basically dx/dt = 0.5 x
# note that the model returns an object of class 'list'. This is needed for the ode function
model <- function(t, x, parms) {list( 0.5 * x)}

# define time steps
times <- seq(0, 10, by = 0.5)

# define the initial state: this start is a "named vector" that contains the starting state 
start <- c(x = 1)

# solve with deSolve function solver
# ode needs the starting state (first argument, the times, the model, and the parameters)
# in this example, we have hardcoded the growth parameter (being 0.5) in the model,
# so no parameters are needed.
out <- deSolve::ode(y = start, times = times, func = model, parms = NULL)


# If you want to select the desired method manually, see the examples for euler and rk4 below
#out_euler <- deSolve::ode(y = state, times = times, func = model, parms = NULL, method = 'euler')
#out_rk4   <- deSolve::ode(y = state, times = times, func = model, parms = NULL, method = 'rk4')

#### Logistic growth model
logistic_ode <- function(time, state, parms) {
  with(as.list(c(state, parms)), {
    dN <- r * N * (1 - N / K)
    list(dN)
  })
}

# Define parameters of the system and its starting state 
parms_l <- c(r = 0.3, K = 150)
state_l <- c(N = 10)

# Call ODE for times 0 to 60 in steps of 0.5, the function is the logistic_ode, and the parameters are parms_l 
out_l   <- ode(y = state_l, times = seq(0, 60, 0.5), func = logistic_ode, parms = parms_l)
out_l_df <- as.data.frame(out_l)

# Plot 
ggplot(out_l_df, aes(x = time, y = N)) + 
  geom_line() + labs(title = "Logistic growth (ODE)") +
  theme_minimal()

#### Local and industrial fisheries example (tutorial 3), possibly aquaculture and fisheries
fi_ode <- function(time, state, parms) {
  with(as.list(c(state, parms)), {
    dN <-r1 * N * (1 - N / k1) - a1*N*M
    dM <- r2* M * (1 - M / k2) - a2*N*M
    list(c(dN, dM))
  })
}

# Define parameters of the system and a couple of starting states
parms_fi <- c(r1 = 5.0, r2=3.0, k1=5.0, k2=3.0, a1=3.0, a2=1.0)
state_fi1 <- c(N = 4.0, M = 1.0)
state_fi2 <- c(N = 0.25, M = 1.0)
state_fi3 <- c(N = 3.0, M = 2.0)
state_fi4 <- c(N = 1.0, M = 0.5)

# Call ODE for times 0 to 10 in steps of 0.05, the function is  
out_fi1 <- ode(y = state_fi1, times = seq(0, 10, 0.01), func = fi_ode, parms=parms_fi)
out_fi2 <- ode(y = state_fi2, times = seq(0, 10, 0.01), func = fi_ode, parms=parms_fi)
out_fi3 <- ode(y = state_fi3, times = seq(0, 10, 0.01), func = fi_ode, parms=parms_fi)
out_fi4 <- ode(y = state_fi4, times = seq(0, 10, 0.01), func = fi_ode, parms=parms_fi)

head(out_fi1)

out_fi_df <- as.data.frame(out_fi1) %>% 
  pivot_longer(-time, names_to = "State_variable", values_to = "Numbers")

ggplot(out_fi_df, aes(x = time, y = Numbers, color = State_variable)) + 
  geom_line() + 
  labs(title = "Two types of fisheries competing", y="Numbers (hundreds)") + 
  theme_minimal()

#### Biomass modelling (tutorial 6)
F_BiomassGrowth <-
  function(S) {
    S + growthRate*S*(1-S/maximumStock)*
      rlnorm(numDraws, meanlog = 0, sdlog = standardDeviation)
  }

growthRate         <- 0.1
maximumStock       <- 100
standardDeviation  <- 1

#### Entry-exit model
catchRate          <- 0.1
targetMortality    <- 0.05
targetEscapement   <- 50
fishPrice          <- 1
costsOfFishing     <- 3
fixedCatch         <- 3 
maxFractionCaught  <- 0.9
entryExitParm      <- 0.1

F_HCR_morty  <- function(B) {
  Q        <- B*targetMortality
  return(Q)
}

F_HCR_escap  <- function(B) {
  Q        <- B-targetEscapement
  Q[Q<0] <- 0
  return(Q)
}

F_catch <-
  function(B,E) {
    B*(1-exp(-catchRate*E))
  }

F_reverseCatch <-
  function(B,C) {
    log(B/(B-C))/catchRate
  }

F_Rents <-
  function(C,E) {
    fishPrice*C-costsOfFishing*E
  }

F_FLT_constCatch <-
  function(B,Q){
    out_catch                                <- rep(fixedCatch,times=length(Q))
    out_catch[out_catch>Q]                   <- Q[out_catch>Q]
    out_catch[out_catch>maxFractionCaught*B] <- maxFractionCaught*B[out_catch>maxFractionCaught*B]
    out_effort                               <- F_reverseCatch(B,out_catch)
    return(list(effort=out_effort,catch=out_catch))
  }

F_FLT_entryExit <-
  function(B,E,R,Q) {
    out_effort               <- E+entryExitParm*R
    out_effort[out_effort<0] <- 0
    out_catch                <- F_catch(B,out_effort)
    out_catch[out_catch > Q] <- Q[out_catch > Q]
    out_effort               <- F_reverseCatch(B,out_catch)
    return(list(effort=out_effort,catch=out_catch))
  }

biomass    <- array(dim=c(numYears,numDraws))
escapement <- array(dim=c(numYears,numDraws))
effort     <- array(dim=c(numYears,numDraws))
TAC        <- array(dim=c(numYears,numDraws))
catch      <- array(dim=c(numYears,numDraws))
rents      <- array(dim=c(numYears,numDraws))

initEscapement <- 50
escapement[1,] <- initEscapement
effort[1,]     <- fixedEffort
catch[1,]      <- F_FLT_constEffrt(initEscapement,Inf)$catch
rents[1,]      <- F_Rents(catch[1,],effort[1,])

for(iYear in 2:numYears) {
  biomass[iYear,]    <- F_BiomassGrowth(escapement[iYear-1,])
  TAC[iYear,]        <- Inf
  temp               <- F_FLT_constEffrt(biomass[iYear,],TAC[iYear,])
  catch[iYear,]      <- temp$catch 
  effort[iYear,]     <- temp$effort
  rents[iYear,]      <- F_Rents(catch[iYear,],effort[iYear,])
  escapement[iYear,] <- biomass[iYear,]-catch[iYear,]
}

plotQuants <- function(inpMatrix,name) {
  distrMatrix <-
    apply(inpMatrix, 1, quantile, probs = c(0.05, 0.5, 0.95),
          na.rm = TRUE)
  plot(distrMatrix[2,],type="l",
       ylim=c(min(0,min(distrMatrix)),max(distrMatrix)),
       xlab="Time step",ylab=name)
  title(main=name)
  polygon(c(1:dim(distrMatrix)[2],rev(1:dim(distrMatrix)[2])),
          c(distrMatrix[3,],rev(distrMatrix[1,])),col = "grey75", border = FALSE)
  lines(distrMatrix[2,])
  lines(inpMatrix[,1],col="red")
}

plotQuants(rents[-1,],"Figure 4.1: Rents")
plotQuants(effort[-1,],"Figure 4.2: Effort")
plotQuants(catch[-1,],"Figure 4.3: Catch")
plotQuants(biomass[-1,],"Figure 4.4: Biomass")
meanRents   <- round(mean(rents[numYears,]),3)
meanBiomass <- round(mean(round(biomass[numYears,]),3))
meanCatch   <- round(mean(catch[numYears,])/mean(biomass[numYears,]),3)