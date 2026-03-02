
library(deSolve)
library(ggplot2)
library(dplyr)
library(tidyr)
# install.packages("deSolve")   # only if not installed yet
# SES group 12
# Wild stock + fishing effort
# Logistic growth + harvest + aquaculture impact
# ==============================

SES_group_12 <- function(t, state, parms) {
  with(as.list(c(state, parms)), {
    
    # Guards against numerical drift
    Nw <- max(Nw, 0)
    E  <- max(E, 0)
    
    # Harvest
    H <- q * E * Nw
    
    # Wild stock dynamics
    dNw <- r * Nw * (1 - Nw / K) - H - alpha * Na * Nw
    
    # Profit-driven effort dynamics
   # profit <- P * q * E * Nw - C * E
    dE     <- gamma * E * ( P * q * Nw - C )  # changed the equation by factoring out E
    
    list(c(dNw, dE))
  })
}

# -----------------------
# Parameters
# -----------------------
parms_SES12 <- c(
  r     = 1.2,        # given in (1/year)
  K     = 4500000,    # given in Kg of salmon biomass(assuming each salmon weights 4.5kg) 
  q     = 0.0025,     # given in percentage of stock removed per E (in fishing days) -> (1/day)
  P     = 74,         # given in NOK per kg
  C     = 126428,     # given in NOK per fishing day
  gamma = 1e-8,       # attraction/effort responsiveness coefficient 
  alpha = 5e-10,      # aquaculture impact coefficient (had to be lowered immensely from 0.0005 to this. 
                           # BC 0.0005*5000000=2500 as a mortality term w only 1.2(1-0.4)=0.72 growth) 
  Na    = 5000000     # aquaculture stocks given in kg 
)

# Initial conditions
state0_SES12 <- c(
  Nw = 1800000,       # wild stock biomass in kg
  E  = 70             # fishing effort in fishing days
)

# Time grid
times <- seq(0, 70, by = 0.1)

# Simulate
out_SES12 <- ode(y = state0_SES12,
                 times = times,
                 func = SES_group_12,
                 parms = parms_SES12)

df_SES12 <- as.data.frame(out_SES12)

# Long format for plotting
df_long_SES12 <- df_SES12 %>%
  pivot_longer(cols = c("Nw", "E"),
               names_to = "var",
               values_to = "value")

# Plot
ggplot(df_long_SES12, aes(x = time, y = value, color = var)) +
  geom_line() +
  theme_minimal() +
  labs(title =  "stock dynamics with aquaculture impact",
       x = "Time",
       y = NULL,
       color = "Variable")
