
library(deSolve)
library(ggplot2)
library(dplyr)
library(tidyr)
install.packages("deSolve")   # only if not installed yet
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
    profit <- P * q * E * Nw - C * E
    dE     <- gamma * profit
    
    list(c(dNw, dE))
  })
}

# -----------------------
# Parameters
# -----------------------
parms_SES12 <- c(
  r     = 1.2,
  K     = 1000,
  q     = 0.001,
  P     = 1,
  C     = 0.2,
  gamma = 0.01,
  alpha = 0.0005,
  Na    = 200
)

# Initial conditions
state0_SES12 <- c(
  Nw = 600,
  E  = 50
)

# Time grid
times <- seq(0, 200, by = 0.1)

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
  labs(title = "SES group 12: Wild stock dynamics with aquaculture impact",
       x = "Time",
       y = NULL,
       color = "Variable")
```
