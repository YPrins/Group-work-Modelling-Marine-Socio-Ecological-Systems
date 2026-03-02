library(ggplot2)
library(dplyr)
library(tidyr)

# --------------------
# 1. Define parameters
# --------------------
numYears <- 30
Nw_init  <- 1e6           #Taken from literature
Na_init  <- 1132044000    #Taken from Statistics Norway

r      <- 0.96
K      <- 4500000         #Taken from literature
q      <- 0.0025          #Taken from literature
E      <- 70              #Taken from literature

# 1 NOK increase in price level leads to 81,150 kg increase in farmed salmon
# Taken from data analysis performed on Statistics Norway data
gamma  <- 81150

# Prices
P_baseline <- 67         #NOK
P_high     <- 80         #NOK
P_low      <- 50         #NOK

# Realistic alpha values. Alpha represents the (negative) impact of aquaculture.
# on wild stocks
alpha_low  <- 2.5e-4      #When there are little farms, relatively little impact -> low alpha
alpha_mid  <- 4.9824e-4   #Average number of farms (baseline), average impact -> medium alpha
alpha_high <- 6.5e-4      #Many farms, considerable impact -> high alpha

# Higher impact alpha scenario. For the scenario in which alpha is higher than estimated.
alpha_low_high  <- 4e-4
alpha_mid_high  <- 5.75e-4
alpha_high_high <- 9e-4

# ------------------------
# 2. Specify the functions
# ------------------------
#Specify when to take which alpha value. Na is the farmed salmon mass, thereby representing fish farms.
get_alpha <- function(Na, alpha_low, alpha_mid, alpha_high) {
  if (Na < 1.1199e9) {
    return(alpha_low)
  } else if (Na <= 1.3e9) {
    return(alpha_mid)
  } else if (Na > 1.4e9) {
    return(alpha_high)
  }
}

#Specify the wild fisheries ODE
simulateFisheries <- function(P, alpha_low, alpha_mid, alpha_high, scenario_name) {
  
  wildStock <- numeric(numYears)
  farmStock <- numeric(numYears)
  catch     <- numeric(numYears)
  
  wildStock[1] <- Nw_init
  farmStock[1] <- Na_init
  catch[1]     <- q * E * wildStock[1]
  
  for (t in 2:numYears) {
    
    alpha_t <- get_alpha(farmStock[t-1], alpha_low, alpha_mid, alpha_high)
#Specify the harvest equation (catchability * effort * biomass of previous year)
    H <- q * E * wildStock[t-1]
#The ODE for wild fisheries   
    dNw <- r * wildStock[t-1] * (1 - wildStock[t-1]/K) -  # logarithmic growth
      H -                                                 # subtract harvest
      alpha_t * farmStock[t-1]                            # subtract negative farm effect
                                                          # (neg. impact of fish) * number of fish (mass)
    wildStock[t] <- max(wildStock[t-1] + dNw, 0)

#Specify the farmed salmon ODE   
    farmStock[t] <- farmStock[t-1] + gamma * P            # Previous year's farms + attractiveness * price
    
   }
  
  data.frame(
    Year = 1:numYears,
    WildStock = wildStock,
    FarmStock = farmStock,
    Scenario = scenario_name
  )
}

# ------------------------------
# 3. Run the different scenarios
# ------------------------------
Baseline <- simulateFisheries(P_baseline,
                              alpha_low, alpha_mid, alpha_high,
                              "Baseline")

highPrice <- simulateFisheries(P_high,
                               alpha_low, alpha_mid, alpha_high,
                               "Higher Price")

lowPrice <- simulateFisheries(P_low,
                              alpha_low, alpha_mid, alpha_high,
                              "Lower Price")

highAlpha <- simulateFisheries(P_baseline,
                               alpha_low_high, alpha_mid_high, alpha_high_high,
                               "Higher Alpha")

# Combine all scenarios
results <- bind_rows(Baseline, highPrice, lowPrice, highAlpha)

# -----------------------------
# 4.Plot the results
# -----------------------------

# Wild Stock
ggplot(results, aes(x=Year, y=WildStock, color=Scenario)) +
  geom_line(size=1.2) +
  labs(title="Wild Salmon Stock Dynamics Under Influence of Aquaculture",
       y="Wild Stock (kg)") +
  theme_minimal()

# Farm Stock
ggplot(results, aes(x=Year, y=FarmStock, color=Scenario)) +
  geom_line(size=1.2) +
  labs(title="Future Farmed Salmon Production",
       y="Farm Stock (kg)") +
  theme_minimal()
