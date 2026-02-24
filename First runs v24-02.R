####install libraries####
library(ggplot2)
library(tidyverse)
#library add

###set working directory to folder containing all data###
setwd("~/Modelling Marine Socio Ecological Systems")

###Salmon prices data###
# 1. Import data (semicolon separated, skip first row)
salmon <- read.csv("Salmon prices.csv", 
                   sep = ";", 
                   skip = 1,
                   check.names = FALSE)

# 2. Select only one commodity group (example: fresh or chilled)
salmon_fresh <- salmon %>%
  filter(`commodity group` == "Fish-farm bred salmon, fresh or chilled")

# 3. Keep only price columns
price_data <- salmon_fresh %>%
  select(`commodity group`, contains("Price per kilo"))



# 4. Convert from wide to long format
price_long <- price_data %>%
  pivot_longer(
    cols = -`commodity group`,
    names_to = "YearWeek",
    values_to = "Price"
  )

# 5. Extract Year and Week from column names
price_long <- price_long %>%
  mutate(
    YearWeek = gsub(" Price per kilo \\(NOK\\)", "", YearWeek),
    Year = substr(YearWeek, 1, 4),
    Week = substr(YearWeek, 6, 7),
    Date = as.Date(paste(Year, Week, 1, sep = "-"), "%Y-%U-%u")
  )

# 6. Plot price fluctuation
ggplot(price_long, aes(x = Date, y = Price)) +
  geom_line() +
  labs(
    title = "Fluctuation of Salmon Price per Kg",
    x = "Year",
    y = "Price per kg (NOK)"
  ) +
  theme_minimal()
