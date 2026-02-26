#only if needed #install.packages(c("httr", "rjstat", "tidyverse"))

library(httr)
library(rjstat)
library(tidyverse)
library(dplyr)

# -----------------------------
# 1) Helper function to fetch PxWeb JSON-stat2 via GET
# -----------------------------
fetch_pxweb <- function(url) {
  res <- httr::GET(url)
  
  if (res$status_code != 200) {
    stop("API request failed: ", res$status_code)
  }
  
  json_text <- httr::content(res, as = "text", encoding = "UTF-8")
  
  parsed <- rjstat::fromJSONstat(json_text)
  
 # if (is.list(parsed)) parsed <- parsed[[1]]
  
  as.data.frame(parsed)
}

# -----------------------------
# 2) API URLs
# -----------------------------
url_prices <- "https://data.ssb.no/api/pxwebapi/v2/tables/03024/data?lang=en&outputFormat=json-stat2&valuecodes[Tid]=*&valuecodes[VareGrupper2]=*&valuecodes[ContentsCode]=*&heading=Tid,ContentsCode&stub=VareGrupper2"
allprices <- fetch_pxweb(url_prices)

url_catches <- "https://data.ssb.no/api/pxwebapi/v2/tables/09243/data?lang=en&outputFormat=json-stat2&valuecodes[Tid]=*&valuecodes[Region]=*&codelist[Region]=vs_Landet&valuecodes[ContentsCode]=Laks&heading=Tid,ContentsCode&stub=Region"
allcatches <- fetch_pxweb(url_catches)

url_farmed <- "https://data.ssb.no/api/pxwebapi/v2/tables/03024/data?lang=en&outputFormat=json-stat2&valuecodes[Tid]=*&valuecodes[VareGrupper2]=*&valuecodes[ContentsCode]=Vekt&heading=Tid,ContentsCode&stub=VareGrupper2"
allfarmed <- fetch_pxweb(url_farmed)
# -----------------------------------------
# 3) Filter and aggregate price data + plot
# -----------------------------------------
prices_filtered <- allprices %>%
  rename(
    commodity   = `commodity group`,
    Description = contents,
    Price       = value
  ) %>%
  
  # Keep only price observations
  filter(Description == "Price per kilo (NOK)") %>%
  
  # Extract year from 2000U01 format
  mutate(
    Year = as.integer(substr(week, 1, 4))
  ) %>%
  
  #Exclude 2026 data as it is currently incomplete 
  filter(Year < max(Year)) %>%
  
  # Aggregate to yearly average price
  group_by(Year, commodity, Description) %>%
  summarise(
    AvgPrice = mean(Price, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  # Fuse the two commodity categories into one yearly average price
  group_by(Year, Description) %>%
  summarise(
    AvgPrice = mean(AvgPrice, na.rm = TRUE),
    .groups = "drop"
  )

#Plot the graph
price_graph <- ggplot(prices_filtered, aes(x = Year, y = AvgPrice)) +
  geom_line() +
  labs(
    title = "Farmed Salmon Price per kg Over Years",
    x = "Year",
    y = "Average price per kg (NOK)"
  ) +
  theme_minimal()

print(price_graph)
# -----------------------------------------
# 4) Filter and aggregate farm data + plot
# -----------------------------------------
farmed_filtered <- allfarmed %>%
  rename(
    commodity        = `commodity group`,
    Description      = contents,
    Weight_exported  = value
  ) %>%
  
  # Extract year from 2000U01 format
  mutate(
    Year = as.integer(substr(week, 1, 4))
  ) %>%
  
  #Exclude 2026 data as it is currently incomplete 
  filter(Year < max(Year)) %>%
  
  # Sum all exported weight within each year
  group_by(Year, commodity, Description) %>%
  summarise(
    Total_Weight_Exported = sum(Weight_exported, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  # Fuse the two commodity categories into one yearly exported weight
  group_by(Year, Description) %>%
  summarise(
    Total_Weight_Exported = sum(Total_Weight_Exported, na.rm = TRUE),
    .groups = "drop"
  )

#Plot the graph
farmed_graph <- ggplot(farmed_filtered, aes(x = Year, y = Total_Weight_Exported)) +
  geom_line() +
  labs(
    title = "Farmed Salmon Export Over Years",
    x = "Year",
    y = "Weight Exported (tonnes)"
  ) +
  theme_minimal()

print(farmed_graph)
# -----------------------------
# 5) Catches trend plot
# -----------------------------
#Plot the graph
catch_graph <- ggplot(allcatches, aes(x = as.numeric(year), 
                                      y = value, 
                                      group = 1)) +
  geom_line() +
  labs(
    title = "Salmon Catches Over Years (kg)",
    x = "Year",
    y = "Total Catch (kg)"
  ) +
  theme_minimal()

print(catch_graph)

# -----------------------------
# 6) Combine all datasets into one dataset and plot this
# -----------------------------
#Transform tonnes to kg
farmed_filtered_kg <- farmed_filtered %>%
  mutate(
    Total_Weight_kg = Total_Weight_Exported * 1000
  )

#Make sure data is compatible
prices_filtered <- prices_filtered %>%
  mutate(Year = as.integer(Year))

#Clean up the allcatches dataset
catches_filtered <- allcatches %>%
  rename(
    Year        = year,
    Catch_in_kg = value
  ) %>%
  group_by(Year)

#Make sure data is compatible
catches_filtered <- catches_filtered %>%
  mutate(Year = as.integer(Year))

#Combine all datasets. Combine by year and keep all years, including no overlap
combined_datasets <- farmed_filtered_kg %>%
  select(Year, Total_Weight_kg) %>%
  full_join(
    prices_filtered %>%
      select(Year, AvgPrice),
    by = "Year"
  ) %>%
  full_join(
    catches_filtered,
    by = "Year"
  ) %>%
  arrange(Year)

#Clean up the combined dataset
combined_datasets <- combined_datasets %>%
  select(-region, -contents) %>%   # remove columns
  rename(
    "Exported farmed salmon (kg)" = Total_Weight_kg,
    "Average price per kg farmed salmon (NOK)"  = AvgPrice,
    "Caught salmon (kg)"          = Catch_in_kg
  )

#Plot the trends of the salmon export and corresponding price levels
FarmedAndPrice_graph <- ggplot(combined_datasets, aes(x = Year)) +
  
  # Price level trend
  geom_line(aes(
    y = `Average price per kg farmed salmon (NOK)`,
    color = "Average price per kg (NOK)"
  ),
  size = 1.2
  ) +
  
  # Exported farmed salmon (scaled) trend
  geom_line(aes(
    y = `Exported farmed salmon (kg)` / 10000000,
    color = "Exported farmed salmon (scaled)"
  ),
  size = 1.2
  ) +
  
  scale_y_continuous(
    name = "Average price per kg (NOK)",
    sec.axis = sec_axis(~ . * 10000000,
                        name = "Salmon volume (kg)")
  ) +
  
  scale_color_manual(
    name = "Legend",  # title of the legend
    values = c(
      "Average price per kg (NOK)" = "darkgreen",
      "Exported farmed salmon (scaled)" = "pink"
    )
  ) +
  
  labs(
    title = "Salmon Price and Quantity of Exported Farmed Salmon",
    x = "Year"
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "top"
  )

plot(FarmedAndPrice_graph)

##Plot yearly catches, export, and price fluctuations next to each other
#First clean up the data
combined_long <- combined_datasets %>%
  pivot_longer(
    cols = -Year,
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  mutate(
    Variable = factor(
      Variable,
      levels = c(
        "Caught salmon (kg)",
        "Exported farmed salmon (kg)",
        "Average price per kg farmed salmon (NOK)"
      )
    )
  )

#Plot the data
facet_graph <- ggplot(combined_long, aes(x = Year, y = Value)) +
  geom_line(size = 1.2, color = "steelblue") +
  
  facet_wrap(
    ~ Variable,
    scales = "free_y",
    labeller = label_wrap_gen(width = 25)   # adjust width if needed
  ) +
  
  labs(
    title = "Salmon Price, Exported Farmed Salmon, and Catches",
    x = "Year",
    y = NULL
  ) +
  
  theme_minimal() +
  theme(
    strip.text = element_text(size = 11)   # optional: adjust size
  )
plot(facet_graph)
# -----------------------------
# 7) Statistical Analysis??
# -----------------------------
lm(`Average price per kg farmed salmon (NOK)` ~ 
     `Exported farmed salmon (kg)` + 
     `Caught salmon (kg)`, 
   data = combined_datasets)



