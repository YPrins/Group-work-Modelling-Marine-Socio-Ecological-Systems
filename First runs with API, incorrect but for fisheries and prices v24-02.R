# -----------------------------
# Install required packages if needed
# -----------------------------
install.packages(c("httr", "rjstat", "tidyverse"))

library(httr)
library(rjstat)
library(tidyverse)

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
  
  if (is.list(parsed)) parsed <- parsed[[1]]
  
  as.data.frame(parsed)
}

# -----------------------------
# 2) Helper function to fetch PxWeb JSON-stat2 via POST
# -----------------------------
fetch_pxweb_post <- function(url, body_list) {
  res <- httr::POST(
    url,
    body = body_list,
    encode = "json",
    httr::accept_json()
  )
  
  if (res$status_code != 200) {
    stop("API POST request failed: ", res$status_code)
  }
  
  json_text <- httr::content(res, as = "text", encoding = "UTF-8")
  
  parsed <- rjstat::fromJSONstat(json_text)
  
  if (is.list(parsed)) parsed <- parsed[[1]]
  
  as.data.frame(parsed)
}

# -----------------------------
# 3) API URLs
# -----------------------------
url_prices <- "https://data.ssb.no/api/pxwebapi/v2/tables/03024/data?lang=en&outputFormat=json-stat2"

url_catches_post <- "https://data.ssb.no/api/pxwebapi/v2/tables/09243/data?lang=en&outputFormat=json-stat2"

# -----------------------------
# 4) Fetch price data
# -----------------------------
prices_raw <- fetch_pxweb(url_prices)

prices <- prices_raw %>%
  rename(
    Time = Tid,
    Commodity = VareGrupper2,
    Measure = ContentsCode,
    Price = value
  ) %>%
  mutate(
    Year = as.integer(substr(Time, 1, 4)),
    Week = as.integer(substr(Time, 6, 7)),
    Date = as.Date(paste(Year, Week, 1, sep = "-"), "%Y-%U-%u")
  ) %>%
  arrange(Date)

# -----------------------------
# 5) POST request body for catches
# -----------------------------
post_body <- list(
  selection = list(
    list(variableCode = "Tid", valueCodes = list("*")),
    list(variableCode = "Region", valueCodes = list("*"), codelist = "vs_Landet"),
    list(variableCode = "ContentsCode", valueCodes = list("Laks","LaksUnder3kg","Laks3til7kg","LaksOver7kg","PukkellaksKilo"))
  ),
  placement = list(
    heading = list("Tid", "ContentsCode"),
    stub = list("Region")
  )
)

# -----------------------------
# 6) Fetch catches data via POST
# -----------------------------
catches_raw <- fetch_pxweb_post(url_catches_post, post_body)

catches <- catches_raw %>%
  rename(
    Time = Tid,
    Region = Region,
    CatchType = ContentsCode,
    Catch = value
  ) %>%
  mutate(
    Year = as.integer(substr(Time, 1, 4))
  ) %>%
  filter(Region == "Total for Norway") %>%
  select(Year, CatchType, Catch) %>%
  mutate(
    SalmonType = case_when(
      CatchType %in% c("Laks", "LaksUnder3kg", "Laks3til7kg", "LaksOver7kg") ~ "Regular Salmon",
      CatchType == "PukkellaksKilo" ~ "Pink Salmon",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(Year, SalmonType) %>%
  summarise(TotalCatch = sum(Catch, na.rm = TRUE), .groups = "drop")

# -----------------------------
# 7) Price trend plot
# -----------------------------
p1 <- ggplot(prices, aes(x = Date, y = Price, color = Commodity)) +
  geom_line() +
  labs(
    title = "Salmon Price per kg (NOK)",
    x = "Year",
    y = "Price per kg (NOK)",
    color = "Commodity"
  ) +
  theme_minimal()

print(p1)

# -----------------------------
# 8) Catches trend plot
# -----------------------------
p2 <- ggplot(catches, aes(x = Year, y = TotalCatch, color = SalmonType)) +
  geom_line() +
  labs(
    title = "Salmon Catches Over Years (kg)",
    x = "Year",
    y = "Total Catch (kg)",
    color = "Salmon Type"
  ) +
  theme_minimal()

print(p2)

# -----------------------------
# 9) Combined price vs catch graph
# -----------------------------
# Aggregate weekly prices to annual average
prices_annual <- prices %>%
  mutate(Year = as.integer(format(Date, "%Y"))) %>%
  group_by(Year, Commodity) %>%
  summarise(AvgPrice = mean(Price, na.rm = TRUE), .groups = "drop")

combined <- left_join(prices_annual, catches, by = "Year")

p3 <- ggplot(combined, aes(x = Year)) +
  geom_line(aes(y = AvgPrice, color = Commodity), size = 1.2) +
  geom_line(aes(y = TotalCatch / 1000, linetype = SalmonType), size = 1.2) +
  scale_y_continuous(
    name = "Avg Price per kg (NOK)",
    sec.axis = sec_axis(~ . * 1, name = "Total Catch (×1000 kg)")
  ) +
  labs(
    title = "Comparison of Salmon Price vs Catches",
    x = "Year",
    color = "Commodity / Price",
    linetype = "Salmon Type (Catch)"
  ) +
  theme_minimal()

print(p3)