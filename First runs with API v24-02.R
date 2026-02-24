install.packages(c("httr", "jsonlite", "rjstat", "tidyverse"))

library(httr)
library(jsonlite)
library(rjstat)
library(tidyverse)

# -----------------------------
# 1) API request URL
# -----------------------------
url <- "https://data.ssb.no/api/pxwebapi/v2/tables/03024/data?lang=en&outputFormat=json-stat2&valuecodes[Tid]=*&valuecodes[VareGrupper2]=*&valuecodes[ContentsCode]=*&heading=Tid,ContentsCode&stub=VareGrupper2"

# -----------------------------
# 2) Fetch data from SSB API
# -----------------------------
response <- httr::GET(url)

# Check if request succeeded
if (response$status_code != 200) {
  stop("API request failed with status: ", response$status_code)
}

# Parse the body as text
json_text <- httr::content(response, as = "text", encoding = "UTF-8")

# Convert JSON-stat2 to R dataframe
ssb_data <- rjstat::fromJSONstat(json_text)

# If it returns a list with multiple datasets, take the first:
if (is.list(ssb_data) && length(ssb_data) > 1) {
  ssb_data <- ssb_data[[1]]
}

# View data structure
head(ssb_data)

# -----------------------------
# 3) Clean and reshape data
# -----------------------------
# Assume typical variables in JSON-stat: "Tid", "ContentsCode", "VareGrupper2", and "value"
df <- ssb_data %>%
  rename(
    Time = Tid,
    Commodity = VareGrupper2,
    Measure = ContentsCode,
    Price = value
  )

# If time is encoded as year/week, convert accordingly
# Example: 2024W10 -> date
df <- df %>%
  mutate(
    # Extract year and week
    Year = as.integer(substr(Time, 1, 4)),
    Week = as.integer(substr(Time, 6, 7)),
    # Create a proper date as the Monday of that week
    Date = as.Date(paste(Year, Week, 1, sep = "-"), "%Y-%U-%u")
  ) %>%
  arrange(Date)

# -----------------------------
# 4) Plot price per kg over time
# -----------------------------
ggplot(df, aes(x = Date, y = Price, color = Commodity)) +
  geom_line() +
  labs(
    title = "Price per kg over time",
    x = "Date",
    y = "Price per kg (NOK)",
    color = "Commodity Group"
  ) +
  theme_minimal()

