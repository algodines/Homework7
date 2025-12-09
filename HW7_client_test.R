## Write your client test code here

# code to test Homework 7 API

library(readr)
library(dplyr)
library(lubridate)
library(httr)
library(jsonlite)


# Load test data & create predictors 

test <- read_csv("test_dataset.csv.gz", show_col_types = FALSE) |>
  mutate(
    appt_time = ymd_hms(appt_time),
    appt_made = ymd(appt_made),
    days_wait = as.numeric(difftime(appt_time, appt_made, units = "days")),
    day_of_week = wday(appt_time, label = TRUE)
  )

newdata <- test |>
  select(age, days_wait, address, provider_id, specialty, day_of_week)

newdata_json <- toJSON(newdata)

base_url <- "http://127.0.0.1:8080"

# Call predict_prob

cat("Calling /predict_prob ...\n")

resp_prob <- POST(
  url   = paste0(base_url, "/predict_prob"),
  body  = newdata_json,
  encode = "raw",
  content_type_json()
)

cat("Status code:", status_code(resp_prob), "\n")

if (status_code(resp_prob) == 200) {
  prob_vec <- unserialize(content(resp_prob, "raw"))
  cat("First 10 predicted probabilities:\n")
  print(head(prob_vec, 10))
} else {
  cat("Server said:\n")
  cat(content(resp_prob, as = "text"), "\n")
}

# Call predict_class

cat("\nCalling /predict_class ...\n")

resp_class <- POST(
  url   = paste0(base_url, "/predict_class"),
  body  = newdata_json,
  encode = "raw",
  content_type_json()
)

cat("Status code:", status_code(resp_class), "\n")

if (status_code(resp_class) == 200) {
  class_vec <- unserialize(content(resp_class, "raw"))
  cat("First 10 predicted classes:\n")
  print(head(class_vec, 10))
} else {
  cat("Server said:\n")
  cat(content(resp_class, as = "text"), "\n")
}
