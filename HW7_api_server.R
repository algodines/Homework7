## Write your API endpoints in this file

# HW7_api_server.R
# API for  HW 4 no-show prediction model

library(plumber)
library(jsonlite)
library(caret)

# Load in Data
no_show_model <- readRDS("no_show_model.rds")

dow_levels <- levels(no_show_model$trainingData$day_of_week)

# Helper: turn JSON body into a data frame with correct columns/types
parse_newdata <- function(body) {
  dat <- fromJSON(body)
  if (!is.data.frame(dat)) {
    dat <- as.data.frame(dat)
  }
  
  needed <- c("age", "days_wait", "address", "provider_id",
              "specialty", "day_of_week")
  
  dat <- dat[needed]
  
  # Make sure day_of_week is a factor with the same levels as training
  dat$day_of_week <- factor(dat$day_of_week, levels = dow_levels)
  
  dat
}

#* @apiTitle No-show Model API

# predict_prob

#* Predict probability of no-show
#* @serializer contentType list(type = "application/octet-stream")
#* @post /predict_prob
function(req) {
  newdata <- parse_newdata(req$postBody)
  
  prob_mat <- predict(no_show_model, newdata, type = "prob")
  
  # positive class is "1"
  probs <- as.numeric(prob_mat[, "1"])
  
  serialize(probs, connection = NULL)
}

# predict_class

#* Predict class (0/1) for no-show
#* @serializer contentType list(type = "application/octet-stream")
#* @post /predict_class
function(req) {
  newdata <- parse_newdata(req$postBody)
  
  class_pred <- predict(no_show_model, newdata, type = "raw")
  
  # factor -> numeric 0/1 
  if (is.factor(class_pred)) {
    class_vec <- as.numeric(as.character(class_pred))
  } else {
    class_vec <- as.numeric(class_pred)
  }
  
  serialize(class_vec, connection = NULL)
}
