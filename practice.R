# Load the coffee ratings data
survey_response <- readr::read_csv('https://raw.githubusercontent.com/yawgmoth/HanabiData/refs/heads/master/participants.csv')

library(ggplot2)
library(dplyr)

# Preview the data
head(survey_response)

survey_complete <- survey_response[complete.cases(survey_response),]

ggplot(survey_complete, aes(x = skill, y = score)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
