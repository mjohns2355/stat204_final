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

score_skill <- aggregate(score ~ skill, data = survey_complete, FUN = mean)
aggregate(score ~ like, data = survey_complete, FUN = mean)
aggregate(score ~ intention, data = survey_complete, FUN = mean)
aggregate(score ~ ai, data = survey_complete, FUN = mean)


plot(score_skill)

model <- aov(score ~ ai, survey_complete)
summary(model)

survey_complete$ai <- factor(survey_complete$ai)
survey_complete$skill <- factor(survey_complete$skill)

model2 <- aov(score ~ skill + ai + skill:ai, survey_complete)
summary(model2)

model3 <- aov(score ~ skill, survey_complete)
summary(model3)

tukey_results <- TukeyHSD(model2)
print(tukey_results)
