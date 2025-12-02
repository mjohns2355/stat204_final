# Load the survey ratings data
survey_response <- readr::read_csv('https://raw.githubusercontent.com/yawgmoth/HanabiData/refs/heads/master/participants.csv')

# add some libraries
library(ggplot2)
library(tidyverse)

# Preview the data
head(survey_response)

# cleanup and remove NAs
survey_complete <- survey_response[complete.cases(survey_response),]

survey_complete <- survey_response[complete.cases(survey_response),]
survey_complete$ai <- survey_complete$ai %>% as.factor()
survey_complete$gamer <- survey_complete$gamer %>% as.factor()
survey_complete $age <- survey_complete$age %>% as.factor()



# plot skill vs score
ggplot(survey_complete, aes(x = skill, y = score)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# aggregate score by perceived skill of AI
score_skill <- aggregate(score ~ skill, data = survey_complete, FUN = mean)
score_skill

# aggregated score by whether they liked the AI
score_like_df <- data.frame(aggregate(score ~ like, data = survey_complete, FUN = mean))
View(score_like_df)

# aggregate score by how intentional they thought the AI was
aggregate(score ~ intention, data = survey_complete, FUN = mean)
View(data.frame(aggregate(score ~ intention, data = survey_complete, FUN = mean)))

# aggregate score by which AI was used
aggregate(score ~ ai, data = survey_complete, FUN = mean)
View(data.frame(aggregate(score ~ ai, data = survey_complete, FUN = mean)))

# plot aggregated score by perceived skill
plot(score_skill)


model <- aov(score ~ ai, survey_complete)
summary(model)

survey_complete$ai <- factor(survey_complete$ai)
survey_complete$skill <- factor(survey_complete$skill)

model2 <- aov(score ~ skill + ai + skill:ai, survey_complete)
summary(model2)

# anova
model3 <- aov(score ~ skill, survey_complete)
summary(model3)


model4 <- aov(score ~ ai, survey_complete)
summary(model4)

# Tukey test compares all pairwise options
tukey_results <- TukeyHSD(model4)
print(tukey_results) 


# did users rate intentional AI more intentionally?

intention_df <- survey_complete %>% filter(ai == "intentional")
full_df <- survey_complete %>% filter(ai == "full")
outer_df <- survey_complete %>% filter(ai == "outer")

intention_df %>% count(intention)
full_df %>% count(intention)
outer_df %>% count(intention)

nrow(intention_df)
nrow(full_df)
nrow(outer_df)

ggplot(intention_df, aes(x = intention)) + 
  geom_bar(fill = "steelblue") + 
  labs(title = "Distribution of Intentional AI rating", x = "Intention Rating", y = "Count")

ggplot(full_df, aes(x = intention)) + 
  geom_bar(fill = "orange2") + 
  labs(title = "Distribution of Full AI rating", x = "Intention Rating", y = "Count")

ggplot(outer_df, aes(x = intention)) + 
  geom_bar(fill = "plum3") + 
  labs(title = "Distribution of Outer AI rating", x = "Intention Rating", y = "Count")

# figure out table to display summary statistics