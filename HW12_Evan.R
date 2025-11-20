library(readxl)
library(tidyverse)


# Objective 1 -------------------------------------------------------------

bsb_data <- read_xlsx('BSB_tagging_data.xlsx')

str(bsb_data) #date is in POSIXct

#remove rows with missing date for recapture
bsb_no_na_date <- bsb_data %>% 
  filter(!is.na(Date_at_recapture)) 
 
#pull out month
bsb_capture_after_july <- bsb_no_na_date %>% 
  mutate(
    month_at_recapture = as.numeric(format(Date_at_recapture, "%m")) 
    )

#Filter for after July, add changed_sex column
bsb_capture_after_july <- bsb_capture_after_july %>% 
  filter(month_at_recapture > 7, Sex_at_capture == "F") %>% 
  mutate(
    changed_sex = if_else(Sex_at_capture == Sex_at_recapture, "no_change", "change")
  )

#Proportion summary
bsb_change_summary <- bsb_capture_after_july %>% 
  summarise(
    n_obs = n(),
    n_changed = sum(changed_sex == 'change'),
    n_unchanged = sum(changed_sex == 'no_change'),
    changed_prop = sum(changed_sex == 'change') / n_obs
  )

probability = seq(0, 1, length = 200)
density <- dbeta(probability, shape1 = 10, shape2 = 21) #shape1 = # changed + 1, shape2 = # unchanged + 1

#A. Probability density function
plot(probability, density, type = 'l')

#B.
conf_int <- c(qbeta(.025, 10, 21), qbeta(.975, 10, 21))
conf_int

# Objective 2 -------------------------------------------------------------
#Make sex change binary variable
bsb_capture_after_july <- bsb_capture_after_july %>% 
  filter(month_at_recapture > 7, Sex_at_capture == "F") %>% 
  mutate(
    changed_sex_binary = if_else(Sex_at_capture == Sex_at_recapture, 0, 1)
  )

#Look at data
hist(bsb_capture_after_july$Length_at_capture)

ggplot(bsb_capture_after_july, aes(x = Length_at_capture, y = changed_sex)) +
  geom_boxplot()

#Make GLM
length_glm <- glm(changed_sex_binary ~ Length_at_capture, family = 'binomial', data = bsb_capture_after_july)
#default is link = logit

#A. 
summary(length_glm) #p = .11, weak / no evidence
coef(length_glm)

#B. 
#For every mm of length increase, log odds is expected to increase by .044. 

#C. 

predictors <- data.frame(Length_at_capture = bsb_capture_after_july$Length_at_capture,
                         Changed_sex = bsb_capture_after_july$changed_sex_binary)

predicted_odds <- predict.glm(length_glm, newdata = predictors, type = 'response')

length_and_odds_df <- data.frame(predictors, predicted_odds)

ggplot(length_and_odds_df, aes(x = Length_at_capture)) +
  geom_line(aes(y = predicted_odds), color = "blue") +
  geom_point(aes(y = Changed_sex * 1), color = "black") +       
  scale_y_continuous(
    name = "Pred. Odds of Change",
    sec.axis = sec_axis(~ . / 1, name = "Changed Sex (1 = Changed)")  #wasn't sure how to do this, used ChatGPT
  ) +
  theme_minimal(base_size = 15)
