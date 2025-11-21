## Homework 12 - Evan/Victoria/Maggie
library(readxl)
library(dplyr)
library(ggplot2)
library(ggfortify)

# read iggplot2# read in excel file
bass_data <- read_excel("BSB_tagging_data.xlsx")
View(bass_data)

#Objective 1 #

#Plot a probability density function (probability density vs. proportions from 0 to 1) for the proportion of female black sea bass that changed sex out of all those which were recaptured after the end of the spawning season (i.e., after July). Relevant functions: dbeta()

#change the date format for bass_data to then filter it in next step
bass_data$month <- format(bass_data$Date_at_recapture, "%m")

#creating a data frame for only female bass observations that fall after July 30th 2011

female_bass <- bass_data %>%
  filter(Sex_at_capture == "F") %>%
  filter(month > "07")

female_bass$binary <- ifelse(female_bass$Sex_at_recapture == "F", 0, 1)

sex_change <- sum(female_bass$binary == 1, na.rm = TRUE)
n <- nrow(female_bass)

#define parameters for 
alpha <- sex_change + 1
beta <- (n - sex_change) + 1
p <- seq(0, 1, length.out = 1000)

plot(
  p, dbeta(p, alpha, beta),
  type = "l",
  lwd = 2,
  xlab = "Proportion of females that changed sex",
  ylab = "Probability Density",
  main = "Sex Change Proportion of Female (Sex at Capture) Bass"
)

#B. Give the 95% CI for the probability of sex change for these individuals in A above. Relevant functions: qbeta()

#Compute the Confidence Interval (95%) and add intercal to graph for visualizing
ci_95 <- qbeta(c(0.025, 0.975), alpha, beta)
ci_95
abline(v = ci_95, col = "purple", lwd = 2, lty = 2)

#### The 95% confidence interval is (0.17, 0.49) ##################################

# Objective 2 #

#A. Does the length of a female influence its probability of sex change given that it was recaptured after the end of the spawning season? Give a p value to support your answer. Relevant functions: glm()
size_glm <- glm(binary ~ Length_at_capture, data = female_bass, family = binomial )
summary(size_glm)

autoplot(size_glm)

### ANSWER: The p-value of this glm is 0.112 which indicates that there is no statistically significant relationship between the size (length at capture) of female bass and probability of a sex change.

#B. By how much is the log odds of sex change predicted to change for every millimeter increase in length?
### ANSWER: If I am understanding this correctly, the coefficient intercept of the GLM indicates the log odds - so it would be -14.37 and the pvalue of it is 0.0941 (which is not statistically significant)

# The log odds of sex change predicted per mm increase in length is 0.045, which is the slope of the glm model.

#C. Plot the relationship between the probability of sex change for these individuals and length. Overlay the model estimated relationship on the data. Label axes appropriately and provide a figure caption. Relevant functions: predict.glm(), ggplot2()

#create predictions for data set based on the glm and add it as a new column
female_bass$predicted <- predict(size_glm, type = "response")

#plot model estimated relationship over actual dataset

ggplot(female_bass, aes(x = Length_at_capture)) +
  geom_jitter(aes(y = binary, color = "Actual Data"), height = 0.05, alpha = 0.4) +
  geom_line(aes(y = predicted, color = "Modeled Probability"), linewidth = 1.2) +
  scale_color_manual(name = "Data source",
                     values = c("Actual Data" = "blue", 
                                "Modeled Probability" = "green"))+
  scale_y_continuous(
    name = "Binary Value of Change",
    sec.axis = sec_axis( trans=~.*1, name= "Modeled Probability of Change")) +
  theme_minimal()

