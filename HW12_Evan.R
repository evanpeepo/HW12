library(readxl)
library(tidyverse)

bsb_data <- read_xlsx('/Users/evanpeepo/Documents/Classes/ZOO800 Quant methods/BSB_tagging_data.xlsx')

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
