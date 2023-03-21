sink("midterm_output.tex")

setwd("C:/Users/Kevin/Documents/GitHub/causal_inference23/midterm")
load("data_midterm.RData")
library(tidyverse)

lm(fact_immigrants_w3_correct_dk0 ~ log_tweets_immigration +
             fact_immigrants_w2_correct_dk0 +
             woman+ age+ lowerclass+ profile_education_age+ 
             white_british+ married+ newsnight_freq+ religious+
             internet_freq_inc+newspaper_type, data = df)

# Transform the treatment variable into a binary variable for whether the 
# respondent was exposed to more or less than the median number of tweets 
# in the sample. Conduct some kind of balance test (graphical, regression-based, 
# whatever you think makes sense) of the covariates across these new binary 
# treatment and control groups. Where are there issues of common support?


df <- df %>% mutate(binary_tweets = ifelse(log_tweets_immigration >= median(
  log_tweets_immigration), 1, 0))

plot((MatchIt::matchit(binary_tweets ~ woman+ age+ lowerclass+ profile_education_age+ 
                         white_british+ married+ newsnight_freq+ religious+
                         internet_freq_inc+newspaper_type, data = na.omit(df))), type = "density")


# In the sense developed in Aronow and Samii (2016), calculate the Regression Weights for 
# each observation in the full regression defined in the code on line 9. What % of the 
# total weight is contributed by the top 10% of the observations?

fit.treat <- lm(log_tweets_immigration ~
     fact_immigrants_w2_correct_dk0 +
     woman+ age+ lowerclass+ profile_education_age+ 
     white_british+ married+ newsnight_freq+ religious+
     internet_freq_inc+newspaper_type, data = df)

fit.full <- lm(fact_immigrants_w3_correct_dk0 ~ log_tweets_immigration +
     fact_immigrants_w2_correct_dk0 +
     woman+ age+ lowerclass+ profile_education_age+ 
     white_british+ married+ newsnight_freq+ religious+
     internet_freq_inc+newspaper_type, data = df)

w <- as.numeric(residuals(fit.treat))^2

sum((sort(w, decreasing = TRUE)[1:round(.11*(length(w)))]))/sum(w)

## Randomization inference question

ate <- c()

df_copy <- df

for (i in 1:1000) {
  df_copy$log_tweets_immigration <- sample(df$log_tweets_immigration, replace = FALSE)
  eff <- coef(lm(fact_immigrants_w3_correct_dk0 ~ log_tweets_immigration +
                   fact_immigrants_w2_correct_dk0 +
                   woman+ age+ lowerclass+ profile_education_age+ 
                   white_british+ married+ newsnight_freq+ religious+
                   internet_freq_inc+newspaper_type, data = df_copy))[2]
  ate <- c(ate, eff)
}


real_eff <- coef(lm(fact_immigrants_w3_correct_dk0 ~ log_tweets_immigration +
                 fact_immigrants_w2_correct_dk0 +
                 woman+ age+ lowerclass+ profile_education_age+ 
                 white_british+ married+ newsnight_freq+ religious+
                 internet_freq_inc+newspaper_type, data = df))[2]

sum(real_eff < ate)/1000

sink(file = NULL)