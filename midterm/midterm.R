



setwd("C:/Users/Kevin/Documents/GitHub/causal_inference23/midterm/")
load("data_midterm.RData")


lm(fact_immigrants_w3_correct_dk0 ~ log_tweets_immigration +
             fact_immigrants_w2_correct_dk0 +
             woman+ age+ lowerclass+ profile_education_age+ 
             white_british+ married+ newsnight_freq+ religious+
             internet_freq_inc+  newspaper_type, data = df)
