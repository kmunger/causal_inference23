
  # IV Excercise 
  
library(haven) # Read .dta files
library(data.table) # For working with data
library(fixest) # For regressions
library(binsreg) # For binscatter
library(ggplot2)



setwd("C:/Users/Kevin/Documents/GitHub/causal_inference23/code/")

#This lab will get your IV hands dirty with data from the Angrist and Krueger
#(1991) quarter-of-birth study.
#To start, load the angrist_krueger.dta data  R 

df <- haven::read_dta("angrist_krueger.dta")

## Load data
data <- as.data.table(df)

data[, qob_1 := (qob == 1)]
data[, qob_2 := (qob == 2)]
data[, qob_3 := (qob == 3)]
data[, qob_4 := (qob == 4)]

##Estimate the bivariate statistical relationship between log wages (lwage) and completed years of schooling (educ) using OLS. 
#Report your coefficient and robust standard error. 
# ---- OLS and Binscatter ------------------------------------------------------

feols(
  lwage ~ educ, # Regression formula
  data,
  vcov = "hc1" # ,r
)

binscatter <- binsreg(data$lwage, data$educ)

# plot and add labels
binscatter$bins_plot +
  labs(y = "Log wages", x = "Years of Completed Schooling")

###Estimate the returns to schooling using an indicator for individuals being born in the first quarter of the year as an instrument for completed years of schooling (and no other controls).


# Formula y ~ exogenous | fixed effects | endogenous ~ instrument
# 1 = constant, 0 = no fixed effects
feols(
  lwage ~ 1 | 0 | educ ~ qob_1,
  data,
  vcov = "hc1"
)

#Estimate the average log wages and completed years of schooling for individuals who are and are not born in the first quarter. 
#Check that you can get the 2SLS estimate in 2 manually from these numbers, 
#using the Wald IV formula:
  
data[,
     .(n = .N, mean = mean(lwage), sd = sd(lwage), min = min(lwage), max = max(lwage)),
     by = qob_1
]
data[,
     .(n = .N, mean = mean(educ), sd = sd(educ), min = min(educ), max = max(educ)),
     by = qob_1
]

(5.157450 - 5.148471) / (11.52515 - 11.39960 )

  



#Collapse your data into means of log wages and completed years of
#schooling by quarter of birth. Plot average log wages 
#against average years of schooling. What is the slope 
#of this relationship? Are you surprised? Explain what we’ve shown here



# collapse data by qob
collapsed <- data[,
                  .(lwage = mean(lwage), educ = mean(educ)),
                  by = qob
]

# plot means
plot(collapsed$educ, collapsed$lwage)

# add regression line
abline(feols(lwage ~ educ, collapsed))



# ---- Putting the 2S in 2SLS --------------------------------------------------

feols(
  lwage ~ 1 | yob | educ ~ qob_1,
  data,
  vcov = "hc1"
)

first_stage <- feols(educ ~ i(qob_1) | yob, data)

data[, educ_hat := predict(first_stage)]

feols(
  lwage ~ educ_hat | yob,
  data,
  vcov = "hc1"
)




###############RDD DWI Exercise

#See assignment here


#https://github.com/Mixtape-Sessions/Causal-Inference-1/blob/main/Lab/DUI%20Recidivism/README.md


df <- haven::read_dta("hansen_dwi.dta")




