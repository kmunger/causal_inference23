
  # Thornton HIV Replication
  
  ## Part 1: Experimental Analysis
  
  #Load data from the following url: `https://raw.github.com/scunning1975/mixtape/master/thornton_hiv.dta`

library(tidyverse)
library(fixest)
library(haven)
df <- haven::read_dta("https://raw.github.com/scunning1975/mixtape/master/thornton_hiv.dta")


#Rebecca Thornton's paper [The Demand for, and Impact of, Learning HIV Status](https://www.rebeccathornton.net/wp-content/uploads/2019/08/Thornton-AER2008.pdf) at *AER* evaluated an experiment in rural Malawi which gave cash incentives for people to follow-up and learn their HIV test result. Thornton’s total sample was 2,901 participants. Of those, 2,222 received any incentive at all. 



#The variable `any` is an indicator variable for if the participant received *any* incentive. The variable `got` denotes that the individual went and *got* their test result information.

#1. Calculate by hand the simple difference in means of `got` based on treatment status `any`. Then use a simple linear regression to see if the result is the same.

# the `with` command lets you access variables in `df` without 
# doing `df$` a bunch
with(df, {
  mean(got[any == 1], na.rm = TRUE) - mean(got[any == 0], na.rm = TRUE)
})

# Cluster SEs by village
feols(got ~ i(any), data = df, cluster = ~villnum)


#2. Following Table 3, we are going to check if the baseline characteristics look the same, on average, between the treated and the control group. Test if the following varaibles differ significantly between treated and the control groups after controlling for `tinc`, `under`, `rumphi`, and `balaka`. 

#- gender via `male` 
#- baseline age via `age` 
#- whether they had HIV in the baseline via `hiv2004`
#- the baseline level of years of education via `educ2004`
#- whether they owned any land in the baseline via `land2004`
#- whether they used condoms in the baseline via `usecondom04`. 
#Interpret whether the results give you confidence in the experiment.

feols(
  c(male, age, hiv2004, educ2004, land2004, usecondom04) ~ 
  i(any) + tinc + i(under) + i(rumphi) + i(balaka), 
  data = df, cluster = ~villnum
) |> 
  etable()



#3. Interestingly, Thornton varied the amount of incentive individuals received (in the variable `tinc`). Let's try comparing treatment effects at different incentive amounts. This is called a `dose response` function. Let's attempt to learn about the dose response function in two ways:

 # a. Calculate a treatment effect using only individuals with `tinc` above 2 (the upper end of incentives). Calculate a treatment effect using indviduals who receive a positive `tinc` but less than 1. Does the treatment effect grow with incentive?

# high incentive
feols(
  got ~ i(any), 
  data = df |> filter(tinc >= 2 | tinc == 0), cluster = ~villnum
)

# low incentive
feols(
  got ~ i(any), 
  data = df |> filter(tinc <= 1 | tinc == 0), cluster = ~villnum
)


#  b. Calculate a linear dose response function by regression `got` on `any` and `tinc`. Note `any` represents the treatment effect at 0 cash incentive (the intercept) and `tinc` represents the marginal change in treatment effect from increasing `tinc`. 

# linear dose-response curve
feols(
  got ~ i(any) + tinc, 
  data = df, cluster = ~villnum
)





## Part 2: Randomization Inference

#1. Estimate the treatment effect of any cash incentive on receiving test results. Perform randomization-based inference to calculate an approximate p-value for the estimate.

df <- df |> filter(!is.na(any))

permuteHIV <- function(df, random = TRUE) {
  # Shuffle `any`
  if(random == TRUE) {
    df$any <- sample(df$any, replace = FALSE)
  }
  
  # `with` lets you access variables in tb without doing tb$ a bunch
  ate <- with(df, {
    mean(got[any == 1], na.rm = TRUE) - mean(got[any == 0], na.rm = TRUE)
  })
  
  return(ate)
}

# Observed treatment effect
permuteHIV(df, random = FALSE)

n_iterations <- 1000

# Run iterations
ate <- c(permuteHIV(df, random = FALSE))
for(i in 1:(n_iterations - 1)) {
  ate <- c(ate, permuteHIV(df, random = TRUE))
}

# Histogram of placebo effects
hist(ate)

#calculating the p-value
ate = abs(ate)
obs_te = ate[1]

# `ecdf` gives us the empirical CDF of `ate`
empirical_cdf = ecdf(ate)

# percentile of obs_te
obs_percentile = empirical_cdf(obs_te)

# p-value 
1 - obs_percentile



