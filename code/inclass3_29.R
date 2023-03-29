


# Lalonde

#1. We will first perform analysis on the experimental dataset `https://raw.github.com/Mixtape-Sessions/Causal-Inference-2/master/Labs/Lalonde/lalonde_exp_panel.dta`

library(tidyverse)
library(fixest)
library(DRDID) # devtools::install_github("pedrohcgs/DRDID")
library(haven)
# 1. Experimental data
df_exp <- haven::read_dta("https://raw.github.com/Mixtape-Sessions/Causal-Inference-2/master/Lab/Lalonde/lalonde_exp_panel.dta")


#a. Under random assignment, the simple difference-in-means identifies the ATE, and since the original NSW was a randomized experiment, we can do this.  Calculate the simple difference-in-means on the experimental dataset to estimate the "treatment effect" two separate ways: (1) manually calculate averages for both treatment (`ever_treated=1`) and control (`ever_treated=0`) and use them to estimate the returns to the program, and (2) estimate the effect with an OLS specification. In both cases, use only the year `78` and `re` variable for real earnings. 


# ---- Difference-in-means - Averages
with(df_exp, {
  y11 = mean(re[year == 78 & ever_treated == 1])
  y01 = mean(re[year == 78 & ever_treated == 0])
  dim = y11 - y01
  dim
})
# ---- Difference-in-means - OLS
feols(
  re ~ i(treat),
  data = df_exp |> filter(year == 78), vcov = "hc1"
)

#b. Estimate the effect of the treatment, `ever_treated`, on real earnings, `re`, in a difference-in-differences estimator using years `78` for post period and `75` as the pre-period (ignoring for now year `74`). As with 1a, do this in the following two ways: (1) manually calculate the four means you need for the DiD equation and then estimate using the DiD equation, and (2) estimate the ATT using the OLS specification for the DiD equation with robust standard errors. Reminder to only use `78` and `75` (i.e., do not include `74` in OLS analysis). 

# ---- Difference-in-Differences - Averages



# ---- Difference-in-Differences - OLS



#c. Check the pre-trends for 1974 relative to 1975 two ways: (1) manually calculate the DiD equation on 1974 relative to 1975 and (2) estimate the dynamic OLS specification with an interaction of `ever_treated` with `74`, an interaction of `ever_treated` with `78`.  Compare your answers for 2c to what you found in 2a and 2b. 


# ---- Event study and pre-trends using manually calculated averages
with(df_exp, {
  y00 = mean(re[year == 75 & ever_treated == 0])
  y01 = mean(re[year == 74 & ever_treated == 0])
  y10 = mean(re[year == 75 & ever_treated == 1])
  y11 = mean(re[year == 74 & ever_treated == 1])
  did = (y11 - y10) - (y01 - y00)
  did
})
# ---- Event study and pre-trends using OLS 
df_exp$pre = df_exp$ever_treated * (df_exp$year == 74)
df_exp$post = df_exp$ever_treated * (df_exp$year == 78)
feols(
  re ~ i(post) + i(pre) | id + year, 
  data = df_exp, 
  vcov = "hc1"
)

#2. Now, we turn to the non-experimental dataset `https://raw.github.com/Mixtape-Sessions/Causal-Inference-2/master/Labs/Lalonde/lalonde_nonexp_panel.dta`. 


# 2. CPS data
df_nonexp <- haven::read_dta("https://raw.github.com/Mixtape-Sessions/Causal-Inference-2/master/Lab/Lalonde/lalonde_nonexp_panel.dta")
```

#a. Repeat 1a (simple difference-in-means for `78` only), 1b (DiD using manual calculations and OLS specification for `78` and `75` only) and 1c (event study calculations manually and dynamic OLS specification for `78`, `75` and `74`)


# ---- Difference-in-means - Averages


with(df_nonexp, {
  mean(re[year == 78 & ever_treated == 1]) -
    mean(re[year == 78 & ever_treated == 0])
})

# ---- Difference-in-means - OLS
feols(
  re ~ i(treat),
  data = df_exp |> filter(year == 78), vcov = "hc1"
)


# ---- Difference-in-Differences - Averages
with(df_nonexp, {
  y00 = mean(re[year == 75 & ever_treated == 0])
  y01 = mean(re[year == 78 & ever_treated == 0])
  y10 = mean(re[year == 75 & ever_treated == 1])
  y11 = mean(re[year == 78 & ever_treated == 1])
  did = (y11 - y10) - (y01 - y00)
  did
})
# ---- Difference-in-Differences - OLS
feols(
  re ~ i(treat) | id + year, 
  data = df_nonexp |> filter(year %in% c(75, 78)),
  vcov = "hc1"
)


# ---- Event study and pre-trends using manually calculated averages
with(df_nonexp, {
  y00 = mean(re[year == 75 & ever_treated == 0])
  y01 = mean(re[year == 74 & ever_treated == 0])
  y10 = mean(re[year == 75 & ever_treated == 1])
  y11 = mean(re[year == 74 & ever_treated == 1])
  did = (y11 - y10) - (y01 - y00)
  did
})
# ---- Event study and pre-trends using OLS 
df_nonexp$pre = df_nonexp$ever_treated * (df_nonexp$year == 74)
df_nonexp$post = df_nonexp$ever_treated * (df_nonexp$year == 78)
feols(
  re ~ i(post) + i(pre) | id + year, 
  data = df_nonexp, 
  vcov = "hc1"
)


#b. Repeat 1b and 1c (OLS specifications) controlling linearly for `age, agesq, agecube, educ, educsq, marr, nodegree, black, hisp` with robust standard errors.


# ---- Difference-in-differeces - OLS with covariates 
# age, agesq, agecube, educ, educsq, marr, nodegree, black, hisp
feols(re ~ i(post) + age + agesq + agecube + educ + educsq +
        marr + nodegree + black + hisp | ever_treated + year, 
      data = df_nonexp,
      vcov = "hc1"
)



#c. Use the `DRDID` command to estimate a doubly-robust difference-in-differences with covariates `age + agesq + agecube + educ + educsq + marr + nodegree + black + hisp`, `id` panel unit identifier, `year` as the panel time identifier, and reporting the outcome regression analysis [(Heckman, Ichimura and Todd 1997)](http://jenni.uchicago.edu/papers/Heckman_Ichimura-Todd_REStud_v64-4_1997.pdf), inverse probability weight estimator [(Abadie 2005)](https://academic.oup.com/restud/article-abstract/72/1/1/1581053?redirectedFrom=fulltext), doubly robust [(Sant'anna and Zhao 2020)](https://www.sciencedirect.com/science/article/abs/pii/S0304407620301901).  Compare these results with 1a, 1b, 2a and 2b. 



# ---- Double-robust DID
DRDID::drdid(
  yname = "re", tname = "year", idname = "id", dname = "ever_treated", 
  xformla = ~ age + agesq + agecube + educ + educsq +
    marr + nodegree + black + hisp + re74 + u74,
  data = df_nonexp |> filter(year == 75 | year == 78)
)





#####


####################




