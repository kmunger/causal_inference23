library(DeclareDesign)
library(tidyverse)

install.packages("ri2")


library(ri2)

###generate data
table_2_2 <- data.frame(Z = c(1, 0, 0, 0, 0, 0, 1),
                        Y = c(15, 15, 20, 20, 10, 15, 30))


# Declare randomization procedure
declaration <- declare_ra(N = 7, m = 2)

# Conduct Randomization Inference
ri2_out <- conduct_ri(
  formula = Y ~ Z,
  declaration = declaration,
  sharp_hypothesis = 0,
  data = table_2_2
)

summary(ri2_out)

plot(ri2_out)

#########block randomized

dat <- data.frame(
  Y = c(8, 6, 2, 0, 3, 1, 1, 1, 2, 2, 0, 1, 0, 2, 2, 4, 1, 1),
  Z = c(1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0),
  block = c(rep(1, 4), rep(2, 6), rep(3, 8))
)

# unites in blocks 1 and 3 have a 1/2 probability of treatment
# but units in block 2 have a 2/3 probability of treatment
with(dat, table(block, Z))


block_m <- with(dat, tapply(Z, block, sum) / 2)

declaration <- 
  with(dat,{
    declare_ra(
      blocks = block,
      block_m = block_m)
  })

declaration


ri2_out <- conduct_ri(
  Y ~ Z,
  sharp_hypothesis = 0,
  declaration = declaration,
  data = dat
)
summary(ri2_out)


plot(ri2_out)


######interactions

N <- 100
# two-arm trial, treat 50 of 100
declaration <- declare_ra(N = N)
X <- rnorm(N)
Z <- conduct_ra(declaration)
Y <- .9 + .2 * Z + .1 * X + -.5 * Z * X + rnorm(N)
dat <- data.frame(X, Y, Z)

# Observed ATE
ate_hat <- coef(lm(Y ~ Z, data = dat))[2]
ate_hat


ri2_out <-
  conduct_ri(
    model_1 = Y ~ Z + X, # restricted model
    model_2 = Y ~ Z + X + Z*X, # unrestricted model
    declaration = declaration,
    sharp_hypothesis = ate_hat,
    data = dat
  )

plot(ri2_out)

summary(ri2_out)

#########


################

#Simulate a new randomization inference 

#sample size 100

#half treated at random

##Coviates generated random normal

##outcomes a function of treatment, covariates and random normal 
## error term (no constant, no interaction term)

## set the sharp null hypothesis to the true value of 
## the treatment effect

## do you reject the null?

## 

