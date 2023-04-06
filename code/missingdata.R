#### unbiased 


N <- 1000
x <- runif(N, 0, 1)
y0 <- x + .25*rnorm(N)
y1 <- x*2 + .25*rnorm(N)
index <- 1:N
potcov <- data.frame(index, y1,y0,x)

plot(x, y0, col="blue", pch=19, 
     cex=.25, ylim=range(c(y1,y0)),
     main="Potential outcomes")
points(x, y1, col="red", pch=19, cex=.25)



####

#### 


N <- 1000
##Covariates
X <- runif(N, 0, 1)

#Treatment
Z <- rbinom(n=1000,size=1,prob=0.5)

#Outcome
Y <- X + Z

#Responsiveness (1 == Responded)
R <- round(X)

#full sample
df <- data.frame( X, Z, Y, R)


##observed sample
df_obs<-filter(df, R == 1)

####QUESTIONS

##1. Is the data Missing Completely at Random (MCAR)?

##2. What is the true treatment effect in the model? Run a regression on the full dataset that recovers this.

##3. Now run the regression on the observed dataset. Is the result the same?

##4. Calculate the  lower Manski bound based on the observed data, imputing the missing potential outcomes with the sharp null hypothesis.

##5. In this case, does the "monotonicity" assumption hold? [See slide 19]



