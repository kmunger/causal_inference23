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

#### correlated potential outcomes


N <- 1000
x <- runif(N, 0, 1)
y0 <- x + .25*rnorm(N)
y1 <- x*2 + y0+ .25*rnorm(N)
index <- 1:N
potcov <- data.frame(index, y1,y0,x)

plot(x, y0, col="blue", pch=19, 
     cex=.25, ylim=range(c(y1,y0)),
     main="Potential outcomes")
points(x, y1, col="red", pch=19, cex=.25)



### strong covariance between covariate and unit-level potential outcomes

N <- 1000
x <- runif(N, 0, 2)
y0 <- .5*x - .25*(x-mean(x))^2 +.25*rnorm(N)
y1 <- 2*y0 + .25 + (x-mean(x))^2 + .25*rnorm(N)
index <- 1:N
potcov <- data.frame(index, y1,y0,x)

plot(x, y0, col="blue", pch=19, 
     cex=.25, ylim=range(c(y1,y0)),
     main="Potential outcomes")
points(x, y1, col="red", pch=19, cex=.25)



plot(x, y1-y0,  pch=19, 
     cex=.25, 
     main="Potential outcomes")


plot((x-mean(x))^2, y1-y0,  pch=19, 
     cex=.25, 
     main="Potential outcomes")

######

#########BOOTSTRAP example for covaraite adjustment




n.vec <- c(20,40,80,160,320)

nsim <- 2000

# vectors to store results
sate.hat.noadj <- sate.hat.simpadj <- sate.hat.intadj <- sate <- matrix(NA, ncol=length(n.vec), nrow=nsim)

for(j in 1:length(n.vec)){
  n <- n.vec[j]
  
  for(i in 1:nsim){
    
    # Simple random sample without replacement
    potcov.s <- potcov[potcov$index %in% sample(potcov$index, n),]
    
    sate[i,j] <- mean(potcov.s$y1 - potcov.s$y0)
    
    # Complete random assignment of treatment
    n1 <- floor(.33*n)  # you can play around with treatment allocation.
    # this is set to an imbalanced design.
    potcov.s$D <- 0
    potcov.s$D[potcov.s$index %in% sample(potcov.s$index, n1)] <- 1
    
    potcov.s$Y <- with(potcov.s, D*y1 + (1-D)*y0)
    
    # No adjustment (difference in means)
    ols.1 <- lm(Y~D, data=potcov.s)
    
    # Simple covariate adjustment
    ols.2 <- lm(Y~D+x, data=potcov.s)

    sate.hat.noadj[i,j] <- coef(ols.1)["D"]
    sate.hat.simpadj[i,j] <- coef(ols.2)["D"]
  }
}

se.sate.hat.noadj <- apply(sate.hat.noadj, 2, sd)
bias.sate.hat.noadj <- apply(sate.hat.noadj-sate, 2, mean)
std.bias.sate.hat.noadj <- bias.sate.hat.noadj/se.sate.hat.noadj

se.sate.hat.simpadj <- apply(sate.hat.simpadj, 2, sd)
bias.sate.hat.simpadj <- apply(sate.hat.simpadj-sate, 2, mean)
std.bias.sate.hat.simpadj <- bias.sate.hat.simpadj/se.sate.hat.simpadj

res.tab <- cbind(c("SE","Bias","Bias/SE","SE","Bias","Bias/SE"),
                 round(1000*rbind(   se.sate.hat.noadj,
                                     bias.sate.hat.noadj,
                                     std.bias.sate.hat.noadj,
                                     se.sate.hat.simpadj,
                                     bias.sate.hat.simpadj,
                                     std.bias.sate.hat.simpadj),0)/1000)
res.tab.out <- cbind(c("No adjustment","","",
                       "Simple adjustment","",""), res.tab)
colnames(res.tab.out) <- c("Estimator","Statistic",paste("N=",n.vec,sep=""))


res.tab.out

######




########################################


##########################################Replication of Munger (2017)

####conceptually, what is the motivation for controlling for log.followers? 
#### is there some risk of this being a ``bad control" ? 


#### The two datasets are based on different assumptions about how to treat 
### study attrition --- subjects whose accounts were deleted/suspended/unmeasured

### "standard" drops those subjects and proceeds as normal
### "conservative" 

### Explain the potential bias from using the standard assumption



###using the code from inclass_215.R, perform CEM on this data, matching
## on the anonymity, pre-treatment outcome measure, and log.followers.

### You'll have to restrict the data to just treatment 3 and the control (0)

## estimate a standard ols model with these cem weights. 

