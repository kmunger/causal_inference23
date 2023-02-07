rm(list=ls())
# change wd as needed:
setwd("C:/Users/Kevin/Documents/GitHub/causal_inference23/code/a_s2016")

library(foreign)
library(maptools)

# Jensen example

# Files needed:
# 5603 folder with world shapefile and auxiliary files
# jensen-rep.dta
# mapnames_filled.csv

world <-  readShapePoly("world_countries_boundary_file_world_2002.shp")
jensen.cc <- read.dta("jensen-rep.dta") 

X.vars <- c(	"var5",
				"market",
				"lgdppc",
				"gdpgrowt",
				"tradeofg",
				"overallb",
				"generalg",
				"country",
				"d2",
				"d3")
X.vars.f <- paste(X.vars,collapse="+")
fit.y <- lm(as.formula(paste("Fvar5~regime+", X.vars.f, sep="")), data=jensen.cc)
fit.d <- lm(as.formula(paste("regime~", X.vars.f, sep="")), data=jensen.cc)

summary(fit.y)
summary(fit.d)

d.tilde <- as.numeric(residuals(fit.d))
w <- d.tilde^2

w1 <- tapply(w, jensen.cc$country, mean)
mapnames <-  read.csv("mapnames_filled.csv")
mapnames$weight <- 0
mapnames$weight[match(rownames(w1), mapnames$jensen)] <- as.numeric(w1)

attributes(world)$data$weight <- 0
attributes(world)$data$weight[match(mapnames$mapname,attributes(world)$data$NAME)] <- mapnames$weight

attributes(world)$data$incl <- 0
attributes(world)$data$incl[match(mapnames$mapname,attributes(world)$data$NAME)] <- as.numeric(!is.na(mapnames$jensen))





# Sample

pdf(file="jensen-nominal-map.pdf", height=5, width=8)
plot(	world, 
		col=gray(1-.75*attributes(world)$data$incl),
		border="gray", lwd=.25)
dev.off()


# Effective sample

pdf(file="jensen-effective-map.pdf", height=5, width=8)
plot(	world, 
		col=gray(1-abs(attributes(world)$data$weight)/max(abs(attributes(world)$data$weight))),
		border="gray", lwd=.25)
dev.off()


########

##in-class


#### What are the top 10 individual observations in terms of weight?

#### WHat is the minimum number of individual observations
####you have to delete in order to eliminate the significant result?



