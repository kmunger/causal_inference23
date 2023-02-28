library(tidyverse)
library(haven)
library(ggplot2)
library(gridExtra)
library(cowplot)

# Part 1

## Load data
pop <- read_dta("/Users/XX/Downloads/pop.dta")

## Create histograms and save them
x_hist <- pop %>% ggplot(aes(x = X)) + geom_histogram()
y_hist <- pop %>% ggplot(aes(x = Y)) + geom_histogram()

ggsave("hw/hw1_hists.pdf", width = 10, height = 5, units = "in",
       arrangeGrob(x_hist, y_hist, ncol = 2))

# Part 2

# Set replicates and sample sizes
n <- 1000
size <- c(10, 50, 250, 500)

plot.list <- list()

# Iterate across the sample sizes
for (s in size) {
  means <- c()
  # Compute the mean of n samples with s elements of X
  for (i in 1:n) {
    samp <- sample(pop$X, s, replace = FALSE)
    m <- mean(samp)
    means <- c(means, m)
  }
  # Construct histogram of means of the n samples and append to plot list
  plot.list <- append(plot.list, list(data.frame(
    i = c(1:n),
    mean = means
  ) %>% ggplot(aes(x = mean)) + geom_histogram(aes(y=..density..)) +
    xlab(paste0("Means with ", s, " samples")) + 
    stat_function(fun = dnorm, n = 101, args = list(mean = mean(pop$X), sd = sd(pop$X)/sqrt(s)))))
}

# Save plot

ggsave("hw/hw1_Xconv.pdf", arrangeGrob(grobs = plot.list, ncol = 2, nrow = 2),
       width = 7, height = 7, units = "in")

plot.list <- list()

# Iterate across the sample sizes
for (s in size) {
  means <- c()
  # Compute the mean of n samples with s elements of Y
  for (i in 1:n) {
    samp <- sample(pop$Y, s, replace = FALSE)
    m <- mean(samp)
    means <- c(means, m)
  }
  # Construct histogram of means of the n samples and append to plot list
  plot.list <- append(plot.list, list(data.frame(
    i = c(1:n),
    mean = means
  ) %>% ggplot(aes(x = mean)) + geom_histogram(aes(y=..density..)) +
    xlab(paste0("Means with ", s, " samples")) +
    stat_function(fun = dnorm, n = 101, args = list(mean = mean(pop$Y), sd = sd(pop$Y)/sqrt(s)))))
}

ggsave("hw/hw1_Yconv.pdf", (arrangeGrob(grobs = plot.list, ncol = 2, nrow = 2)),
       width = 7, height = 7, units = "in")

# Part 3

# Set replicates and sample sizes
n <- 1000
size <- c(10, 50, 250, 500)

plot.list <- list()

for (s in size) {
  diff_means <- c()
  # Compute the difference of means of X and Y with n bivariate samples, each with s observations
  for (i in 1:n) {
    samp <- sample(pop$index, s*2, replace = FALSE)
    treat <- sample(samp, s, replace = FALSE)
    control <- samp[samp != treat]
    diff_means <- c(diff_means, mean(pop$X[treat]) - mean(pop$Y[control]))
  }
  # Construct histogram of difference of means of the n samples and append to plot list
  plot.list <- append(plot.list, list(data.frame(
    i = c(1:n),
    diff = diff_means
  ) %>% ggplot(aes(x = diff)) + geom_histogram(aes(y=..density..)) +
    xlab(paste0("Diff. of means with ", s, " samples")) +
    stat_function(fun = dnorm, n = 100, args = list(mean = mean(pop$X) - mean(pop$Y), 
                                                    sd = ((sd(pop$X - pop$Y))/sqrt(s))))))
}

# Save plot
ggsave("hw/hw1_diffmeans.pdf", (arrangeGrob(grobs = plot.list, ncol = 2, nrow = 2)),
       width = 7, height = 7, units = "in")
