rm(list = ls())

# import relevant packages
library(metafor)


## THE FUNNEL PLOT

# generate a random normal predictor
slope <- -0.25
intercept <- 0
predictor <- rnorm(n=100, mean = 10, sd = 10)

# calculate response using these parameters
response <- slope * predictor + intercept + rnorm(n=100, mean = 10, sd = 40)

# plot the data
plot(predictor, response)

# Q: Run a simple lm with the data you generated. How does the slope estimate compare to the slope you simulated?
model <- lm(response ~ predictor)
summary(model)
# the slope is quite close to the one that was simulated (-0.0299)

# create somewhere to store the data
store <- matrix(nrow=200, ncol=4)

# simulate 200 different datasets
for(x in 1:200){
  
  # select sample sizes at random from a log normal distribution (small sample sizes common, large sample sizes rare)
  # n is always >3
  sample_size <- ceiling(exp(rnorm(1,4.5,1.5)))*3
  
  # predictor and response as before, but n is given by sample_size
  predictor <- rnorm(n = sample_size, mean = 10, sd = 10)
  response <- slope * predictor + intercept + rnorm(n = sample_size, 10, 40)
  
  # use the same model as before
  model <- lm(response ~ predictor)
  
  # extract model outputs and store them in the matrix
  store[x,] <- c(sample_size, summary(model)$coefficients[2,1:2], summary(model)$coefficients[2,4])
  # 
  
}

# 
store <- as.data.frame(store)
names(store) <- c("n", "slope", "standard.error", "p.value")

# visualise the data from these 200 simulated datasets
par(mfrow=c(1,2))
plot(store$slope, store$n, xlab = "Slope", ylab = "Sample Size")
plot(store$slope, (1/store$standard.error), xlab = "Slope", ylab = "Precision (1/se)")
par(mfrow=c(1,1))

# colour the significant slope estimates (>0.05)
# and indicate the slope we used to simulate with a dotted line
significant_slopes <- which(store$p.value<0.05)
par(mfrow=c(1,2))
plot(store$slope, store$n, xlab = "Slope", ylab = "Sample Size")
points(store$slope[significant_slopes], store$n[significant_slopes], col = "deeppink")
abline(v=slope, lty=2, col = "grey")
plot(store$slope, (1/store$standard.error), xlab = "Slope", ylab = "Precision (1/se)")
points(store$slope[significant_slopes], (1/store$standard.error[significant_slopes]), col = "deeppink")
abline(v=slope, lty=2, col = "grey")
par(mfrow=c(1,1))


# Q1: What happens to the effect size estimates as sample size and precision increases?

#     As slope increases, both the sample size and precision increase from zero,
#     reaching a peak at the simulated slope. After this point they both decrease as
#     slope continues to increase.


# Q2: When precision is low, what can you say about the estimates of effect size
#     that tend to be significant?

#     The effect sizes that are significant are either from those studies where the
#     sample size/precision is either extreme (strong pattern shown), or where the
#     slope is negative in smaller studies... because these show the same direction
#     as simulated.

# Q3: If there is a tendency to only publish results when the effect is significant
#     and/or sample size is large, which points would be missing from the plot?

#     The black ones (lower middle section of the plot), where sample size is small
#     and effect size is small.


## BASIC META ANALYSIS