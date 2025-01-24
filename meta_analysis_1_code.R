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
model1 <- lm(response ~ predictor)
summary(model1)
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
  model1 <- lm(response ~ predictor)
  
  # extract model outputs and store them in the matrix
  store[x,] <- c(sample_size, summary(model1)$coefficients[2,1:2], summary(model1)$coefficients[2,4])
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

# to find out a mean effect size, we can use a simple linear model
# 1 means we fit only intercept
model2 <- lm(slope ~ 1, data = store)
summary(model2)

# Q: What is wrong with the analysis above?
#    It does not take into account sampling variance, or the fact that some slopes
#    are estimated much more accurately. 

# check the main r function of metafor
?rma
meta <- rma(yi = slope, sei = standard.error, data = store)
meta
# gives us an estimate of -0.2381 - much closer to our true slope

# can also use in-built metafor functions to generate a nice funnel plot...
funnel(meta, shade = "grey", back = "white", lwd = 2, col = "deeppink")

# ... and forest plot
forest(meta, cex.lab = 0.8, cex.axis = 0.8, addfit = TRUE, shade = "zebra")


## A META-ANALYSIS WITH MODERATORS AND RANDOM TERMS

# this time we will generate datasets where the slope estimates (effect sizes)
# vary as a function of another variable.

# let's imagine the slopes we are generating correspond to the effect of temperature
# on phenology

# latitude predicts slope - i.e. species from further north advance timings more
# in response to temperature
latitude <- runif(100,0,90)
# we randomly sample a latitude from 0,90 degree north
slope <- -0 + latitude * -0.1 + rnorm(100,0,3)
plot(latitude, slope)

# we can then add this to the code we used before
# we will also add an extra step - a random effect - so that slopes vary among 20 species
# slopes 1-10 for species 1, slopes 11-20 for species 2

# create somewhere to store data
# this time we also want to save the latitude and species that the slope estimate comes from
# we will also save a unique ID for each observation to include a residual random effect
store2 <- matrix(nrow = 200, ncol = 7)

# we can then generate 20 species random effects
species <- rep(1:20, each = 10)
species_effect <- rep(rnorm(20,0,2), each = 10)

for (x in 1:200){
  
  latitude <- runif(1,0,90)
  
  slope <- 0 + species_effect[x] + latitude * -0.1 + rnorm(1,0,3)
  
  # then we select sample sizes at random from a log normal distribution , so that
  # small sample sizes are common and large sample sizes are rare
  sample_size <- ceiling(exp(rnorm(1,4.5,1.5)))
  
  # we don't want to run analyses on datasets that are too small
  if(sample_size>3){
    predictor <- rnorm(n = sample_size, mean = 10, sd = 10)
    response <- intercept + predictor * slope + rnorm(n = sample_size, 0, 40)
    
    # we can then use the same linear model as before
    model <- lm(response~predictor)
    
    # we can then extract the model outputs we want and then store them in our matrix
    store2[x,] <- c(sample_size, summary(model)$coefficients[2,1:2],
                    summary(model)$coefficients[2,4], latitude, species[x], x)

  }}

store2 <- as.data.frame(store2)
names(store2) <- c("n", "slope", "standard.error", "p.value", "latitude", "species", "ID")

# now we can try to generate a funnel plot, and run a meta-analysis
plot(store2$slope, (1/store2$standard.error), xlab = "Slope", ylab = "Precision (1/se)")

meta2 <- rma(yi = slope, sei = standard.error, data = store2)
summary(meta2)

# Q: Why doesn't the slope estimate funnel in much this time?
#    There is heterogeneity in slopes (as seen in the I^2 value and the heterogeneity test)

# we can add the model formula to include latitude as a covariate to the rma function
meta3 <- rma(yi = slope, sei = standard.error, mods = ~latitude, data = store2)
summary(meta3)
# even after controlling for the effect of latitude there is still some heterogeneity

# to add a random term to our meta-analysis we need to use the rma.mv function

# we include the slope variance as the square of the standard error
store2$se2 <- store2$standard.error^2
# the function won't run with NAs so we have to remove those rows
store3 <- store2[-which(is.na(store2$slope)==TRUE),]

# we can also include an observation-level random effect
meta4 <- rma.mv(yi = slope, V = se2, mods = ~latitude, random = ~1|species/ID, data = store3)
meta4
# we can see that variance estimated among species is similar to the variance we simulated
# the residual variance is similar to what we simulated
# our meta-analysis has done a good job of recovering



