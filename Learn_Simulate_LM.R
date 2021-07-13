## Learn to simulate LM
# sources to check out: 
# https://benwhalley.github.io/just-enough-r/power-analysis.html
# Vignette about paramtest package: https://cran.r-project.org/web/packages/paramtest/vignettes/Simulating-Power.html 

# http://disjointedthinking.jeffhughes.ca/2017/09/power-simulations-r/:

# create one variable that is continuous and normally distributed and another that is a manipulation with two conditions
n = 100 # sample size
x1 = rnorm(n, mean = 0, sd = 1) # predictor 1
x2 = sample(0:1, n, replace = T) # predictor 2

# next create a DV that depends on the two predictors. This is were we reverse engineer a regression, instead of using the predictors to find the regression equation that relates x to y, we create a regression equation and use that to set the values of y. 
# Here b0:b3 set the individual coeffecients (effect sizes), first the intercept, two main effects and the interaction. y is a linear combination of our variables multiplied by effect sizes for each (intercept, two effects, interaction) plus the error term (should come from empirical data). 
b0 = 0
b1 = .3
b2 = .2
b3 = .3
y = b0 + (b1* x1) + (b2 * x2) + (b3 * x1 * x2) + rnorm(n, mean = 0, sd = 1)

# now run our model based on simulated data:
LM = lm(y ~ x1 * x2)
summary(LM)

# Next pull p values and other data from model and save
str(summary(LM)) # this tells us the summary output is a list. 

output = summary(LM)$coefficients
coefs = output[,1]
ps = output[,4]
Rsq = summary(LM)$r.squared

results = c(coefs, ps, Rsq)
names(results) = c('b0_coef', 'b1_coef', 'b2_coef', 'b3_coef',
                   'b0_p', 'b1_p', 'b2_p', 'b3_p', 'rsq')

# Pack into a function: 
regression_sim = function(simNum, n, b0, b1, b2, b3, x1mean=0, x1_sd=1, err_mean=0, err_sd=1){
  x1 = rnorm(n, mean = x1_sd)
  x2 = sample(0:1, n, replace = TRUE)
  
  y = b0 + (b1* x1) + (b2 * x2) + (b3 * x1 * x2) + rnorm(n, mean = err_mean, sd = err_sd)
  
  model = lm(y~ x1 * x2)
  summary(model)
  #Pull info
  output = summary(model)$coefficients
  coefs = output[,1]
  ps = output[,4]
  Rsq = summary(model)$r.squared
  
  results = c(coefs, ps, Rsq)
  names(results) = c('b0_coef', 'b1_coef', 'b2_coef', 'b3_coef',
                     'b0_p', 'b1_p', 'b2_p', 'b3_p', 'rsq')
  return(results)
}

regression_sim(1, 300, 0, .3, .2, .3)

# now try with repeatable simulations
library(plyr)
num_sims = 1000 # number of simulations
sims = ldply(1:num_sims, regression_sim, n=100, b0=0, b1=.3, b2=.2, b3=.3)

# ldply repeats the function for the number supplied (num_sims) using the parms supplied
# calculate the proportion of pvalues that are significant for each main effect:

powerx1 = sum(sims$b1_p < 0.05)/nrow(sims)
powerx2 = sum(sims$b2_p < 0.05)/nrow(sims)
powerx3 = sum(sims$b3_p < 0.05)/nrow(sims)

## Varying Parameters: now use a range of parameters. Namely sample size and effect size
sample_sizes = c(50, 100, 200, 300, 500)
results = NULL
num_sims = 1000
for (val in sample_sizes) {
  sims = ldply(1:num_sims, regression_sim, n=val, b0=0, b1=.3, b2=.2, b3=.3)
  sims$n = val
  results = rbind(results, sims)
  
}

# plot results:
library(ggplot2)
library(tidyr)
library(tidyverse)

power_ests = results %>%
  group_by(n)%>%
  summarize(power = sum(b3_p < .05)/n())

ggplot(power_ests, aes(x=n, y=power))+
  geom_point()+
  geom_line()+
  ylim(c(0,1))+
  geom_hline(yintercept =  .8)+
  theme_minimal()


#####################
# Notes from  http://ms.mcmaster.ca/~bolker/emdbook/chap5A.pdf

# Simple example of linear model with normal distribution of error
# yi = a + bx + ei, ei ~ N(0, o^2) <-- Normally distributed error term with mean of zero. "~" means is distributed according to...

x = 1:20
a = 2
b = 1

# calculate deterministic part of the model
y_det = a + b * x

# pick 20 random normal deviates with the mean equal to the deterministic equation and O = 2
y = rnorm(20, mean = y_det, sd = 2)

# regression line and scatter plot
LM1 = lm(y~x)
plot(x, y)
abline(LM1)

# 2nd example with hyperbolic functions with negative binomial error


# Example from  https://stats.stackexchange.com/questions/1866/how-to-simulate-a-custom-power-analysis-of-an-lm-model-using-r:
a = 2  #desired slope
b = 1  #estimated intercept
sd = 20  #estimated variability defined by standard deviation
nsim = 400  #400 simulations
pval = numeric(nsim)  #placeholder for the second for loop output
Nvec = seq(25, 100, by = 1)  #vector for the range of sample sizes to be tested
power.N = numeric(length(Nvec))   #create placeholder for first for loop output
for (j in 1:length(Nvec)) {
  N = Nvec[j]  
  x = seq(1, 20, length = Nvec[j])  #x value length needs to match sample size (Nvec) length
  for (i in 1:nsim) {   #for this value of N, create random error 400 times
    y_det = a + b * x
    y = rnorm(N, mean = y_det, sd = sd)
    m = lm(y ~ x)
    pval[i] = coef(summary(m))["x", "Pr(>|t|)"]  #all the p values for 400 sims
  }  #cycle through all N values
  power.N[j] = sum(pval < 0.05)/nsim  #the proportion of correct p-values (i.e the power)
}
power.N
plot(Nvec, power.N)  #need about 90 - 100 samples for 80% power