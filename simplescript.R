source('C:/Users/yangs/Desktop/2017 autunm/statistical computing/assignment-2-yangslmn/source script.R')

trueA <- 5
trueB <- 0
trueSd <- 10
sampleSize <- 31

# create independent x-values 
x <- (-(sampleSize-1)/2):((sampleSize-1)/2)
# create dependent values according to ax + b + N(0,sd)... similar to linear regression model
y <-  trueA * x + trueB + rnorm(n=sampleSize,mean=0,sd=trueSd) #vector y

plot(x,y, main="Test Data")


# Example: plot the likelihood profile of the slope a

slopelikelihoods <- lapply(seq(3, 7, by=.05), slopevalues ) # apply the slopevalues function to each list.
plot (seq(3, 7, by=.05), slopelikelihoods , type="l", xlab = "values of slope parameter a", ylab = "Log likelihood")





startvalue = c(4,0,10) # set the start point 
chain = run_metropolis_MCMC(startvalue, 10000)

burnIn = 5000 # no of trial
acceptance = 1-mean(duplicated(chain[-(1:burnIn),]))  # proportion of acceptance. mean of no.logic vector.


graph_function(chain, burnIn)




# for comparison:
summary(lm(y~x))
