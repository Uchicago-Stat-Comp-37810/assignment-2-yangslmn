trueA <- 5
trueB <- 0
trueSd <- 10
sampleSize <- 31

# create independent x-values 
x <- (-(sampleSize-1)/2):((sampleSize-1)/2)
# create dependent values according to ax + b + N(0,sd)... similar to linear regression model
y <-  trueA * x + trueB + rnorm(n=sampleSize,mean=0,sd=trueSd) #vector y

plot(x,y, main="Test Data")

likelihood <- function(param){ # param is a vector with 3 entries
  a = param[1]
  b = param[2]
  sd = param[3]
  
  pred = a*x + b
  singlelikelihoods = dnorm(y, mean = pred, sd = sd, log = T) # normal distribution of each value y
  sumll = sum(singlelikelihoods) # because they are log value, add them. 
  return(sumll)   # return the liklehood
}

# Example: plot the likelihood profile of the slope a
slopevalues <- function(x){return(likelihood(c(x, trueB, trueSd)))}
slopelikelihoods <- lapply(seq(3, 7, by=.05), slopevalues ) # apply the slopevalues function to each list.
plot (seq(3, 7, by=.05), slopelikelihoods , type="l", xlab = "values of slope parameter a", ylab = "Log likelihood")


# Prior distribution
prior <- function(param){
  a = param[1]
  b = param[2]
  sd = param[3]
  aprior = dunif(a, min=0, max=10, log = T) #assume that prior distribution of a is uniform.
  bprior = dnorm(b, sd = 5, log = T) # assume normal distribution
  sdprior = dunif(sd, min=0, max=30, log = T)  # assume unifrom.
  return(aprior+bprior+sdprior)
}

posterior <- function(param){
  return (likelihood(param) + prior(param)) #because it is log, add the results. 
}

######## Metropolis algorithm ################

proposalfunction <- function(param){ # random values from normal distribution
  return(rnorm(3,mean = param, sd= c(0.1,0.5,0.3)))
}

run_metropolis_MCMC <- function(startvalue, iterations){
  chain = array(dim = c(iterations+1,3)) # make the chain vector as matrix with indicated dim. 
  chain[1,] = startvalue
  for (i in 1:iterations){
    proposal = proposalfunction(chain[i,]) # from the 1st row to the ith row, apply proposal function.
    
    probab = exp(posterior(proposal) - posterior(chain[i,])) # because it is log, this formula is same with the ratio. 
    if (runif(1) < probab){ #random value in uniform [0,1]
      chain[i+1,] = proposal  # accept when the ratio is bigger than p.
    }else{
      chain[i+1,] = chain[i,]  # reject when the ratio is not bigger than p
    }
  }
  return(chain)
}

startvalue = c(4,0,10) # set the start point 
chain = run_metropolis_MCMC(startvalue, 10000)

burnIn = 5000 # no of trial
acceptance = 1-mean(duplicated(chain[-(1:burnIn),]))  # proportion of acceptance. mean of no.logic vector.

### Summary: #######################

par(mfrow = c(2,3)) # divide the window by 2*3
hist(chain[-(1:burnIn),1],nclass=30,  main="Posterior of a", xlab="True value = red line" )
abline(v = mean(chain[-(1:burnIn),1]))
abline(v = trueA, col="red" )
hist(chain[-(1:burnIn),2],nclass=30, main="Posterior of b", xlab="True value = red line")
abline(v = mean(chain[-(1:burnIn),2]))
abline(v = trueB, col="red" )
hist(chain[-(1:burnIn),3],nclass=30, main="Posterior of sd", xlab="True value = red line")
abline(v = mean(chain[-(1:burnIn),3]) )
abline(v = trueSd, col="red" )
plot(chain[-(1:burnIn),1], type = "l", xlab="True value = red line" , main = "Chain values of a" )
abline(h = trueA, col="red" )
plot(chain[-(1:burnIn),2], type = "l", xlab="True value = red line" , main = "Chain values of b" )
abline(h = trueB, col="red" )
plot(chain[-(1:burnIn),3], type = "l", xlab="True value = red line" , main = "Chain values of sd" )
abline(h = trueSd, col="red" )

# for comparison:
summary(lm(y~x))